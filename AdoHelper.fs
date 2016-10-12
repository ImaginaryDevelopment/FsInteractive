module Pm.Dal.AdoHelper

// desired features 
// "do we have a connectionstring or connection?" ignorance-capable coding
// "are we in a transaction?" ignorance-capable coding
// massive reduction in using blocks necessary
// remove any reliance/requirement to refer directly to System.Data in layers that need not

// conventions: 
//  pascal cased functions if they are specially crafted to be easier for C# to consume
//  F# method params are in an order that makes more sense for partial application
//  C# targeted method params are in an order that makes more sense for C# consumption

// possible future features:
// automatic sql exception catching to add ex.data, then rethrow
// adding nice extension methods to help IDbConnection act as fully featured as SqlConnection (there are some features like cmd.Params.AddWithValue missing in IDbCommand)

open System
open System.Collections.Generic
open System.Data
open System.Data.SqlClient

open Pm.Schema.BReusable

let ( |? ) (x:'t) (f) = if not <| isNull x then x else f()
let ( |?? ) (v:'t option) x = match v with |Some x -> x  |None -> x

let getRecordOpt (r:#System.Data.IDataRecord) (name:string) =
    try
            match r.[name] with
            | x when x = box System.DBNull.Value -> None
            | x -> Some x
        with ex ->
            ex.Data.Add("Read failure", name)
            reraise()

let getRecordOptT<'t> (r:System.Data.IDataRecord) (name:string) =
    match getRecordOpt r name with
    | Some v -> Some(v :?> 't)
    | None -> None

let getRecordT<'t> (r:System.Data.IDataRecord) (name:string) =
    try
        getRecordOpt r name
        |> function
            | Some d -> d :?> 't
            | None -> failwithf "Expected a value found none for column %s" name
    with ex -> 
        ex.Data.Add("ColumnName",name)
        reraise()

// http://stackoverflow.com/questions/2983087/f-working-with-datareader/
let getRecordBytesData i (r : IDataRecord) = 
    let len = r.GetBytes(i, int64 0, null, 0, 0)
    // Create a buffer to hold the bytes, and then
    // read the bytes from the DataTableReader.
    let buffer : byte array = Array.zeroCreate (int32 len)
    r.GetBytes(1, int64 0, buffer, 0, int32 len) |> ignore
    buffer

let readInt r name = getRecordOptT<int> r name 
let readStringOrNull r name = getRecordOptT<String> r name |> function | Some s -> s | None -> null
//
//type System.Data.IDataRecord with
//    member r.getOpt(name:string) = getRecordOpt r name
//    member r.getOptT<'t> (name:string) : 't option = r.getOpt name |> Option.bind(fun o -> o :?> 't |> Some)
//    member r.getOptTf<'t> (name:string) (f: obj -> 't) = r.getOpt name |> Option.bind (f >> Some)
//
//    member r.ReadValueMap (name:string) (convertValueFOpt:(obj -> 't) option) = 
//        match convertValueFOpt with
//        | Some f -> r.ReadOr name f
//        | None -> r.ReadOr name (Some (fun o -> o :?> 't))

// this doesn't work in C# land, it's an extension class inside a module
[<System.Runtime.CompilerServices.Extension>]
module DataRecordExtensions =
    [<System.Runtime.CompilerServices.Extension>]
    // uses default instead of opt
    let ReadOrDefault(r:System.Data.IDataRecord) name (fOpt:Func<_,_>) = 
        getRecordOpt r name 
        |> Option.bind (fun x -> (if not <| isNull fOpt then Some (fOpt.Invoke x) else x :?> _ ))
        |> function
            | Some x -> x
            | None -> Unchecked.defaultof<_>

    [<System.Runtime.CompilerServices.Extension>]
    let ReadNullable(r:System.Data.IDataRecord) name =
        ReadOrDefault r name (Func<_,_>(fun o -> Nullable (o :?> 't)))

let nullToDbNull x = if not <| isNull x then x :> obj else upcast System.DBNull.Value
let nullToOption (x:_) = if not <| isNull x then Some x else None // is this duplicated, better, or worse than what's in Nullable.fs?

let dbNullToOption (x:obj) : obj option= 
    if System.DBNull.Value.Equals x then 
        None
    else Some x

let dbNullToObjNull (x:obj) : obj = 
    match dbNullToOption x with
    |Some x-> x
    |None -> null

type System.DBNull with
    static member OfObj (x:obj) = if not <| isNull x then x else upcast System.DBNull.Value
    static member ToObj (x:obj) = dbNullToObjNull x
    static member OfOption (x:obj option) = match x with Some x -> System.DBNull.OfObj x | None -> upcast System.DBNull.Value
    static member ToOption (x:obj) = dbNullToOption x

module Connections = 
    // The heart of the code-ignorance possibilities
    [<NoComparison>][<NoEquality>]
    type Connector = 
        private // suggested by http://stackoverflow.com/q/24212865/57883
        | CString of string
        | ICon of IDbConnection
        with
            static member CreateICon x = Connector.ICon(x)
            static member CreateCString s = 
                if System.String.IsNullOrEmpty(s) then
                    failwithf "Invalid connection string:%s" s
                Connector.CString(s)

    let (|CString|ICon|) x = 
        match x with
        |Connector.CString cs -> CString cs
        |Connector.ICon con -> ICon con

    let inline openConnection (conn: #IDbConnection) =
        if conn.State = ConnectionState.Closed then
            conn.Open()

    let validateDelegateIsNotPartial (_ : _ -> 't) = 
        let tType = typeof<'t>

        if tType = typeof<Delegate> || tType.IsSubclassOf typeof<Delegate> || tType.FullName.StartsWith "Microsoft.FSharp.Core.FSharpFunc" then
            invalidArg "f" (sprintf "Bad delegate passed %A" tType)

    /// Expectations:
    ///     Connector.ICon expects an open connection
    /// as long as you aren't returning 
    ///  an IEnumerable that depends on the connection staying open
    ///  a partial function
    let inline runWithConnection connector f = 
        validateDelegateIsNotPartial f

        match connector with
        | ICon con -> f con
        | CString cs -> 
            use conn = new SqlConnection(cs)
            openConnection conn
            f conn

    let runWithCn cn f = runWithConnection cn (Connector.ICon >> f)

    let inline getItems connector f = runWithConnection connector (f >> Array.ofSeq)

    /// get a sequence of items, which is automatically pulled into an array so that the disposal of the connection is safe
    let GetItems<'tResult> (runWithConnectionFunc:System.Func<IDbConnection, IEnumerable<'tResult>>) cn = getItems cn (runWithConnectionFunc.Invoke >> Array.ofSeq)
    let ActWithConnection connector (f:System.Action<_>) = runWithConnection connector f.Invoke
    let RunWithConnection connector (f:System.Func<_,_>) = runWithConnection connector f.Invoke
    let RunWithCn connector (f:System.Func<_,_>) = runWithConnection connector (Connector.ICon >> f.Invoke)
    let ActWithCn connector (f:System.Action<_>) = runWithConnection connector (Connector.ICon >> f.Invoke)

type Connector = Connections.Connector

module Transactions =
    [<NoComparison>]
    type Transaction =
    | ConnTran of IDbConnection
    | TranScope
    /// For situations where there is already an ambient connection, so don't create one
    /// assumes all provided code respects the ambient transaction type that was created
    | UseAmbient

    let runInTrans tranType (f: (unit -> unit) -> _ ) = 
        match tranType with 
        |UseAmbient -> f (fun () -> ())
        |ConnTran con ->
            use tran = con.BeginTransaction ()
            f tran.Commit
        |TranScope ->
            use tran = new System.Transactions.TransactionScope ()
            f tran.Complete

/// all SqlClient specific code should live in this module
module SqlConnections =
    type SqlConnector = Connections.Connector

    /// Expecations:
    ///     Connector.ICon expects an open connection
    /// as long as you aren't returning an IEnumerable that depends on the connection staying open, this method is safe for anything
    let inline runWithConnection (connector:SqlConnector) f = Connections.runWithConnection connector (fun con -> con :?> SqlConnection |> f)
    let inline getInTrans tranType connector f =  Transactions.runInTrans tranType (fun fCommit -> runWithConnection connector (f fCommit))

    let RunWithConnection connector (f:System.Action<_>)=  runWithConnection connector f.Invoke
    /// as long as you aren't returning an IEnumerable that depends on the connection staying open, this method is safe for anything
    let GetFromConnection connector (f:System.Func<_,_>)= runWithConnection connector f.Invoke
    /// get a sequence of items, which is automatically pulled into an array so that the disposal of the connection is safe
    let GetItems connector (f:System.Func<_,_ seq>) = runWithConnection connector (f.Invoke >> Array.ofSeq)
    let RunInConnectionInTrans tranType connector (f:System.Action<System.Action,_>) = getInTrans tranType connector (fun fComplete con -> f.Invoke(new System.Action(fComplete),con))
    let GetInConnectionInTrans tranType connector (f:System.Func<System.Action,_>) = 
        getInTrans tranType connector (fun fComplete con ->
            f.Invoke (new System.Action(fComplete)) con 
        )

    let ExecuteInTransaction tranType connector (transAction:System.Action<System.Action<_>, _ >) = getInTrans tranType connector (fun fComplete con -> transAction.Invoke(new System.Action<_>(fComplete),con ))

module Commands = 
    /// replace ' with '' and any other sanitize/cleaning of a string
    open Pm.Schema.BReusable.StringPatterns
    let encodeStringParam s =  
        match s with 
        |NullString -> s
        |Empty  -> s
        |WhiteSpace -> s
        |ValueString -> s.Replace("'", "''")

    [<NoComparison;NoEquality>]
    type Input = {CommandText:string; OptCommandType:CommandType option; OptParameters:IDictionary<string,obj> option}

    [<NoComparison;NoEquality>]
    type InputC = {CommandTextC:string; CommandTypeOpt: System.Nullable<CommandType>; ParametersOpt:IDictionary<string,obj>; }
        with
            member x.ToSqlCommandInput = 
                {       CommandText = x.CommandTextC
                        OptCommandType = if x.CommandTypeOpt.HasValue then Some x.CommandTypeOpt.Value else None
                        OptParameters = if isNull x.ParametersOpt then None else Some x.ParametersOpt 
                        //OptExtraPrep = if isNull x.ExtraPrepOpt then None else Some x.ExtraPrepOpt.Invoke 
                        }

    // works with null just as well as `None`
    let loadParameters (cmd: #IDbCommand) (parameters: IDictionary<string,obj> option) =
        let inline loadParam (KeyValue(k,v)) = 
            let param = cmd.CreateParameter ()
            param.Value <- System.DBNull.OfObj v
            param.ParameterName <- k
            cmd.Parameters.Add param |> ignore
        match parameters with
        | None -> ()
        | Some x when isNull x -> ()
        | Some items -> items |> Seq.iter loadParam

    let inline prepareCommand sci (cmd:'a when 'a :> IDbCommand) =
        cmd.CommandText <- sci.CommandText
        match sci.OptCommandType with
        |Some ct -> 
            cmd.CommandType <- ct
        | None -> ()
        loadParameters cmd sci.OptParameters

    // sci is solely for diagnostic output on failure
    let inline runWithSqlDiag sci f = 
        printfn "sql: %s params: %A" sci.CommandText sci.OptParameters
        let sw = System.Diagnostics.Stopwatch.StartNew ()
        try
            let result = f ()
            sw.Stop ()
            if sw.ElapsedMilliseconds > 700L then
                    System.Diagnostics.Debug.WriteLine(sprintf "runWithSqlDiag took %A" sw.ElapsedMilliseconds)
            result
        with ex ->
            ex.Data.Add("CommandText", sci.CommandText) 
            ex.Data.Add("CommandType", sci.OptCommandType)
            ex.Data.Add("Parameters", sprintf "%A" sci.OptParameters)
            reraise ()

    let inline useCmd (con: #IDbConnection) sci f = 
        use cmd = con.CreateCommand()
        prepareCommand sci cmd
        runWithSqlDiag sci (fun () -> f cmd)

    let inline executeNonQuery (cmd: #IDbCommand) = cmd.ExecuteNonQuery ()
    let inline executeScalar (cmd: #IDbCommand) = cmd.ExecuteScalar ()
    let inline executeReader (cmd: #IDbCommand) = cmd.ExecuteReader ()
    let inline executeTable (fDataAdapter: _ -> #System.Data.Common.DbDataAdapter) (cmd: #IDbCommand) = 
        let dt = new DataTable ()
        use da = fDataAdapter cmd
        let result = da.Fill dt
        result |> ignore
        dt

    let inline executeDataset (fDataAdapter: _ -> #System.Data.Common.DbDataAdapter) (cmd: #IDbCommand) =
        let ds = new DataSet()
        use da = fDataAdapter cmd
        da.Fill ds |> ignore
        ds

    let inline executeDatasetName (fDataAdapter: _ -> #System.Data.Common.DbDataAdapter) tableName (cmd: #IDbCommand) =
        let ds = new DataSet()
        use da = fDataAdapter cmd
        da.Fill(ds,tableName) |> ignore
        ds

    let inline executeReaderArray f (cmd: #IDbCommand) = 
        use reader = cmd.ExecuteReader ()
        reader
        |> Seq.unfold(fun r -> if r.Read () then Some (r :> IDataRecord |> f,r) else None)
        |> List<_>
        |> System.Collections.ObjectModel.ReadOnlyCollection

    /// Works with non-nullable return values (an int column that allows nulls for instance, would fail in the case of null)
    let inline getOptScalar cmd = 
        let raw = executeScalar cmd 
        if isNull raw then
            None
        else
            raw
            |> System.DBNull.ToOption

    let inline getScalarT<'t> cmd =  
        let result = executeScalar cmd 
        let onBadResult (ex:#Exception) = 
            if System.Diagnostics.Debugger.IsAttached then
                System.Diagnostics.Debugger.Break()
            raise ex
        // if 't is an option type or nullable, then the null exceptions are inappropriate
        if box System.DBNull.Value = result then
            let ex = NullReferenceException("getScalarT result was dbNull") 
            onBadResult ex

        if isNull result then 
            let ex = NullReferenceException("getScalarT result was null") 
            onBadResult ex

        result :?> 't
    let inline getNonQueryFromCon con sci= useCmd con sci executeNonQuery

    let inline getScalarFromCon con sci= useCmd con sci executeScalar
    let inline getOptScalarFromCon con sci = useCmd con sci getOptScalar
    let inline getScalarIntFromCon con sci = useCmd con sci getScalarT<int>

    // a single runReader doesn't make sense unless reading a single row of data
    let inline getReaderArrayFromCon con sci f = useCmd con sci (executeReaderArray f) 

    // unless you cast the identity to int (within your sql statements) it will be a decimal (http://dba.stackexchange.com/questions/4696/why-is-select-identity-returning-a-decimal)
    /// select @@identity or SCOPE_IDENTITY() both return Numeric(38,0)
    /// see also http://dba.stackexchange.com/questions/4696/why-is-select-identity-returning-a-decimal
    let inline getScalarIdentityFromCon con sci = useCmd con sci (getScalarT<decimal> >> int)

    let UseCmd connector (scic:InputC) (f:System.Func<_,_>) = useCmd connector scic.ToSqlCommandInput f.Invoke
    let RunReaderArray cmd (f:System.Func<_,_>) = executeReaderArray f.Invoke cmd
    let ExecuteReaderArray con (scic:InputC) (f:System.Func<_,_>) = getReaderArrayFromCon con scic.ToSqlCommandInput f.Invoke

let inline private flip f x y = f y x

let inline private runComplete f cn (sci:Commands.Input) = Connections.runWithConnection cn (flip f sci)

let getNonQuery cn= runComplete Commands.getNonQueryFromCon cn
let getScalar cn= runComplete Commands.getScalarFromCon cn 
let getOptScalar cn = runComplete Commands.getOptScalarFromCon cn
let getOptScalarInt cn = runComplete Commands.getOptScalarFromCon cn >> Option.map (fun o -> o :?> int)
let getScalarInt cn= runComplete Commands.getScalarIntFromCon cn
let getScalarIdentity cn= runComplete Commands.getScalarIdentityFromCon cn
let getReaderArray cn sci f= Connections.runWithConnection cn (fun con -> Commands.getReaderArrayFromCon con sci f)

let inline private createScicFromParts cmdText cmdType parameters = 
    let scic : Commands.InputC = {Commands.InputC.CommandTextC = cmdText; CommandTypeOpt = cmdType; ParametersOpt = parameters}
    scic
let inline private createSciFromParts cmdText cmdType parameters = 
    let scic = createScicFromParts cmdText cmdType parameters
    scic.ToSqlCommandInput

let GetReaderArray cn (scic:Commands.InputC) (f:System.Func<_,_>) = getReaderArray cn scic.ToSqlCommandInput f.Invoke

let ExecuteScalar cmdText cmdType cn parameters =           createSciFromParts cmdText cmdType parameters       |> getScalar cn
let ExecuteScalarInt cmdText cmdType cn parameters =        createSciFromParts cmdText cmdType parameters       |> getScalarInt cn
let ExecuteNullableInt cmdText cmdType cn parameters =      createSciFromParts cmdText cmdType parameters       |> getOptScalarInt cn |> Option.toNullable
let ExecuteScalarIdentity cmdText cmdType cn parameters =   createSciFromParts cmdText cmdType parameters       |> getScalarIdentity cn
let ExecuteNonQuery cmdText cmdType cn parameters =         createSciFromParts cmdText cmdType parameters       |> getNonQuery cn

let ExecuteReaderArray cmdText cmdType cn parameters f =    createScicFromParts cmdText cmdType parameters      |> GetReaderArray cn <| f

/// If you are using Microsoft's Sql Server specifically and need that functionality, or just find it easier to work with fewer generic params
module SqlCommands = 
    let inline getSqlCommandInput (scic:Commands.InputC) = scic.ToSqlCommandInput
    let inline createAdapter (cmd:SqlCommand) = new SqlDataAdapter(cmd)
    let inline useSqlCmd (con:SqlConnection) sci f= 
        use cmd = con.CreateCommand()
        Commands.prepareCommand sci cmd
        Commands.runWithSqlDiag sci (fun () -> f cmd)

    let getDs cmd tableName = 
        let ds = new DataSet()
        use adapter = createAdapter(cmd)
        adapter.Fill( ds, tableName) |> ignore
        ds

    let inline private runComplete f (cn:Connections.Connector) (sci:Commands.Input) = runComplete f cn sci

    // begin ease of use (generic parameters getting to be unwiedly) helpers for C#
    let ExecuteNonQuery cn scic = getSqlCommandInput scic |> runComplete Commands.getNonQueryFromCon cn
    let ExecuteScalar cn scic = getSqlCommandInput scic |> runComplete Commands.getScalarFromCon cn
    let ExecuteScalarInt cn scic = getSqlCommandInput scic |> runComplete Commands.getScalarIntFromCon cn 
    let ExecuteScalarIdentity cn scic = getSqlCommandInput scic |> runComplete Commands.getScalarIdentityFromCon cn
    
    let ExecuteReaderArray scic (f:System.Func<_,_>) cn = getReaderArray cn (getSqlCommandInput scic) f.Invoke
    let ExecuteReaderArraySci commandText commandType cn parametersOpt f = ExecuteReaderArray {CommandTextC = commandText; CommandTypeOpt = System.Nullable commandType; ParametersOpt = parametersOpt} f cn
    let ExecuteTableCon scic sqlCon= useSqlCmd sqlCon (getSqlCommandInput scic) (Commands.executeTable createAdapter)

    let ExecuteDatasetCon scic sqlCon = useSqlCmd sqlCon (getSqlCommandInput scic) (Commands.executeDataset createAdapter)
    let ExecuteDatasetNameCon scic tableName sqlCon = useSqlCmd sqlCon (getSqlCommandInput scic) (Commands.executeDatasetName createAdapter tableName)
    let ExecuteTable scic sqlCn = SqlConnections.runWithConnection sqlCn (fun con -> ExecuteTableCon scic con)
    let ExecuteDataset scic sqlCn = SqlConnections.runWithConnection sqlCn (fun con -> ExecuteDatasetCon scic con)
    let ExecuteTableM cmdText cmdType cn parameters = createScicFromParts cmdText cmdType parameters |> (fun scic -> ExecuteTable scic cn)
    let ExecuteDatasetNameM cmdText cmdType parameters tableName sqlCn = 
        let scic = createScicFromParts cmdText cmdType parameters 
        SqlConnections.runWithConnection sqlCn (ExecuteDatasetNameCon scic tableName) 

module ConnectionTests = 
    let openCon cs =
        use con = new SqlConnection(cs)
        con.Open ()