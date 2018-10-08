module CodeGeneration.SqlMeta // translation of SqlGeneration.ttinclude

//module SqlGeneration =
open System
open System.Collections.Generic
open System.Text
open System.Linq
open System.IO
open System.Data
open System.Data.SqlClient

open BReusable
open BReusable.Reflection
open BReusable.StringHelpers
open Macros.SqlMacros
open BCore.CodeGeneration.SqlWrapCore
open BCore.CodeGeneration.SqlWrapCore.ColumnTyping
open BCore.CodeGeneration.DteWrapCore
open BCore.ADO.AdoHelper

open CodeGeneration.SqlScriptGeneration
open CodeGeneration.DataModelToF
open Macros



let getParms conn procName =
    use cmd = new SqlCommand(procName, conn)
    cmd.CommandType <- CommandType.StoredProcedure
    if conn.State = ConnectionState.Closed then
        conn.Open()
    SqlCommandBuilder.DeriveParameters cmd
    cmd.Parameters

let makeStrFkey50 name fkey = {ColumnInput.Name=name; DefaultValue=null; ColumnType=ColumnType.StringColumn 50; IsUnique=NotUnique; Nullability = NotNull; FKey = Some (FKeyIdentifier fkey); Comments = List.empty}
let makeStrRefFkey50 name fkey = {ColumnInput.Name=name; DefaultValue=null; ColumnType=ColumnType.StringColumn 50; IsUnique=NotUnique; Nullability = NotNull; FKey = Some (FKeyWithReference fkey); Comments = List.empty}
let makeIntFkey name fkey = {Name=name; DefaultValue=null; ColumnType=ColumnType.IntColumn; IsUnique=NotUnique; Nullability = NotNull; FKey=Some fkey; Comments = List.empty}
let makeNullable50 name =
    {Name=name; ColumnType = StringColumn 50; Nullability = AllowNull; DefaultValue=null; IsUnique=NotUnique; FKey = None; Comments = List.empty}

type SqlParameterCollection with
    member x.ToSeq() = seq { for p in x -> p }
    static member ToSeq (x: SqlParameterCollection) = x.ToSeq()

let getSqlMeta appendLine cgsm tables =
        use cn = new SqlConnection(cgsm.CString)
        cn.Open()
        sprintf "Connected to %s,%s" cn.DataSource cn.Database
        |> tee appendLine
        |> printfn "%s"
        appendLine String.Empty
        let fGenerated =
            function
            |Unhappy(ti,ex) -> Unhappy(ti,ex)
            |Happy (ti, typeName, tm:TableDataMeta) ->
                let result = Happy {TI=ti; TypeName= typeName; PrimaryKeys=tm.PrimaryKeys; Identities=tm.Identities; Columns=SqlTableColumnChoice.SqlTableColumnMeta tm.ColumnDescriptions}
                result
        let getTableData = SqlMacros.getTableData cn
        let tableData =
            tables
            |> Seq.map (getTableGenerationData appendLine cgsm.Singularize getTableData)
            |> Seq.map fGenerated
            |> List.ofSeq
        tableData

let getSqlSprocs cn =
    let sprocData =
        getReaderArray cn {CommandText= "select * from information_schema.routines where routine_type = 'PROCEDURE'"; OptCommandType = Some CommandType.Text; OptParameters= None}
            (fun r ->
                {   SpecificCatalog= getRecordOptT r "specific_catalog" |> Option.getOrDefault null
                    SpecificSchema= getRecordOptT r "specific_schema" |> Option.getOrDefault null
                    SpecificName= getRecordT r "specific_name"
                    Created= getRecordT r "created"
                    LastAltered= getRecordT r "last_altered"
                    IsDeterministic= getRecordOptT r "is_deterministic" |> Option.getOrDefault null
                    SqlDataAccess= getRecordOptT r "sql_data_access" |> Option.getOrDefault null
                    SchemaLevelRoutine= getRecordOptT r "sql_data_access" |> function | Some(StringEqualsI "yes") -> Some true | Some (StringEqualsI "no") -> Some false | _ -> None
                }
            )
    sprocData

let getSqlSprocMeta cn sprocName =
    Connections.runWithConnection cn (fun cn ->
            getParms (cn :?> SqlConnection) sprocName
    )

let mapSprocParams cn (appendLine:int -> string -> unit) (sp:SqlSprocMeta) =
    let ps =
        cn
        |> getSqlSprocMeta
        // type is in CodeGenerationCore : SqlSprocMeta
        <| sprintf "%s.%s.%s" sp.SpecificCatalog sp.SpecificSchema sp.SpecificName
        |> SqlParameterCollection.ToSeq
        |> List.ofSeq
    let mapParamName (s: string) =
        // get rid of @ sign
        match s.[1..] with
        // get rid of keywords
        | "end" -> "``end``"
        | x -> x

    // would be nice if we mapped measures into this
    let mapParam =
        // name, if the type is inherently nullable
        function
        | DbType.AnsiString
        | DbType.AnsiStringFixedLength
        | DbType.String
        | DbType.StringFixedLength
        | DbType.Xml
            -> "string"
        | DbType.Binary -> "byte[]"
        | DbType.Boolean -> "bool"
        | DbType.Byte -> "byte"
        | DbType.Decimal
        | DbType.Currency -> "decimal"
        | DbType.Date
        | DbType.DateTime
        | DbType.DateTime2
            // this one might need to be TimeSpan
        | DbType.DateTimeOffset
            -> "DateTime"
        | DbType.Double -> "float"
        | DbType.Int16
        | DbType.Int32 -> "int"
        | DbType.Int64 -> "int64"

        | x -> failwithf "unaccounted for type found %A" x

    match ps with
    | [] -> ()
    | ps ->
        let filtered =
            ps
            |> Seq.filter(fun p ->
                p.Direction <> ParameterDirection.Output &&
                p.Direction <> ParameterDirection.ReturnValue &&
                p.ParameterName |> String.equalsI "@RETURN_VALUE" |> not)
            |> List.ofSeq
        let memberList =
            filtered
            |> Seq.map (fun p ->
                let typeWording = (p.DbType |> mapParam) + if p.IsNullable then " option" else String.Empty
                sprintf "%s: %s" (p.ParameterName |> mapParamName) typeWording)
            |> List.ofSeq
        match memberList with
        | [] -> ()
        | x ->
            filtered
            |> List.map (fun p -> p.ParameterName, p.Direction)
            |> printfn "member list for %s has %i params after filter, which are %A" sp.SpecificName filtered.Length
            x
            |> delimit ";"
            |> sprintf "type %sInput = {%s}" (toPascalCase sp.SpecificName)
            |> appendLine 1
        ()
