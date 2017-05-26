module CodeGeneration.SqlMeta // translation of SqlGeneration.ttinclude

//module SqlGeneration =
open System
open System.Collections.Generic
open System.Text
open System.Linq
open System.IO
open System.Data.SqlClient
open System.Data
open BReusable
open BReusable.Reflection
open BReusable.StringHelpers
open MacroRunner.MultipleOutputHelper
open Macros.SqlMacros

type ReferenceData = {FKeyId:FKeyIdentifier; GenerateReferenceTable:bool; ValuesWithComment: IDictionary<string,string>}
type FKey =
    |FKeyIdentifier of FKeyIdentifier
    |FKeyWithReference of ReferenceData

type ColumnGenerationInfo<'T> =
    {
        Name:string;
        Type: 'T
        AllowNull:Nullability
        Attributes: string list
        IsUnique: bool
        DefaultValue: string
        CustomSqlType: string
        FKey:FKey option
        Comments: string list
    }
    with
        static member Zero ct =
            {Name=null; Type = ct; AllowNull = NotNull;IsUnique=false; Attributes = List.empty; DefaultValue=null; CustomSqlType=null; FKey = None; Comments = List.empty}

type ColumnGenerationInfo = ColumnGenerationInfo<ColumnType>
//type TableInfo = { Id:TableIdentifier; Columns: ColumnInfo list}
type TableGenerationInfo = {Id:TableIdentifier; Columns: ColumnGenerationInfo list}

let getParms conn procName =
    use cmd = new SqlCommand(procName, conn)
    cmd.CommandType <- CommandType.StoredProcedure
    if conn.State = ConnectionState.Closed then
        conn.Open()
    SqlCommandBuilder.DeriveParameters cmd
    cmd.Parameters

let getTableGenerationData appendLine fSingularizer cn (ti:TableIdentifier) =
    let typeName = fSingularizer ti.Name
    appendLine <| sprintf "Starting table %s as type %s" ti.Name typeName
    let (ti, pks,columns,identities) = getTableData cn ti
    ti, typeName, pks, columns, identities

let toColumnType (t:Type) length precision scale useMax =
    match t with
    | TypeOf (isType:bool) -> ColumnType.Other typeof<bool>
    | TypeOf (isType:decimal) ->
        match precision,scale with
        | NullableValue p, NullableValue s ->
            {Precision=p; Scale = s}
            |> Some
            |> ColumnType.Decimal
        | _ -> ColumnType.Decimal None
    |TypeOf (isType:string) ->
        match useMax,length with
        | true, _ -> Max
        | _, NullableValue length -> Length length
        | _ -> failwithf "mismatched for %A " (useMax,length)
        |> ColumnType.VarChar
    | _ -> Other t

let formatFKey (table:string) column (fKey:FKeyIdentifier) : string =
    let fKeyColumn = if isNull fKey.Column then column else fKey.Column
    sprintf "CONSTRAINT [FK_%s_%s_%s_%s] FOREIGN KEY ([%s]) REFERENCES [%s].[%s] ([%s])" table column fKey.Table.Name fKeyColumn column fKey.Table.Schema fKey.Table.Name fKeyColumn

let formatDefaultValue (table:string) column defaultValue : string = 
    sprintf "CONSTRAINT [DF_%s_%s] DEFAULT %s" table column defaultValue

let mapTypeToSql customType ct =
    let mapLength =
        function
        | Max -> "MAX"
        | Length l -> string l
    match customType with
    | ValueString _ -> customType
    | _ -> 
        match ct with
        | Decimal (Some(di)) -> sprintf "decimal(%i,%i)" di.Precision di.Scale
        | Decimal None -> "decimal"
        | VarChar cl -> sprintf "varchar(%s)" (mapLength cl)
        | NVarChar cl -> sprintf "nvarchar(%s)" (mapLength cl)
        | Char cl -> sprintf "char(%s)" (mapLength cl)
        | NChar cl -> sprintf "nchar(%s)" (mapLength cl)
        | Other t ->
            match t with
            | TypeOf(isType:int) -> "int"
            | TypeOf(isType:bool) -> "bit"
            | TypeOf(isType:DateTime) -> "datetime"
            | _ -> t.Name

let composeFKeyAndDefaultValue tableName columnName defaultValue fkeyOpt = 
    let fkeyText = 
        fkeyOpt
        |> Option.map (
            function
            |FKeyIdentifier fk -> fk
            | FKeyWithReference rd -> rd.FKeyId
            >> formatFKey tableName columnName
         )
         |> Option.getOrDefault null
    let defaultValueText = Option.ofObj defaultValue |> Option.map (formatDefaultValue tableName columnName) |> Option.getOrDefault null
    // apparently null string + null string is empty string
    match fkeyText, defaultValueText with
    | ValueString x, ValueString y -> sprintf "%s %s" x y
    | ValueString x, _ -> x
    | _, ValueString y -> y
    | _ -> null

let (|RefValueComments|_|) x =
        match x.FKey with
        | Some (FKeyWithReference rd) when not <| isNull rd.ValuesWithComment && rd.ValuesWithComment.Count > 0 ->
            Some rd.ValuesWithComment
        | _ -> None

let formatColumnComments doDiag appendLine appendLine' tableName ci =
    let multipleComments = ci.Comments.Length > 1
    if multipleComments then
        appendLine String.Empty
        ci.Comments
        |> Seq.map (fun c -> "-- " + c)
        |> delimit "\r\n"
        |> appendLine' 1
    let comment =
        match ci.Comments with
        | [comment] -> " -- " + comment |> Some
        // if it is empty, let it fall through for ref value comments checking
        | []
        | _ :: _ :: _ ->
            match ci with
            | RefValueComments rvc ->
                if doDiag then
                    printfn "found rvc for %s -> %s! %A" tableName ci.Name rvc
                " -- " + (delimit "," rvc.Keys) |> Some
            | _ -> None
        |> Option.getOrDefault null
    multipleComments,comment

let formatAttributes (attributes: string seq) hasCombinationPK fKeyText allowNull tableName (columnName, isUnique)=
    let allowNull = (match allowNull with | AllowNull -> true | NotNull -> false) 
    let isPk = not <| isNull attributes && Seq.contains "primary key" attributes
    let unique = if not <| isNull attributes && Seq.contains "unique" attributes || isUnique then " CONSTRAINT UQ_" + tableName + "_" + columnName + " UNIQUE" else String.Empty
    let needsStarter = allowNull || not isPk || hasCombinationPK
    let starter = (if allowNull then "null" elif needsStarter then "not null" else String.Empty) + (if needsStarter then " " else String.Empty)
    if isNull attributes then
        starter + (if not <| isNull fKeyText then " " + fKeyText else null)
    else
        let attribs = starter + (delimit " " (if hasCombinationPK then attributes |> Seq.except([| "primary key" |]) else attributes)) + unique
        match fKeyText with
        | ValueString _ -> attribs + " " + fKeyText
        | _ -> attribs |> trim

let generateColumn doDiag (tableId:TableIdentifier) appendLine appendLine' hasCombinationPK isLastColumn ci = 
    let fKeyTextOpt = composeFKeyAndDefaultValue tableId.Name ci.Name ci.DefaultValue ci.FKey
    let hasMultipleComments,comment = formatColumnComments doDiag appendLine appendLine' tableId.Name ci
    let attribs = 
        formatAttributes 
            ci.Attributes 
            hasCombinationPK 
            fKeyTextOpt 
            ci.AllowNull 
            tableId.Name
            (ci.Name, ci.IsUnique)

    let result =
        // %-32s is padRight 32chars ' '
        sprintf "%-32s%-16s%s%s%s%s"
            (sprintf "[%s]" ci.Name)
            (mapTypeToSql ci.CustomSqlType ci.Type)
            attribs
            //(if i < columnCount - 1 || hasCombinationPK then "," else String.Empty)
            (if isLastColumn || hasCombinationPK then "," else String.Empty)
            (if hasMultipleComments then Environment.NewLine else String.Empty)
            comment
    if doDiag then
        match ci.Name with
        | "PaymentItemStatusId" as columnName ->
            printfn "%s column diag: {commentCount:%i;result:%s}" columnName ci.Comments.Length result
        | _ -> ()
    result
    |> appendLine' 1
    ()

// SqlGeneration.ttinclude ~ 49
let generateTable doDiag (manager:IManager) (generationEnvironment:StringBuilder) targetProjectFolderOpt (tableInfo:TableGenerationInfo) =
//    printfn "Generating a table into %A %s" targetProjectFolderOpt tableInfo.Name
    let targetFilename = Path.Combine(defaultArg targetProjectFolderOpt String.Empty, "Schema Objects", "Schemas", tableInfo.Id.Schema, "Tables", tableInfo.Id.Name + ".table.sql")
    manager.StartNewFile(targetFilename)

    let appendLine text = generationEnvironment.AppendLine(text) |> ignore
    let appendLine' indentLevel text = delimit String.Empty (Enumerable.Repeat("    ",indentLevel)) + text |> appendLine
    appendLine "-- Generated file, DO NOT edit directly"
    appendLine (sprintf "CREATE TABLE [%s].[%s] (" tableInfo.Id.Schema tableInfo.Id.Name)
    // SqlGeneration.ttinclude ~ 165
    // custom type,  column type

    let mutable i = 0
    let columnCount = tableInfo.Columns.Length
    let hasCombinationPK = tableInfo.Columns.Count (fun ci -> ci.Attributes |> Seq.contains "primary key") > 1
    

    tableInfo.Columns
    |> Seq.iter (fun ci ->
        generateColumn doDiag tableInfo.Id appendLine appendLine' hasCombinationPK (i < columnCount - 1) ci
        i <- i + 1
    )
    if hasCombinationPK then
        let columns =
            tableInfo.Columns.Where(fun ci -> ci.Attributes.Contains("primary key")).Select(fun ci -> ci.Name)
            |> delimit ","

        sprintf "CONSTRAINT PK_%s PRIMARY KEY (%s)" tableInfo.Id.Name columns
        |> appendLine' 1
    appendLine ");"
    manager.EndBlock()

type TitledReferenceData =
    | TitledReferenceData of title:string* (ReferenceData list)
let generateReferenceInsert appendLine =
    function
    | TitledReferenceData(title, referenceData) ->
        appendLine "-- Generated file, DO NOT edit directly"
        appendLine "SET ANSI_NULLS ON"
        appendLine "GO"
        appendLine "SET QUOTED_IDENTIFIER ON"
        appendLine "GO"
        appendLine (sprintf "PRINT 'Starting %s Synchronization'" title)
        appendLine "GO"
        referenceData
        |> Seq.iter (fun rd ->
            printfn "Starting table %s" rd.FKeyId.Table.Name
            let cteName = sprintf "CTE_%s" rd.FKeyId.Table.Name
            appendLine "---------------------------------------------------"
            appendLine (sprintf "PRINT 'Synchronizing [%s.%s]';" rd.FKeyId.Table.Schema rd.FKeyId.Table.Name)
            appendLine (sprintf "WITH %s(%s) AS" cteName rd.FKeyId.Column)
            appendLine "("
            appendLine (sprintf "    SELECT [%s]" rd.FKeyId.Column)
            appendLine "    FROM (VALUES"
            let mutable i = 0
            let valueCount = rd.ValuesWithComment.Keys.Count
            for k in rd.ValuesWithComment.Keys do
                let comment = match rd.ValuesWithComment.[k] with
                                |null -> String.Empty
                                |k -> " -- " + k
                appendLine (sprintf "        ('%s')%s%s" (k |> replace "'" "''") (if i < valueCount - 1 then "," else ")" ) comment )
                i <- i + 1
            appendLine (sprintf "        AS SOURCE([%s])" rd.FKeyId.Column)
            appendLine ")"
            appendLine (sprintf "MERGE INTO [%s].[%s] AS TARGET" rd.FKeyId.Table.Schema rd.FKeyId.Table.Name)
            appendLine (sprintf  "USING %s" cteName)
            appendLine (sprintf "ON %s.[%s] = TARGET.[%s]" cteName rd.FKeyId.Column rd.FKeyId.Column )
            appendLine "WHEN NOT MATCHED BY TARGET THEN"
            appendLine (sprintf "    INSERT([%s])" rd.FKeyId.Column)
            appendLine (sprintf "    VALUES([%s]);" rd.FKeyId.Column)
            appendLine String.Empty
            appendLine (sprintf "PRINT 'Done Synchronizing [%s.%s]';" rd.FKeyId.Table.Schema rd.FKeyId.Table.Name)
            appendLine "GO"
            appendLine String.Empty
        )

type InsertsGenerationConfig =
    {
        InsertTitling: string
        // @"Scripts\Post-Deployment\TableInserts\Accounting1.5\AccountingInserts.sql";
        // or @"Scripts\Post-Deployment\TableInserts\Accounting1.5\AccountingGeneratorInserts.sql";
        TargetInsertRelativePath: string
        AdditionalReferenceData: ReferenceData seq
    }


let generateInserts appendLine (manager:IManager) targetProjectFolder (tables:#seq<_>) igc =
    printfn "Starting inserts, template text length: %i" (manager.GetTextSize())
    // generate reference data
    let toGen =
        tables
        |> Seq.filter(fun t ->
            t.Columns
            |> Seq.exists(fun c ->
                match c.FKey with
                | Some(FKeyIdentifier fk) -> false
                | Some(FKeyWithReference rd) -> rd.ValuesWithComment.Any()
                | _ -> false
            )
        )
        |> List.ofSeq

    printfn "Generating for %i parent tables (%A)" toGen.Length (toGen |> Seq.map (fun t -> t.Id.Name) |> List.ofSeq)
    if not <| Seq.any toGen && not <| Seq.any igc.AdditionalReferenceData then
        ()
    else
        let targetFilename = Path.Combine(targetProjectFolder, igc.TargetInsertRelativePath)
        manager.StartNewFile targetFilename
        let refData =
            let addlRefData = igc.AdditionalReferenceData |> List.ofSeq
            toGen
            |> Seq.map (fun tbl ->
                let refData =
                    tbl.Columns
                    |> Seq.choose(fun c -> match c.FKey with | Some(FKeyWithReference rd) -> Some(c.Name, rd) | _ -> None)
                    |> Seq.filter (fun (cName,rd) -> rd.ValuesWithComment |> isNull |> not && rd.ValuesWithComment |> Seq.any )
                    |> Seq.map (fun (cName,rd) ->
                        if rd.FKeyId.Column |> isNull then
                            {rd with FKeyId = {rd.FKeyId with Column = cName}}
                        else rd
                    )
                    |> List.ofSeq
                refData
            )
            |> Seq.collect id
            |> List.ofSeq
            |> flip (@) addlRefData

        generateReferenceInsert appendLine (TitledReferenceData (igc.InsertTitling, refData))
        manager.EndBlock()
        printfn "Done with inserts, template text length: %i" (manager.GetTextSize())

    //feature desired: auto-name primary keys
    // adjustment desired: put all reference values comment (when on the reference table, above the column instead of beside it
let generateTablesAndReferenceTables(manager:IManager, generationEnvironment:StringBuilder, targeting, toGen: TableGenerationInfo seq ) =
    toGen
    |> Seq.iter(fun ti ->
        generateTable false manager generationEnvironment targeting ti
        ti.Columns
        |> Seq.choose(fun ci->
            match ci.FKey with
            | Some (FKeyWithReference rd) when rd.GenerateReferenceTable ->
                Some (rd, {ci with FKey = None})
            | _ -> None
        )
        // iterate reference table columns
        |> Seq.iter(fun (fKey,childCi) ->
            try
                let refPKComment =
                    if childCi.Comments.Length > 0 then
                        childCi.Comments
                    elif fKey.ValuesWithComment |> isNull |> not && fKey.ValuesWithComment.Count > 0 then [fKey.ValuesWithComment.Keys |> delimit"," ]
                    else []
                let pkeyColumn = {childCi with Attributes = ["primary key"]; AllowNull = NotNull; FKey=None; Comments= refPKComment}
                let tId = fKey.FKeyId.Table
                let table = {TableGenerationInfo.Id={Schema = tId.Schema; Name=tId.Name}; Columns = [pkeyColumn]}
                printfn "Generating ReferenceTable from %A" table
                generateTable true manager generationEnvironment targeting table
            with _ ->
                printfn "Failing:"
                try printfn " fKey: %A" fKey with _ -> ()
                try printfn " childCi: %A" childCi with _ -> ()
                reraise()
        )
    )
let makeStrFkey50 name fkey = {ColumnGenerationInfo.Name=name; DefaultValue=null; CustomSqlType=null; Type=VarChar (Length 50); IsUnique=false; Attributes = List.empty; AllowNull = NotNull; FKey = Some (FKeyIdentifier fkey); Comments = List.empty}
let makeStrRefFkey50 name fkey = {ColumnGenerationInfo.Name=name; DefaultValue=null; CustomSqlType=null; Type=VarChar (Length 50); IsUnique=false; Attributes = List.empty; AllowNull = NotNull; FKey = Some (FKeyWithReference fkey); Comments = List.empty}
let makeIntFkey name fkey = {ColumnGenerationInfo.Name=name; DefaultValue=null; CustomSqlType=null; Type=Other typeof<int>; IsUnique=false; Attributes = List.empty; AllowNull = NotNull; FKey=Some fkey; Comments = List.empty}
let makeNullable50 name =
    {Name=name; Type = VarChar (Length 50); AllowNull = AllowNull; DefaultValue=null; CustomSqlType=null; IsUnique=false; Attributes = List.empty; FKey = None; Comments = List.empty}

let makeNonFKeyColumn name columnType allowNull =
    {Name=name; Type=columnType; AllowNull=allowNull; Comments = List.empty; DefaultValue=null; CustomSqlType=null; FKey=None; IsUnique=false; Attributes = List.empty}

// end sql generation module
