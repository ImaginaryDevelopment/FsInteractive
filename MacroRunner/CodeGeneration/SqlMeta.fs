module CodeGeneration.SqlMeta // translation of SqlGeneration.ttinclude

//module SqlGeneration = 
open System
open System.Collections.Generic
open System.Text
open System.Linq
open BReusable
open BReusable.Reflection
open MacroRunner.MultipleOutputHelper
open System.IO

type FKeyInfo = {Schema:string; Table:string; Column:string}
type ColumnLength = |Max | Length of int
type DecimalInfo = {Precision:int; Scale:int}

type ColumnType = 
    |Decimal of DecimalInfo option
    |VarChar of ColumnLength
    |NVarChar of ColumnLength
    |Char of ColumnLength
    |NChar of ColumnLength
    |Other of Type

type Nullability = 
    | AllowNull
    | NotNull
type ColumnInfo = 
    { 
        Name:string; 
        Type:ColumnType
        AllowNull:Nullability
        Attributes: string list
        IsUnique: bool
        FKey:FKeyInfo option
        Comments: string list
        GenerateReferenceTable:bool
        ReferenceValuesWithComment: IDictionary<string,string>
    }
    with
        static member Zero ct = 
            {Name=null; Type = ct; AllowNull = NotNull;IsUnique=false; Attributes = List.empty; FKey = None; Comments = List.empty; GenerateReferenceTable = false; ReferenceValuesWithComment = null}

type TableIdentifier = {Schema:string; Table:string;}

type TableInfo = { Name:string; Schema:string; Columns: ColumnInfo list}

type ReferenceData = {Schema:string; Table:string; Column:string; ValuesWithComments: IDictionary<string,string>}

//void GenerateTable(Manager manager, EnvDTE.Project targetProject, string targetProjectFolder, TableInfo ti)
//type Targeting = TargetProject of EnvDTE.Project*targetProjectFolder:string

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

// SqlGeneration.ttinclude ~ 49
let generateTable (manager:IManager) (generationEnvironment:StringBuilder) targetProjectFolderOpt (tableInfo:TableInfo) =
//    printfn "Generating a table into %A %s" targetProjectFolderOpt tableInfo.Name
    let targetFilename = Path.Combine(defaultArg targetProjectFolderOpt String.Empty, "Schema Objects", "Schemas", tableInfo.Schema, "Tables", tableInfo.Name + ".table.sql")
    manager.StartNewFile(targetFilename)

    let formatFKey (table:string) column (fKey:FKeyInfo option) : string =
        match fKey with
        |None -> null
        | Some fKey -> 
            let fKeyColumn = if isNull fKey.Column then column else fKey.Column
            sprintf "CONSTRAINT [FK_%s_%s_%s_%s] FOREIGN KEY ([%s]) REFERENCES [%s].[%s] ([%s])" table column fKey.Table fKeyColumn column fKey.Schema fKey.Table fKeyColumn

    let appendLine text = generationEnvironment.AppendLine(text) |> ignore
    let appendLine' indentLevel text = delimit String.Empty (Enumerable.Repeat("    ",indentLevel)) + text |> appendLine
    appendLine "-- Generated file, DO NOT edit directly"
    appendLine (sprintf "CREATE TABLE [%s].[%s] (" tableInfo.Schema tableInfo.Name)
    // SqlGeneration.ttinclude ~ 165
    let mapTypeToSql ct =
        let mapLength = 
            function 
            | Max -> "MAX"
            | Length l -> string l

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

    let mutable i = 0
    let columnCount = tableInfo.Columns.Length
    let hasCombinationPK = tableInfo.Columns.Count (fun ci -> ci.Attributes |> Seq.contains "primary key") > 1

    tableInfo.Columns
    |> Seq.iter (fun ci -> 
        let fKey = formatFKey tableInfo.Name ci.Name ci.FKey

        let multipleComments = ci.Comments.Length > 1
        if multipleComments then
            appendLine String.Empty
            ci.Comments 
            |> Seq.map (fun c -> "-- " + c)
            |> delimit "\r\n"
            |> appendLine' 1 

        let comment = 
            if not <| isNull ci.ReferenceValuesWithComment && Seq.any ci.ReferenceValuesWithComment && (multipleComments || not <| Seq.any ci.Comments) then
                " -- " + (delimit "," ci.ReferenceValuesWithComment.Keys)
            elif ci.Comments.Length = 1 then 
                " -- " + (Seq.head ci.Comments)
            else String.Empty
        
        let formatAttributes (attributes: string seq) hasCombinationPK fKey allowNull = 
            let isPk = not <| isNull attributes && Seq.contains "primary key" attributes
            let unique = if not <| isNull attributes && Seq.contains "unique" attributes || ci.IsUnique then " CONSTRAINT UQ_" + tableInfo.Name + "_" + ci.Name + " UNIQUE" else String.Empty
            let needsStarter = allowNull || not isPk || hasCombinationPK
            let starter = (if allowNull then "null" elif needsStarter then "not null" else String.Empty) + (if needsStarter then " " else String.Empty)
            if isNull attributes then
                starter + (if not <| isNull fKey then " " + fKey else null)
            else
                let attribs = starter + (delimit " " (if hasCombinationPK && (not <| isNull attributes) then attributes |> Seq.except([| "primary key" |]) else attributes)) + unique
                if isNull fKey then
                    attribs
                else attribs + " " + fKey

        // TODO: finish translation
        sprintf "%-32s%-16s%s%s%s%s" 
            (sprintf "[%s]" ci.Name)
            (mapTypeToSql ci.Type) 
            (formatAttributes ci.Attributes hasCombinationPK fKey (match ci.AllowNull with |AllowNull -> true | NotNull -> false))
            (if i < columnCount - 1 || hasCombinationPK then "," else String.Empty) 
            (if multipleComments then Environment.NewLine else String.Empty)
            comment
        |> appendLine' 1
        i <- i + 1
    )
    if hasCombinationPK then
        let columns = 
            tableInfo.Columns.Where(fun ci -> ci.Attributes.Contains("primary key")).Select(fun ci -> ci.Name)
            |> delimit ","
        
        sprintf "CONSTRAINT PK_%s PRIMARY KEY (%s)" tableInfo.Name columns
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
            printfn "Starting table %s" rd.Table 
            let cteName = sprintf "CTE_%s" rd.Table
            appendLine "---------------------------------------------------"
            appendLine (sprintf "PRINT 'Synchronizing [%s.%s]';" rd.Schema rd.Table)
            appendLine (sprintf "WITH %s(%s) AS" cteName rd.Column)
            appendLine "("
            appendLine (sprintf "    SELECT [%s]" rd.Column)
            appendLine "    FROM (VALUES"
            let mutable i = 0
            let valueCount = rd.ValuesWithComments.Keys.Count
            for k in rd.ValuesWithComments.Keys do
                let comment = match rd.ValuesWithComments.[k] with
                                |null -> String.Empty
                                |k -> " -- " + k
                appendLine (sprintf "        ('%s')%s%s" (k |> replace "'" "''") (if i < valueCount - 1 then "," else ")" ) comment )
                i <- i + 1
            appendLine (sprintf "        AS SOURCE([%s])" rd.Column)
            appendLine ")"
            appendLine (sprintf "MERGE INTO [%s].[%s] AS TARGET" rd.Schema rd.Table)
            appendLine (sprintf  "USING %s" cteName)
            appendLine (sprintf "ON %s.[%s] = TARGET.[%s]" cteName rd.Column rd.Column )
            appendLine "WHEN NOT MATCHED BY TARGET THEN"
            appendLine (sprintf "    INSERT([%s])" rd.Column)
            appendLine (sprintf "    VALUES([%s]);" rd.Column)
            appendLine String.Empty
            appendLine (sprintf "PRINT 'Done Synchronizing [%s.%s]';" rd.Schema rd.Table)
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
        tables.Where( fun t -> t.Columns.Any( fun c -> not <| isNull c.ReferenceValuesWithComment && c.ReferenceValuesWithComment.Any())).ToArray()
    printfn "Generating for %i parent tables (%A)" toGen.Length (toGen |> Seq.map (fun t -> t.Name) |> List.ofSeq)
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
                    |> Seq.filter(fun c -> isNull c.ReferenceValuesWithComment |> not && c.ReferenceValuesWithComment |> Seq.any)
                    |> Seq.map (fun c -> 
                        let schema, table, columnName = 
                            match c.FKey with
                            | Some fKey -> 
                                fKey.Schema, fKey.Table, if isNull fKey.Column then c.Name else fKey.Column
                            | None -> failwithf "ReferenceValuesWithComment existed but no fkey"
                        { ReferenceData.Schema = schema; Table=table; Column = columnName; ValuesWithComments = c.ReferenceValuesWithComment}
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
let generateTablesAndReferenceTables(manager:IManager, generationEnvironment:StringBuilder, targeting, toGen: TableInfo seq ) =
    toGen
    |> Seq.iter(fun ti ->
        generateTable manager generationEnvironment targeting ti
        ti.Columns
        |> Seq.filter(fun ci-> ci.GenerateReferenceTable)
        |> Seq.iter(fun childCi -> 
            let pkeyColumn = {childCi with Attributes = ["primary key"]; AllowNull = NotNull; FKey=None}
            let fkey = childCi.FKey.Value
            let name = fkey.Table
            let table = {Schema = fkey.Schema; Name=name; Columns = [pkeyColumn]}
            generateTable manager generationEnvironment targeting table
        )
    )
let makeStrFkey50 name fkey = {Name=name; Type=VarChar (Length 50); IsUnique=false; Attributes = List.empty; AllowNull = NotNull; FKey = Some fkey; Comments = List.empty; GenerateReferenceTable=false; ReferenceValuesWithComment=null}
let makeIntFkey name fkey = {Name=name; Type=Other typeof<int>; IsUnique=false; Attributes = List.empty; AllowNull = NotNull; FKey=Some fkey; Comments = List.empty; GenerateReferenceTable=false; ReferenceValuesWithComment=null}
let makeNullable50 name = 
    {Name=name; Type = VarChar (Length 50); AllowNull = AllowNull; IsUnique=false; Attributes = List.empty; FKey = None; Comments = List.empty; GenerateReferenceTable = false; ReferenceValuesWithComment = null}
let makeNonFKeyColumn name columnType allowNull = 
    {Name=name; Type=columnType; AllowNull=allowNull; Comments = List.empty; GenerateReferenceTable = false; ReferenceValuesWithComment=null;FKey=None; IsUnique=false; Attributes = List.empty}

// end sql generation module
