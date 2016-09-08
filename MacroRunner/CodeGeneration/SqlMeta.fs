﻿module CodeGeneration.SqlMeta // translation of SqlGeneration.ttinclude

//module SqlGeneration = 
open System
open System.Collections.Generic
open System.Text
open System.Linq
open BReusable
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

type ColumnInfo = 
    { 
        Name:string; 
        Type:ColumnType
        AllowNull:bool
        Attributes: string list
        FKey:FKeyInfo option
        Comments: string list
        GenerateReferenceTable:bool
        ReferenceValuesWithComment: IDictionary<string,string>
    }
    with 
        static member Zero ct = 
            {Name=null; Type = ct; AllowNull = false; Attributes = List.empty; FKey = None; Comments = List.empty; GenerateReferenceTable = false; ReferenceValuesWithComment = null}

type TableInfo = { Name:string; Schema:string; Columns: ColumnInfo list}

//void GenerateTable(Manager manager, EnvDTE.Project targetProject, string targetProjectFolder, TableInfo ti)
type Targeting = TargetProject of EnvDTE.Project*targetProjectFolder:string

let generateTable (manager:IManager) (generationEnvironment:StringBuilder) targeting tableInfo =
    printfn "Generating a table into %A %A" targeting tableInfo
    let formatFKey (table:string) column fKey : string =
        match fKey with
        |None -> null
        | Some fKey -> 
            let fKeyColumn = if isNull fKey.Column then column else fKey.Column
            sprintf "CONSTRAINT [FK_%s_%s_%s_%s] FOREIGN KEY ([%s]) REFERENCES [%s].[%s] ([%s])" table column fKey.Table fKeyColumn column fKey.Schema fKey.Table fKeyColumn

    let appendLine text = generationEnvironment.AppendLine(text) |> ignore
    let appendLine' indentLevel text = delimit String.Empty (Enumerable.Repeat("    ",indentLevel)) + text |> appendLine
    appendLine "-- Generated file, DO NOT edit directly"
    appendLine (sprintf "CREATE TABLE [%s].[%s] (" tableInfo.Schema tableInfo.Name)
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
            if t = typeof<int> then
                "int"
            elif t = typeof<bool> then
                "bit"
            else t.Name

//        let projects = manager.Dte |> Option.bind (EnvDteHelper.recurseSolutionProjects >> Some) // was dte
//        let targetProject = projects |> Option.bind (fun projs -> projs.First(fun p -> p.Name = targetProjectName) |> Some)
//        let targetProjectFolder = targetProject |> Option.bind (fun tp -> Path.GetDirectoryName(tp.FullName) |> Some)

    match targeting with
        |Some (targetProject,targetProjectFolder) ->
            let targetFilename = Path.Combine(targetProjectFolder, "Schema Objects", "Schemas", tableInfo.Schema, "Tables", tableInfo.Name + ".table.sql")
            manager.StartNewFile(targetFilename, targetProject)
        | None -> ()
    
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
                "--" + (Seq.head ci.Comments)
            else String.Empty
        
        let formatAttributes (attributes: string seq) hasCombinationPK fKey allowNull = 
            let isPk = not <| isNull attributes && Seq.contains "primary key" attributes
            let needsStarter = allowNull || not isPk || hasCombinationPK
            let starter = (if allowNull then "null" elif needsStarter then "not null" else String.Empty) + (if needsStarter then " " else String.Empty)
            if isNull attributes then
                starter + (if not <| isNull fKey then " " + fKey else null)
            else
                let attribs = starter + (delimit " " (if hasCombinationPK && (not <| isNull attributes) then attributes |> Seq.except([| "primary key" |]) else attributes))
                if isNull fKey then
                    attribs
                else attribs + " " + fKey
                
        // TODO: finish translation
        sprintf "%-32s%-16s%s%s%s%s" 
            (sprintf "[%s]" ci.Name)
            (mapTypeToSql ci.Type) 
            (formatAttributes ci.Attributes hasCombinationPK fKey ci.AllowNull) 
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

let generateInserts title appendLine (manager:IManager) targetProject targetProjectFolder (tables:#seq<_>) targetRelativePath =
    // generate reference data
    let toGen = tables.Where( fun t -> t.Columns.Any( fun c -> not <| isNull c.ReferenceValuesWithComment && c.ReferenceValuesWithComment.Any())).ToArray()
    if not <| Seq.any toGen then
        ()
    else
        let targetFilename = Path.Combine(targetProjectFolder, targetRelativePath)
        manager.StartNewFile(targetFilename, targetProject)
    appendLine "-- Generated file, DO NOT edit directly"
    appendLine "SET ANSI_NULLS ON"
    appendLine "GO"
    appendLine "SET QUOTED_IDENTIFIER ON"
    appendLine "GO"
    appendLine (sprintf "PRINT 'Starting %s Synchronization'" title)
    appendLine "GO"
    for tbl in toGen do
    for column in tbl.Columns.Where( fun c -> Seq.any c.ReferenceValuesWithComment).ToArray() do
        let schema, table, columnName = 
            match column.FKey with
            | Some fKey -> 
                fKey.Schema, fKey.Table, if isNull fKey.Column then column.Name else fKey.Column
            | None -> failwithf "ReferenceValuesWithComment existed but no fkey"
        appendLine "---------------------------------------------------"
        appendLine (sprintf "PRINT 'Synchronizing [%s.%s]'" schema table)
        appendLine "WITH CTE AS"
        appendLine "("
        appendLine (sprintf "    SELECT [%s]" columnName)
        appendLine "    FROM (VALUES"
        let mutable i = 0
        let valueCount = column.ReferenceValuesWithComment.Keys.Count
        for k in column.ReferenceValuesWithComment.Keys do
            let comment = match column.ReferenceValuesWithComment.[k] with
                            |null -> String.Empty
                            |k -> " -- " + k
            appendLine (sprintf "        ('%s'%s)%s" (k.Replace("'","''")) (if i < valueCount - 1 then "," else ")" ) comment )
            i <- i + 1
        appendLine (sprintf "        AS SOURCE([%s])" columnName)
        appendLine ")"
        appendLine (sprintf "MERGE INTO [%s].[%s] AS TARGET" schema table)
        appendLine "USING CTE"
        appendLine (sprintf "ON CTE.[%s] = TARGET.[%s]" columnName columnName )
        appendLine "WHEN NOT MATCHED BY TARGET THEN"
        appendLine (sprintf "    INSERT([%s])" columnName)
        appendLine (sprintf "    VALUES([%s]);" columnName)
        appendLine String.Empty
        appendLine (sprintf "PRINT 'Done Synchronizing [%s.%s]'" schema table)
        appendLine "GO"
        appendLine String.Empty
        
    manager.EndBlock()
// end sql generation module
