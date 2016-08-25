namespace CodeGeneration
open System
open System.Collections.Generic
open System.Globalization
open System.Text

open BReusable

type Path = System.IO.Path
type IManager = MacroRunner.MultipleOutputHelper.IManager

module DataModelToF = 
    open System.Data.SqlClient
//    open System.Linq

    type ColumnDescription = {ColumnName:string; Type:string; Length:int; Nullable:bool; IsIdentity:bool}
    
    let generateTypeComment columnCount = sprintf "/// %i properties" columnCount
    
    let mapNullableType(targetType:string, nullable:bool, useOptions:bool ) = targetType + (if nullable then (if useOptions then " option" else " Nullable") else String.Empty)
    
    let mapSqlType(type' : string, nullable:bool , useOptions:bool) = 
        match type'.ToLower() with
            |"char"
            |"nchar"
            |"nvarchar"
            |"varchar" -> "string"
            |"bit" -> mapNullableType("bool", nullable, useOptions)
            |"date"
            |"datetime"
            |"smalldatetime" -> mapNullableType("DateTime", nullable, useOptions)
            |"uniqueidentifier" -> mapNullableType("Guid",nullable, useOptions)
            |"int" -> mapNullableType("int", nullable, useOptions)
            |"decimal" -> mapNullableType("decimal", nullable, useOptions)
            |"float" -> mapNullableType("float", nullable, useOptions)
            |_ -> if isNull type' then String.Empty else type'
    
    let generateColumnComment (cd:ColumnDescription) = sprintf "/// %s (%i) %s" (if isNull cd.Type then "null" else cd.Type) cd.Length (if cd.Nullable then "null" else "not null")
    
    let generateInterface (typeName:string, columns: ColumnDescription seq, appendLine:int -> string -> unit, writeable:bool , useOptions:bool ) =
        appendLine 0 (generateTypeComment (Seq.length columns))
        appendLine 0 ("type I" + typeName + (if writeable  then "RW" else String.Empty) + " =")
        if writeable then
            appendLine 1 ("inherit I" + typeName)
            
        for cd in columns do
            appendLine 1 (generateColumnComment cd)
            appendLine 1 ("abstract member " + cd.ColumnName + ":" + mapSqlType(cd.Type, cd.Nullable, useOptions) + " with get" + (if writeable then ",set" else String.Empty))
            
        appendLine 0 String.Empty
            
    let getDefaultValue (mappedType:string) =
        if mappedType.EndsWith("Nullable") then
            "Nullable()"
        elif mappedType.EndsWith("option") then
            "None"
        else 
            match mappedType.ToLower() with
                |"int" -> "0"
                |"bool" -> "false"
                |"decimal" -> "0m"
                |"float" -> "0."
                |"datetime" -> "System.DateTime.MinValue"
                |"uniqueidentifier" -> "Guid.Empty"
                |_ -> "null"
        
    let generateRecord(typeName:string, columns: ColumnDescription seq, appendLine:int -> string -> unit, useOptions:bool) =
        appendLine 0 (generateTypeComment (Seq.length columns))
        if not useOptions then
            appendLine 0 "[<NoComparison>]"
    
        appendLine 0 ("type " + typeName + "Record =")
        appendLine 1 "{"
    
        for cd in columns do
            appendLine 1 (generateColumnComment cd)
            appendLine 1 (cd.ColumnName + ":" + mapSqlType(cd.Type,cd.Nullable,useOptions))
    
        appendLine 1 "}"
        appendLine 1 ("interface I" + typeName + " with")
    
        for cd in columns do
            appendLine 2 ("member x." + cd.ColumnName + " with get () = x." + cd.ColumnName)
    
        appendLine 1 "static member Zero () = "
        appendLine 2 "{"
    
        for cd in columns do
            let mapped = mapSqlType(cd.Type, cd.Nullable, useOptions)
            appendLine 2 (cd.ColumnName + " = " + (getDefaultValue mapped))
    
        appendLine 2 "}"
        appendLine 0 String.Empty
    
    let toCamel s = // https://github.com/ayoung/Newtonsoft.Json/blob/master/Newtonsoft.Json/Utilities/StringUtils.cs
        if String.IsNullOrEmpty s then
            s
        elif not <| Char.IsUpper s.[0] then
            s
        else
            let camelCase = Char.ToLower(s.[0], CultureInfo.InvariantCulture).ToString(CultureInfo.InvariantCulture)
            if (s.Length > 1) then
                camelCase + (s.Substring 1)
            else 
                camelCase
    
    let generateModule (typeName:string, columns:ColumnDescription seq, schemaName:string, tableName:string, appendLine:int -> string -> unit, useOptions:bool ) =
        let camelType = toCamel typeName
        appendLine 0 ("module " + typeName + "Helpers =")
        appendLine 1 "open Microsoft.FSharp.Core.Operators.Unchecked"
        appendLine 1 String.Empty
        appendLine 1 ("let tableName = \"" + typeName + "\"")
        appendLine 1 ("let ToRecord (i" + typeName + ":I" + typeName + ") =")
        appendLine 2 "{"
    
        for cd in columns do
            let _mapped = mapSqlType(cd.Type,cd.Nullable,useOptions)
            appendLine 2 (cd.ColumnName + " = i" + typeName + "." + cd.ColumnName)
    
        appendLine 2 "}"
        appendLine 0 String.Empty
    
        appendLine 1 ("let toRecord (" + camelType + ":I"+ typeName + ") =")
        appendLine 2 "{"
    
        for cd in columns do
            let _mapped = mapSqlType(cd.Type,cd.Nullable,useOptions)
            appendLine 2 (cd.ColumnName + " = " + camelType + "." + cd.ColumnName)
    
        appendLine 2 "}"
        appendLine 0 String.Empty
    
        appendLine 1 "let FromF (camelTypeF:Func<string,obj option>) ="
        appendLine 2 "{"
    
        let mapConverter(type' : string , nullable: bool, useOptions:bool) = 
            match type'.ToLower() with 
                |"char"
                |"nchar"
                |"nvarchar"
                |"varchar" -> "ToString"
                |"bit" -> "ToBoolean"
                |"date"
                |"datetime"
                |"smalldatetime" -> "ToDateTime"
                |"uniqueidentifier" -> "ToGuid" // invalid
                |"int" -> "ToInt32"
                |"decimal" -> "ToDecimal"
                |"float"  -> "ToDouble"
                |_ -> if isNull type' then String.Empty else type'
    
        for cd in columns do
            let mapped = mapSqlType(cd.Type,cd.Nullable,useOptions)
            let converter = mapConverter(cd.Type,cd.Nullable,useOptions)
            appendLine 2 (cd.ColumnName + " = ")
            appendLine 3 ("match camelTypeF.Invoke \"" + cd.ColumnName + "\" with ")
    
            if cd.Nullable && (mapped <> typeof<string>.Name) && (mapped <> "string") && mapped <> "String" then
                appendLine 3 ("|Some x -> Nullable (Convert." + converter + " x )")
            else
                appendLine 3 ("|Some x -> Convert." + converter + " x ")
                appendLine 3 "|None -> Unchecked.defaultof<_>"
    
        appendLine 2 "}"
    
        appendLine 0 String.Empty
    
        appendLine 1 ("let inline toRecordStp (" + camelType + ": ^a) =")
        appendLine 2 "{"

        for cd in columns do
            let _mapped = mapSqlType(cd.Type,cd.Nullable,useOptions)
            appendLine 2 (cd.ColumnName + " = (^a: (member " + cd.ColumnName + ": _) " + camelType + ")")

        appendLine 2 "}"

        appendLine 0 String.Empty
        appendLine 1 ("let createInsert (r:I" + typeName + ") =")
        appendLine 2 ("let quoted s = \"'\" + s + \"'\"")
        let mapValue (cd:ColumnDescription, prefix:string) :string  = 
            match cd.Type.ToLower() with
                |"varchar" -> "if String.IsNullOrEmpty " + prefix + cd.ColumnName+ " then \"null\" else quoted " + prefix + cd.ColumnName
                |"int" -> if cd.Nullable then "if isNull (box " + prefix + cd.ColumnName + ") then \"null\" else " + prefix + cd.ColumnName + " |> string" else prefix + cd.ColumnName + " |> string"
                |_ ->  if cd.Nullable then "if isNull (box " + prefix + cd.ColumnName + ") then \"null\" else " + prefix + cd.ColumnName + " |> string |> quoted" else prefix + cd.ColumnName + " |> string |> quoted"
    
        appendLine 2 "["
    
        for cd in columns |> Seq.filter (fun c -> not c.IsIdentity) do
            let mapped = "\"" + cd.ColumnName + "\", " + mapValue(cd,"r.")
            appendLine 3 mapped
    
        appendLine 2 "]"
        appendLine 2 ("|> fun pairs -> sprintf \"insert into " + schemaName + "." + tableName + "(%s) values (%s)\" (String.Join(\",\", pairs |> Seq.map fst )) (String.Join(\",\", pairs |> Seq.map snd))" )
        appendLine 0 String.Empty
            
    let mapFieldNameFromType(columnName:string) = 
        match toCamel columnName with
        | "type" ->  "type'"
        | camel -> camel
            
    let generateClass(typeName:string, columns:ColumnDescription seq, appendLine:int -> string -> unit, useOptions:bool ) =
        appendLine 0 (generateTypeComment (Seq.length columns))
        appendLine 0 ("type "+ typeName + "N (model:" + typeName + "Record) = ")
        appendLine 0 String.Empty
        appendLine 1 "let propertyChanged = new Event<_, _>()"
        appendLine 0 String.Empty
    
        appendLine 0 String.Empty
        for cd in columns do // https://fadsworld.wordpress.com/2011/05/18/f-quotations-for-inotifypropertychanged/
            let camel = mapFieldNameFromType(cd.ColumnName)
            appendLine 1 ("let mutable "+ camel + " = model." + cd.ColumnName)
    
        appendLine 0 String.Empty
    
        appendLine 1 ("interface I" + typeName + " with")
    
        for cd in columns do
            appendLine 2 (generateColumnComment cd)
            appendLine 2 ("member x." + cd.ColumnName + " with get () = x." + cd.ColumnName)
    
        appendLine 1 ("interface I" + typeName + "RW with")

        for cd in columns do
            appendLine 2 (generateColumnComment cd)
            appendLine 2 ("member x." + cd.ColumnName + " with get () = x." + cd.ColumnName + " and set v = x." + cd.ColumnName + " <- v")
    
        appendLine 0 String.Empty
        appendLine 1 "member x.MakeRecord () ="
        appendLine 2 "{"
    
        for cd in columns do
            appendLine 2 (cd.ColumnName + " = x." + cd.ColumnName)
    
        appendLine 2 "}"
    
        appendLine 0 String.Empty
    
        appendLine 1 "interface INotifyPropertyChanged with"
        appendLine 2 "[<CLIEvent>]"
        appendLine 2 "member x.PropertyChanged = propertyChanged.Publish"
        appendLine 1 "abstract member RaisePropertyChanged : string -> unit"
        appendLine 1 "default x.RaisePropertyChanged(propertyName : string) = propertyChanged.Trigger(x, PropertyChangedEventArgs(propertyName))"
                     
        appendLine 0 String.Empty
        appendLine 1 "abstract member SetAndNotify<'t> : string * 't byref * 't -> bool"
        appendLine 1 "default x.SetAndNotify<'t> (propertyName, field: 't byref, value:'t) ="
        appendLine 2 "if obj.ReferenceEquals(box field,box value) then false"
        appendLine 2 "else"
        appendLine 3 "field <- value"
        appendLine 3 "x.RaisePropertyChanged(propertyNam)"
        appendLine 3 "true"

        appendLine 0 String.Empty
        appendLine 1 "abstract member SetAndNotify<'t,'b> : string * 'b * 't Action * 't -> bool"
        appendLine 1 "default x.SetAndNotify<'t,'b> (propertyName, baseValue:'b, baseSetter: 't Action, value:'t) ="
        appendLine 2 "if obj.ReferenceEquals(box baseValue,box value) then false"
        appendLine 2 "else"
        appendLine 3 "baseSetter.Invoke value"
        appendLine 3 "x.RaisePropertyChanged(propertyName)"
        appendLine 3 "true"
    
        for cd in columns do
            let camel = mapFieldNameFromType cd.ColumnName
            appendLine 0 String.Empty
            appendLine 1 (generateColumnComment cd)
            appendLine 1 ("member x." + cd.ColumnName)
            appendLine 2 ("with get() = " + camel)
            appendLine 2 "and set v = "
            appendLine 3 (camel + " <- v")
            appendLine 3 ("x.RaisePropertyChanged \"" + cd.ColumnName + "\"")

    let generate(manager:MacroRunner.MultipleOutputHelper.IManager, generationEnvironment:StringBuilder, targetProjectName:string, tables:string seq, cString:string , doMultiFile:bool) (pluralizer:string -> string) (singularizer:string -> string) useOptions=
        let appendLine text = generationEnvironment.AppendLine(text) |> ignore
        generationEnvironment.AppendLine(manager.TemplateFile) |> ignore // was host.TemplateFile
        let appendLine' indentLevels text = 
            let indentation = List.replicate indentLevels "    " (* Enumerable.Repeat("    ",indentLevels) *) |> delimit String.Empty
            generationEnvironment.AppendLine(indentation + text) |> ignore
        
        generationEnvironment.AppendLine("Main file output") |> ignore
        
        //let sol,projects = Macros.VsMacros.getSP dte //EnvDteHelper.recurseSolutionProjects dte 
        let projects = manager.Dte |> Option.map (Macros.VsMacros.getSP>>snd) // was dte
        let targetProject = projects |> Option.map (Seq.find (fun p -> p.Name = targetProjectName))
        let targetProjectFolder = targetProject |> Option.map (fun tp -> tp.FullName |> Path.GetDirectoryName)
    
        projects |> Option.iter (fun projs ->
            for p in projs do
                appendLine (p.Name + " " + p.FullName)
        )
     
        use cn = new SqlConnection(cString)
        cn.Open()
        
        for tableName in tables do
            match targetProjectFolder with
            | Some targetProjectFolder -> 
                manager.StartNewFile(Path.Combine(targetProjectFolder,tableName + ".generated.fs"),targetProject)
            | None -> manager.StartNewFile(tableName + ".generated.fs",targetProject)
            let typeName = singularizer tableName
            let columns = List<ColumnDescription>()
            let identities = List<string>()
            use cmd = new System.Data.SqlClient.SqlCommand("sp_help " + tableName,cn)
            use r = cmd.ExecuteReader()
            r.NextResult() |> ignore // ignore the first table
            while r.Read() do // column info
                // columns and info
                let columnName = r.["Column_name"].ToString()
                let type' = r.["Type"].ToString()
                // var computed = r["Computed"];
                let length = Convert.ToInt32(r.["Length"])
                // var prec = r["Prec"];
                columns.Add {ColumnName=columnName; Type= type'; Length=length; Nullable = r.["Nullable"].ToString() ="yes"; IsIdentity = false}
    
            r.NextResult() |> ignore
            while r.Read() do // identities
                if r.["Seed"] <> box System.DBNull.Value then // only valid identities (sql uses the identity column to say there are none defined instead of an empty set)
                    identities.Add(r.["Identity"].ToString())
            let columns = 
                columns
                |> Seq.map (fun c -> if identities.Contains(c.ColumnName) then {c with IsIdentity = true} else c)
                |> List.ofSeq
                
            let columns = columns |> List.sortBy(fun c -> c.ColumnName)
            appendLine ("namespace Pm.Schema.DataModels." + (pluralizer typeName) + " // Generated by item in namespace " + manager.DefaultProjectNamespace )
    
            appendLine String.Empty
            appendLine "open System"
            appendLine "open System.ComponentModel"
            appendLine "open System.Linq.Expressions"
            appendLine String.Empty
            appendLine "open FSharp.NullHelpers"
            
            generateInterface (typeName, columns, appendLine', false, useOptions)
            generateInterface (typeName, columns, appendLine', true, useOptions)
            generateRecord(typeName, columns, appendLine', useOptions)
            generateModule(typeName, columns, "dbo", tableName, appendLine', useOptions)
            generateClass(typeName, columns, appendLine', useOptions)
    
            manager.EndBlock()
    
        manager.Process doMultiFile

module SqlGeneration = 
    open System.Text
    open BReusable
    open System.Linq

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
        
        for ci in tableInfo.Columns do
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

// impure! applied code, meant for specific scripts, not api
module GenerationSample = 
    open Microsoft.VisualStudio.TextTemplating

    type GenerationStrategy = 
        | UseMultipleOutputHelperCode
        | UseCustomManager
    let generate connectionString hostDirectory= 
        let sb = StringBuilder()
        let mutable currentFile:string = null
        let pluralizer = Macros.VsMacros.createPluralizer()

        let getManager strat : IManager = 
            match strat with 
            |UseMultipleOutputHelperCode -> // WIP - generates a file, just to the wrong dir
            
                let host = 
                    let assRefs = List<string>()
                    let currentDir = hostDirectory // Path.Combine(Environment.ExpandEnvironmentVariables "%devroot%","PracticeManagement","dev","PracticeManagement", "PracticeManagement.Foundation") //Path.GetDirectory(Host.TemplateFile)
                    printfn "CurrentDirectory was %s" Environment.CurrentDirectory
                    Environment.CurrentDirectory <- currentDir
                    {new ITextTemplatingEngineHost
                        with
                            override __.GetHostOption optionName = printfn "Option requested:%s" optionName; null 
                            override __.LoadIncludeText (x,y,z) = printfn "LoadIncludetext:%A" (x,y,z); false
                            override __.LogErrors errors =printfn "loggedErrors: %A" (errors); Unchecked.defaultof<_>
                            override __.ProvideTemplatingAppDomain content = printfn "ProvideTemplatingAppDomain:%A" content; AppDomain.CreateDomain content
                            override __.ResolveAssemblyReference _ = printfn "ResolveAssemblyReference" ;Unchecked.defaultof<_>
                            override __.ResolveDirectiveProcessor _ = printfn "ResolveDirectiveProcessor"; Unchecked.defaultof<_>
                            override __.ResolveParameterValue (_,_,_) = printfn "ResolveParameterValue" ; Unchecked.defaultof<_>
                            override __.ResolvePath _ = printfn "ResolvePath"; Unchecked.defaultof<_>
                            override __.SetFileExtension _ = ()
                            override __.SetOutputEncoding (_, _) = ()
                            override __.StandardAssemblyReferences with get() = printfn "getting assRefs";  upcast assRefs 
                            override __.StandardImports with get() = printfn "getting StandardImports"; upcast assRefs
                            override __.TemplateFile with get() =printfn "GettingTemplateFile"; "DataModels.tt" //Path.Combine(currentDir,"DataModels.tt")
                    }
                upcast MacroRunner.MultipleOutputHelper.Managers.Manager(host,sb)
            | UseCustomManager -> 
                let generatedFileNames = List<string>()
                { new IManager
                     with 
                        override this.StartNewFile (s,p) = currentFile <- s;  sb.AppendLine(sprintf "// Starting a new file '%s' s for project opt '%A'" s p) |> ignore
                        override this.EndBlock () = sb.AppendLine(String.Empty) |> ignore; sb.AppendLine(sprintf "// file finished '%s'" currentFile) |> ignore
                        override this.Process doMultiFile = ()
                        override this.DefaultProjectNamespace with get() = "DefaultProjectNamespace"
                        override this.Dte = None
                        override this.TemplateFile with get() = "DataModels.tt"
                        override this.GeneratedFileNames = upcast generatedFileNames
                }

        let manager = getManager UseMultipleOutputHelperCode
        SqlGeneration.generateTable manager sb None 
            {
            Name="Users"; Schema="dbo"; 
            Columns= 
                [ 
                    { // should be [PaymentID]                     int             identity primary key,
                        Name="PaymentID"
                        Type = SqlGeneration.ColumnType.Other typeof<int>
                        Attributes = ["identity";"primary key" ]
                        AllowNull = false
                        FKey = None
                        Comments = List.empty
                        GenerateReferenceTable = false
                        ReferenceValuesWithComment = null
                    }
                ] 
            }

        //let generate(manager:IManager, generationEnvironment:StringBuilder , targetProjectName:string , tables:string seq, cString:string , doMultiFile:bool) (pluralizer:string -> string) (singularizer:string -> string) useOptions=
        DataModelToF.generate (manager, sb, "Pm.Schema", ["Users"], connectionString, (* doMultiFile *) true) pluralizer.Pluralize pluralizer.Singularize false

        manager.GeneratedFileNames
        |> dumpt "files generated"
        |> ignore
        sb.ToString()
        |> dumpt "generated"
