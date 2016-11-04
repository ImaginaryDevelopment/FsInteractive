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
    open System.Diagnostics
    open System.Data

//    open System.Linq
    let dbNullToOption (x:obj) : obj option= 
        if System.DBNull.Value.Equals x then 
            None
        else Some x

    // WIP: actual usage of isPrimaryKey (it's never populated and the checking part needs converted from the .ttinclude
    // isidentity may not be populated/checked in the right places
    type ColumnDescription = {ColumnName:string; Measure:string; Type:string; Length:int; Nullable:bool; IsPrimaryKey:bool; IsIdentity:bool}
    type TableGenerationInfo = {Schema:string; Name:string; GenerateFull:bool}

    let mapNullableType(targetType:string, nullable:bool, measureType:string, useOptions:bool ) = 
        
//        return targetType + (string.IsNullOrEmpty(measure)? string.Empty : "<" + measure + ">") + (nullable ? (useOptions ? " option" : " Nullable") : string.Empty);
//        return targetType + measureType + nullability);
        let nullability = 
            match nullable, useOptions with
            | true,true -> " option"
            | true,false -> " Nullable"
            | _ -> String.Empty
        let measureType = 
            match String.IsNullOrEmpty measureType with
            | true -> String.Empty
            | false -> sprintf "<%s>" measureType

        sprintf "%s%s%s" targetType measureType nullability
    
    let mapSqlType(type' : string, nullable:bool, measureType:string, useOptions:bool) = 
        match type'.ToLower() with
            |"char"
            |"nchar"
            |"nvarchar"
            |"xml"
            |"varchar" -> "string"
            |"bit" -> mapNullableType("bool", nullable, measureType, useOptions)
            |"date"
            |"datetime"
            |"datetime2"
            |"smalldatetime" -> mapNullableType("DateTime", nullable, measureType, useOptions)
            |"image" -> "byte[]"
            |"uniqueidentifier" -> mapNullableType("Guid",nullable, measureType, useOptions)
            |"int" -> mapNullableType("int", nullable, measureType, useOptions)
            |"decimal" -> mapNullableType("decimal", nullable, measureType, useOptions)
            |"float" -> mapNullableType("float", nullable, measureType, useOptions)
            |_ -> if isNull type' then String.Empty else type'
    
    let generateColumnComment (cd:ColumnDescription) = 
        let typeName = if isNull cd.Type then "null" else cd.Type
        let suffixes = 
            [
                if cd.IsIdentity then 
                    yield "identity"
                if cd.IsPrimaryKey then
                    yield "primaryKey"
            ]
        let nullability = if cd.Nullable then "null" else "not null"
        //let identity = if cd.IsIdentity then " identity" else String.Empty
        let suffix = if suffixes |> Seq.any then suffixes |> delimit " " |> (+) " " else String.Empty
        sprintf "/// %s (%i) %s%s" typeName cd.Length nullability suffix

    let generateTypeComment columnCount = sprintf "/// %i properties" columnCount
    
    type InterfaceGeneratorArgs = { Writeable:bool; UseOptions:bool}
    let generateInterface (typeName:string, columns: ColumnDescription seq, appendLine:int -> string -> unit, interfaceGeneratorArgs ) =
        appendLine 0 <| sprintf "// typeName:%s writeable:%A useOptions:%A" typeName interfaceGeneratorArgs.Writeable interfaceGeneratorArgs.UseOptions
        appendLine 0 (generateTypeComment (Seq.length columns))
        appendLine 0 ("type I" + typeName + (if interfaceGeneratorArgs.Writeable  then "RW" else String.Empty) + " =")
        if interfaceGeneratorArgs.Writeable then
            appendLine 1 ("inherit I" + typeName)
            
        columns
        |> Seq.iter (fun cd -> 
            appendLine 1 (generateColumnComment cd)
            let mappedSqlTpe = mapSqlType(cd.Type, cd.Nullable, cd.Measure, interfaceGeneratorArgs.UseOptions) 
            appendLine 1 <| sprintf "abstract member %s:%s with get%s" cd.ColumnName mappedSqlTpe (if interfaceGeneratorArgs.Writeable then ",set" else String.Empty)
        )

        appendLine 0 String.Empty

    let getDefaultValue (mappedType:string) (measureType:string) =
        if mappedType.EndsWith("Nullable") then
            "Nullable()"
        elif mappedType.EndsWith("option") then
            "None"
        else 
            let reMappedType,measuredValue = 
                if String.IsNullOrEmpty measureType then 
                    mappedType,String.Empty
                else 
                    try
                        match mappedType with
                        | "string" -> mappedType, String.Empty
                        | _ -> mappedType |> before "<", sprintf "<%s>"measureType
                    with ex ->
                        ex.Data.Add("mappedType", mappedType)
                        ex.Data.Add("measureType", measureType)
                        reraise()

            match reMappedType.ToLower() with
                |"int" -> "0" + measuredValue
                |"int64" -> "0L" + measuredValue
                |"bool" -> "false"
                |"decimal" -> "0m" + measuredValue
                |"float" -> "0." + measuredValue
                |"datetime" -> "System.DateTime.MinValue"
                |"uniqueidentifier" -> "Guid.Empty"
                |_ -> "null"

    let generateRecord(typeName:string, columns: ColumnDescription seq, appendLine:int -> string -> unit, useOptions:bool, generateValueRecord:bool) =
        //TODO: conversion of generate value records idea - some is already in the latest DataModelToF.ttinclude
        let columns = if generateValueRecord then columns |> Seq.filter(fun c -> not c.IsPrimaryKey) else columns
        let recordTypeName = if generateValueRecord then sprintf "type %sValueRecord" typeName else sprintf "type %sRecord" typeName

        appendLine 0 (generateTypeComment (Seq.length columns))
        if not useOptions then
            appendLine 0 "[<NoComparison>]"
            // isn't structuralEquality the default for records?
//            appendLine 0 "[<StructuralEquality;NoComparison>]"
    
        appendLine 0 <| sprintf "%s =" recordTypeName //"type " + typeName + "Record =")
        appendLine 1 "{"

        columns
        |> Seq.iter (fun cd -> 
            appendLine 1 <| generateColumnComment cd
            appendLine 1 <| cd.ColumnName + ":" + mapSqlType(cd.Type,cd.Nullable, cd.Measure, useOptions)
        )
    
        appendLine 1 "}"
        if not generateValueRecord then
            appendLine 1 <| "interface I" + typeName + " with"

            columns
            |> Seq.iter (fun cd -> 
                appendLine 2 <| "member x." + cd.ColumnName + " with get () = x." + cd.ColumnName
            )
        
        appendLine 0 String.Empty
        appendLine 1 "static member Zero () = "
        appendLine 2 "{"
    
        for cd in columns do
            let mapped = mapSqlType(cd.Type, cd.Nullable, cd.Measure, useOptions)
            try
                appendLine 2 (cd.ColumnName + " = " + (getDefaultValue mapped cd.Measure))
            with ex -> 
                ex.Data.Add("mapped", mapped)
                ex.Data.Add("ColumnName",cd.ColumnName)
                ex.Data.Add("Measure", cd.Measure)
                reraise()

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
//        appendLine 1 "open Microsoft.FSharp.Core.Operators.Unchecked"
        appendLine 1 String.Empty
        appendLine 1 "module Meta = "
        if not <| String.IsNullOrEmpty schemaName then
            appendLine 2 <| sprintf "let schemaName = \"%s\"" schemaName

        appendLine 2 <| sprintf "let tableName = \"%s\"" tableName
        columns
        |> Seq.map (fun c -> c.ColumnName, c.ColumnName)
        |> Seq.iter (uncurry (sprintf "let %s = \"%s\"") >> appendLine 2)
//        |> Seq.iter (fun c -> appendLine 2 <| sprintf "let %s = \"%s\"" c.ColumnName c.ColumnName)
        appendLine 0 String.Empty

        appendLine 1 <| sprintf "let ToRecord (i%s:I%s) =" typeName typeName
        appendLine 2 "{"
    
        for cd in columns do
            let _mapped = mapSqlType(cd.Type,cd.Nullable,cd.Measure,useOptions)
            appendLine 3 <| sprintf "%s = i%s.%s" cd.ColumnName typeName cd.ColumnName
    
        appendLine 2 "}"
        appendLine 0 String.Empty
    
        appendLine 1 <| sprintf "let toRecord (%s:I%s) =" camelType typeName
        appendLine 2 "{"
    
        for cd in columns do
            let _mapped = mapSqlType(cd.Type,cd.Nullable,cd.Measure,useOptions)
            appendLine 3 (cd.ColumnName + " = " + camelType + "." + cd.ColumnName)
    
        appendLine 2 "}"
        appendLine 0 String.Empty
    
        appendLine 1 "let FromF (camelTypeF:Func<string,obj option>) ="
        appendLine 2 "{"
    
        let mapConverter(type' : string , _nullable: bool, _useOptions:bool) = 
            match type'.ToLower() with 
                |"char"
                |"nchar"
                |"nvarchar"
                |"xml"
                |"varchar" -> "ToString"
                |"bit" -> "ToBoolean"
                |"image" -> "ToBinaryData"
                |"date"
                |"datetime"
                |"datetime2"
                |"smalldatetime" -> "ToDateTime"
                |"uniqueidentifier" -> "ToGuid" // invalid
                |"int" -> "ToInt32"
                |"decimal" -> "ToDecimal"
                |"float"  -> "ToDouble"
                |_ -> if isNull type' then String.Empty else type'
    
        let nonNullables = ["string";"byte[]"]
        for cd in columns do
            let mapped = mapSqlType(cd.Type,cd.Nullable,cd.Measure,useOptions)
            let converter = mapConverter(cd.Type,cd.Nullable,useOptions)
            appendLine 2 (cd.ColumnName + " = ")
            appendLine 3 <| sprintf "match camelTypeF.Invoke \"%s\" with // %s" cd.ColumnName mapped
            let measureType = if String.IsNullOrEmpty cd.Measure || stringEqualsI mapped "string" then String.Empty else sprintf " |> (*) 1<%s>" cd.Measure
    
            if cd.Nullable && nonNullables |> Seq.exists (stringEqualsI mapped) |> not then//(mapped <> typeof<string>.Name) && stringEqualsI mapped "string" |> not  then
                sprintf "|Some x -> Nullable (Convert.%s x%s)" converter measureType
            else
                sprintf "|Some x -> Convert.%s x%s" converter measureType
            |> appendLine 3

            appendLine 3 "|None -> Unchecked.defaultof<_>"
    
        appendLine 2 "}"
    
        appendLine 0 String.Empty
    
        appendLine 1 ("let inline toRecordStp (" + camelType + ": ^a) =")
        appendLine 2 "{"

        for cd in columns do
            let mapped = mapSqlType(cd.Type,cd.Nullable,cd.Measure,useOptions)
            let measureType = 
                match String.IsNullOrEmpty cd.Measure || stringEqualsI mapped "string", cd.Nullable with
                | true, _ -> String.Empty
                | false, true -> sprintf "|> Nullable.map((*) 1<%s>)" cd.Measure
                | false, false -> sprintf " * 1<%s>" cd.Measure

            
            appendLine 3 <| sprintf "%s = (^a: (member %s: _) %s)%s" cd.ColumnName cd.ColumnName camelType measureType

        appendLine 2 "}"

//        var canDoInsert =  !columns.Any(c => c.Type == "image" || c.Type == "byte[]");
        let canDoInsert = columns |> Seq.exists(fun c -> c.Type = "image" || c.Type = "byte[]") |> not
        if canDoInsert then
//        if(canDoInsert)
            appendLine 0 String.Empty
            appendLine 1 ("let createInsert blacklist (r:I" + typeName + ") =")
            let needsQuoted (c:ColumnDescription) = ["varchar"; "char"; "nvarchar"; "nchar";"datetime";"xml";"datetime2"] |> Seq.tryFind (fun n -> match c.Type with |IsTrue (containsI n) -> true | _ -> false)
            
            match columns |> Seq.filter (fun c -> not c.IsIdentity) |> Seq.choose needsQuoted |> Seq.tryHead with
            | Some firstTypeToNeedQuoteF -> 
                firstTypeToNeedQuoteF
                |> sprintf "let quoted (s:string) = \"'\" + s.Replace(\"'\",\"''\") + \"'\" // %s" 
                |> appendLine 2 
            | None -> ()

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
            appendLine 2 <| "|> Seq.filter (fun kvp -> blacklist |> Seq.contains (fst kvp) |> not)"
            appendLine 2 <| "|> fun pairs -> sprintf \"insert into " + schemaName + "." + tableName + "(%s) values (%s)\" (String.Join(\",\", pairs |> Seq.map fst )) (String.Join(\",\", pairs |> Seq.map snd))" 
            appendLine 2 <| "|> sprintf \"%s;select SCOPE_IDENTITY()\""
        appendLine 0 String.Empty

    let generateINotifyClass(typeName:string, columns:ColumnDescription seq, appendLine:int -> string -> unit, useOptions:bool ) =
        let mapFieldNameFromType(columnName:string) = 
            match toCamel columnName with
            | "type" ->  "type'"
            | camel -> camel
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
            appendLine 3 (cd.ColumnName + " = x." + cd.ColumnName)
    
        appendLine 2 "}"
    
        appendLine 0 String.Empty
    
        appendLine 1 "interface INotifyPropertyChanged with"
        appendLine 2 "[<CLIEvent>]"
        appendLine 2 "member x.PropertyChanged = propertyChanged.Publish"
        appendLine 1 "abstract member RaisePropertyChanged : string -> unit"
        appendLine 1 "default x.RaisePropertyChanged(propertyName : string) = propertyChanged.Trigger(x, PropertyChangedEventArgs(propertyName))"

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

    type CodeGenSettingMap = {
        TargetProjectName:string
        TargetNamespace: string
        CString:string
        UseOptionTypes:bool
        ColumnBlacklist:Map<string, string list>
        Measures: string list
        MeasuresBlacklist: string list
        IncludeNonDboSchemaInNamespace:bool
        GenerateValueRecords:bool
    }

    let generate (fPluralizer:string -> string) (fSingularizer:string -> string) (cgsm:CodeGenSettingMap) (manager:MacroRunner.MultipleOutputHelper.IManager, generationEnvironment:StringBuilder, tables:TableGenerationInfo seq) =

        let appendLine text = generationEnvironment.AppendLine(text) |> ignore
        let appendEmpty() = appendLine String.Empty
        let appendLine' indentLevels text = 
            let indentation = List.replicate indentLevels "    " (* Enumerable.Repeat("    ",indentLevels) *) |> delimit String.Empty
            generationEnvironment.AppendLine(indentation + text) |> ignore
        appendEmpty()
        appendLine <| sprintf "TemplateFile: %s" manager.TemplateFile
        appendEmpty()

        //let sol,projects = Macros.VsMacros.getSP dte //EnvDteHelper.recurseSolutionProjects dte 
        let projects = manager.DteWrapperOpt |> Option.map (fun dte -> dte.GetProjects()) //(Macros.VsMacros.getSP>>snd) // was dte
        let targetProjectFolder = 
            projects |> Option.map (Seq.find (fun p -> p.Name = cgsm.TargetProjectName))
            |> Option.map (fun tp -> tp.FullName |> Path.GetDirectoryName)

        appendLine "Projects"
    
        projects |> Option.iter (Seq.iter (fun p -> 
                appendLine' 1 (p.Name + " " + p.FullName)
            )
        )
        appendEmpty()
        appendLine "Tables"

        tables |> Seq.iter (fun t ->
            appendLine' 1 <| sprintf "%s.%s" t.Schema t.Name
        )

        appendEmpty()
        use cn = new SqlConnection(cgsm.CString)
        cn.Open()

        appendLine <| sprintf "Connected to %s,%s" cn.DataSource cn.Database
        appendLine String.Empty
        let startNewFile path = manager.StartNewFile(path)
        for tableInfo in tables do
            let typeName = fSingularizer tableInfo.Name
            appendLine <| sprintf "Starting table %s as type %s" tableInfo.Name typeName

            let columns = List<ColumnDescription>()
//            let identities = List<string>()
            let cmdText = sprintf "sp_help '%s.%s'" tableInfo.Schema tableInfo.Name
            use cmd = new System.Data.SqlClient.SqlCommand(cmdText,cn)
            use r = 
                try
                    cmd.ExecuteReader()
                with ex ->
                    let ex = InvalidOperationException(sprintf "cmdText: %s" cmdText, ex)
                    ex.Data.Add("tableInfo.Name", tableInfo.Name)
                    raise ex
            let unfoldRows f (r:System.Data.IDataReader) = 
                Seq.unfold(fun _ -> 
                    if r.Read() then
                        Some(r :> IDataRecord, ())
                    else None
                ) () 
                |> f
                |> List.ofSeq
            r.NextResult() |> Debug.Assert // ignore the first table
            
            while r.Read() do // column info
                // columns and info
                let columnName = r.["Column_name"] |> string
                let type' = r.["Type"] |> string
                // var computed = r["Computed"];
                let length = Convert.ToInt32 r.["Length"]
                // var prec = r["Prec"];
                let measureType = cgsm.Measures |> Seq.tryFind (fun m -> cgsm.MeasuresBlacklist |> Seq.contains columnName |> not && containsI m columnName)
                columns.Add {ColumnName=columnName; Type= type'; Measure = measureType |> Option.toObj; Length=length; Nullable = r.["Nullable"].ToString() ="yes"; IsIdentity = false; IsPrimaryKey = false}
    
            r.NextResult() |> Debug.Assert

//            let identities = 
//                seq {
//                    while r.Read() do // identities
//                        // only valid identities (sql uses the identity column to say there are none defined instead of an empty set)
//                        match r.["Seed"] |> dbNullToOption with
//                        | Some _ -> 
//                            yield r.["Identity"] |> string
//                        | None -> ()
//                }
//                |> List.ofSeq
            let identities = 
                r
                |> unfoldRows (
                    Seq.filter(fun r -> r.["Seed"] |> dbNullToOption |> Option.isSome)
                    >> Seq.map (fun r -> r.["Identity"] |> string)
                )
                |> Set.ofSeq
            // rowGuidCol result set
            r.NextResult() |> Debug.Assert
            //Data_located_on_filegroup
            r.NextResult() |> Debug.Assert
            //indexes
            r.NextResult() |> Debug.Assert
            //multi-row constraints (some rows are more data from the previous row
            r.NextResult() |> Debug.Assert
            let (|PrimaryKey|_|) (r:IDataRecord) =
                r.["constraint_type"]
                |> dbNullToOption
                |> Option.map string
                |> function
                    | None -> None
                    | Some s ->
                        match s with
                        | ValueString ct -> if ct |> startsWithI "PRIMARY KEY" then Some() else None
                        | _ -> None
            let pks = 
                seq {
                    while r.Read() do // have not checked what this does on a table with a composite primary key
                        match r with
                        | PrimaryKey -> yield r.["constraint_keys"] |> string
                        | _ -> ()
                }
                |> List.ofSeq

            let columns = 
                let fIncludeColumn c = isNull (box cgsm.ColumnBlacklist) || cgsm.ColumnBlacklist |> Map.containsKey tableInfo.Name |> not || cgsm.ColumnBlacklist.[tableInfo.Name] |> Seq.contains c.ColumnName |> not 
                columns
                |> Seq.filter fIncludeColumn
                |> Seq.map (fun c -> if identities.Contains(c.ColumnName) then {c with IsIdentity = true} else c)
                |> Seq.map (fun c -> if pks |> Seq.exists (stringEqualsI c.ColumnName) then {c with IsPrimaryKey = true} else c)
                |> List.ofSeq

            let columns = columns |> List.sortBy(fun c -> c.ColumnName)
            columns 
            |> Seq.map (fun c -> sprintf "%s%s" c.ColumnName (if String.IsNullOrEmpty c.Measure then String.Empty else sprintf "<%s>" c.Measure))
            |> Seq.iter (appendLine' 1)

            let templateProjectNamespace = manager.DefaultProjectNamespace

            match targetProjectFolder with
            | Some targetProjectFolder -> Path.Combine(targetProjectFolder,tableInfo.Name + ".generated.fs")
            | None -> tableInfo.Name + ".generated.fs"
            |> startNewFile

            let typeTargetNamespace = 
                let subNamespaceName = fPluralizer typeName 
                match tableInfo.Schema, cgsm.IncludeNonDboSchemaInNamespace with
                | "dbo", _  
                | _, false ->
                    appendLine <| sprintf "namespace %s.%s // Generated by item in namespace %s" cgsm.TargetNamespace subNamespaceName templateProjectNamespace
                | x, true -> 
                    appendLine <| sprintf "namespace %s.%s.%s // Generated by item in namespace %s" cgsm.TargetNamespace x subNamespaceName templateProjectNamespace

            appendEmpty()

            appendLine "open System"
            appendLine "open System.ComponentModel"
            appendLine "open System.Linq.Expressions"
            appendEmpty()

            if columns |> Seq.exists (fun c -> c.Measure |> String.IsNullOrEmpty |> not) then
                appendLine "open Pm.Schema"
            appendLine "open Pm.Schema.BReusable"
            appendEmpty()

            let iga = {UseOptions=cgsm.UseOptionTypes;Writeable=false}
            generateInterface (typeName, columns, appendLine', iga)
            appendEmpty()

            generateInterface (typeName, columns, appendLine', {iga with Writeable=true})
            if cgsm.GenerateValueRecords then
                generateRecord(typeName, columns, appendLine', cgsm.UseOptionTypes, true)

            generateRecord(typeName, columns, appendLine', cgsm.UseOptionTypes, false)
            generateModule(typeName, columns, tableInfo.Schema, tableInfo.Name, appendLine', cgsm.UseOptionTypes)
            generateINotifyClass(typeName, columns, appendLine', cgsm.UseOptionTypes)

            manager.EndBlock()
    
//        manager.Process doMultiFile

//     TargetProjectName:string
//        TargetNamespace: string
//        CString:string
//        UseOptionTypes:bool
//        ColumnBlacklist:Map<string, string list>
//        Measures: string list
//        MeasuresBlacklist: string list
//        IncludeNonDboSchemaInNamespace:bool
//        GenerateValueRecords:bool
    let Generate (pluralizer:Func<_,_>,singularizer:Func<_,_>) (columnBlacklist:IDictionary<string, string seq>) (manager:MacroRunner.MultipleOutputHelper.IManager, generationEnvironment:StringBuilder, targetProjectName:string, tables, cString:string) useOptions generateValueRecords (measures: string seq) (measureBlacklist: string seq) includeNonDboSchemaInNamespace targetNamespace = 
        let columnBlacklist = 
            columnBlacklist |> Map.ofDictionary |> Map.toSeq |> Seq.map (fun (k,v) -> KeyValuePair(k, v |> List.ofSeq)) 
            |> Map.ofDictionary

        let cgsm = {TargetProjectName= targetProjectName; TargetNamespace=targetNamespace; CString=cString; UseOptionTypes=useOptions; ColumnBlacklist = columnBlacklist; Measures=measures |> List.ofSeq; MeasuresBlacklist= measureBlacklist |> List.ofSeq; IncludeNonDboSchemaInNamespace= includeNonDboSchemaInNamespace; GenerateValueRecords=generateValueRecords}
        generate 
            pluralizer.Invoke
            singularizer.Invoke
            cgsm
            (manager, generationEnvironment, tables)

    // dbPath was Path.GetFullPath(Path.Combine(currentDir, "..", "..","PracticeManagement","Db"));
module SqlProj =
    open System.IO
    open DataModelToF
    type TableSpecifier = {Path:string; Schema:string; Table:string; GenerateFull:bool}
    let toTableGenerationInfo (ts:TableSpecifier) : TableGenerationInfo = 
        {TableGenerationInfo.Name = ts.Table; Schema= ts.Schema; GenerateFull = ts.GenerateFull}

    type DbPathOption = 
        |MustExist
        |FallbackToNone
    let getTables dbPath = 
        Directory.GetFiles(dbPath, "*.table.sql", SearchOption.AllDirectories)
        |> Seq.map (fun tp -> {Path=tp; Schema = tp |> after "Schemas\\" |> before "\\"; Table=tp |> Path.GetFileNameWithoutExtension |> Path.GetFileNameWithoutExtension; GenerateFull = false})

    // tables may be just a simple table name (implying schema is dbo) or 'schema.tablename'
    // fLog and fAppend appear to be the same place, in current usage, why were they ever distinct?
    let getTableInfoFromSqlProj fLog fAppend pathOption sqlProjRootDir tables blacklist generatePartials= 
        let sqlProjRootDirOpt = if not <| String.IsNullOrEmpty sqlProjRootDir && Directory.Exists sqlProjRootDir then Some sqlProjRootDir else None
        match Option.isSome sqlProjRootDirOpt, pathOption with
        | false,MustExist -> failwithf "Sql project directory not found at %s" sqlProjRootDir
        | false, FallbackToNone -> ()
        | true, _ -> 
            fLog <| sprintf "found sql project directory at %s" sqlProjRootDir
        let dbTablesOpt = 
            match sqlProjRootDirOpt with 
            | Some dbPath -> getTables dbPath |> Some
            | None -> None
        let fAppendI indentLevels text = List.replicate indentLevels "    " |> delimit String.Empty |> flip (+) text |> fAppend
        fAppend "dbTables"
        dbTablesOpt 
        |> Option.iter (Seq.iter (fun dt -> sprintf "%s.%s" dt.Schema dt.Table |> fAppendI 1))
        fAppendI 0 String.Empty
        let allTables = 
            match dbTablesOpt with
            | Some dbTables -> 
                dbTables 
                |> Seq.map (fun t -> {t with GenerateFull = tables |> Seq.contains t.Table || tables |> Seq.contains (sprintf "%s.%s" t.Schema t.Table) })
                |> List.ofSeq
            | None ->  
                tables 
                |> Seq.map (fun t -> 
                    let schema,table = 
                        if t |> contains "." then 
                            t |> before".", t |> after "." 
                        else "dbo", t
                    {GenerateFull = true; Schema= schema; Table= table;Path=null }
                )
                |> List.ofSeq
            |> Seq.filter (fun t-> blacklist |> Seq.contains t.Table |> not)
            |> Seq.filter (fun t -> generatePartials || t.GenerateFull)
            |> List.ofSeq
        match sqlProjRootDirOpt with
        | Some _ ->
            fAppend "allTables"
            allTables 
            |> Seq.iter (fun t -> fAppendI 1 <| sprintf "%s.%s,%s" t.Schema t.Table t.Path)
        | None -> fLog <| sprintf " didn't find it at %s" sqlProjRootDir

        allTables

    let GetTableInfoFromSqlProj (log:Action<_>) (append:Action<_>) pathOption sqlProjRootDir tables blacklist generatePartials = getTableInfoFromSqlProj log.Invoke append.Invoke pathOption sqlProjRootDir tables blacklist generatePartials

// impure! applied code, meant for specific scripts, not api
module GenerationSample = 
    open Microsoft.VisualStudio.TextTemplating
    open SqlMeta
    open DataModelToF

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
                        override __.StartNewFile s = currentFile <- s;  sb.AppendLine(sprintf "// Starting a new file '%s' s" s) |> ignore
                        override __.EndBlock () = sb.AppendLine(String.Empty) |> ignore; sb.AppendLine(sprintf "// file finished '%s'" currentFile) |> ignore
                        override __.Process _doMultiFile = List.empty |> dict
                        override __.DefaultProjectNamespace with get() = "DefaultProjectNamespace"
                        override __.DteWrapperOpt = None
                        override __.TemplateFile with get() = "DataModels.tt"
                        override __.GeneratedFileNames = upcast generatedFileNames
                }

        let manager = getManager UseMultipleOutputHelperCode
        generateTable manager sb None 
            {
            Name="Users"; Schema="dbo"; 
            Columns= 
                [ 
                    { // should be [PaymentID]                     int             identity primary key,
                        Name="PaymentID"
                        Type = ColumnType.Other typeof<int>
                        Attributes = ["identity";"primary key" ]
                        AllowNull = NotNull
                        FKey = None
                        Comments = List.empty
                        GenerateReferenceTable = false
                        ReferenceValuesWithComment = null
                    }
                ] 
            }
        let tablesToGen = [
            {Schema="dbo"; Name="Users"; GenerateFull =false}
        ]
//         type CodeGenSettingMap = {
//        TargetProjectName:string
//        TargetNamespace: string
//        CString:string
//        UseOptionTypes:bool
//        ColumnBlacklist:Map<string, string list>
//        Measures: string list
//        MeasuresBlacklist: string list
//        IncludeNonDboSchemaInNamespace:bool
//        GenerateValueRecords:bool
//    }
        let _results = 
            let cgsm = {TargetProjectName= null; TargetNamespace="Pm.Schema"; CString=connectionString; UseOptionTypes=false; ColumnBlacklist = Map.empty;Measures=List.empty; MeasuresBlacklist= List.empty; IncludeNonDboSchemaInNamespace= false; GenerateValueRecords = false}
            DataModelToF.generate pluralizer.Pluralize pluralizer.Singularize cgsm (manager, sb, tablesToGen)
        

        manager.GeneratedFileNames
        |> dumpt "files generated"
        |> ignore
        sb.ToString()
        |> dumpt "generated"
