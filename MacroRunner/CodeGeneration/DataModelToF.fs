// file focus is on creating F# types from either a coded spec, or a sql connection (with whitelist/mapping data)
namespace CodeGeneration
open System
open System.Collections.Generic
open System.Globalization
open System.Text

open BReusable
open BReusable.StringHelpers
open MacroRunner.AdoHelper

type Path = System.IO.Path
type IManager = MacroRunner.MultipleOutputHelper.IManager

module DataModelToF =
    let hoist f x = f x |> ignore; x
    type private MeasureText = string

    open System.Data.SqlClient
    open System.Diagnostics
    open System.Data
    open Macros.SqlMacros
    open SqlMeta

    let private log (s:string) =
        let text = if s.EndsWith "\r\n" then s else sprintf "%s\r\n" s
        printfn "%s" text
        if System.Diagnostics.Debugger.IsAttached then
            System.Diagnostics.Debugger.Log(0, "Logger", text)

    type TableGenerationInput = {Id: TableIdentifier; GenerateFull:bool}

    type CodeGenSprocSettingMap = {
        SprocBlacklist: string list
        GenerateSprocInputRecords: bool
    }

    type CodeGenSettingMap = {
        TargetProjectName:string
        TargetNamespace: string
        CString:string
        UseOptionTypes:bool
        ColumnBlacklist:Map<string, string list>
        Measures: string list
        ///Needs to include any namespace that defines:
        /// - Func<_>.invoke1 ( type System.Func<'tResult> with static member invoke1<'t> (x:System.Func<'t,'tResult>) y = x.Invoke y)
        AdditionalNamespaces: string list
        GetMeasureNamepace: (string -> string) option

        MeasuresBlacklist: string list
        IncludeNonDboSchemaInNamespace:bool
        // not functional yet, right?
        GenerateValueRecords:bool
        SprocSettingMap: CodeGenSprocSettingMap option
        UseCliMutable:bool
    }

    let mapNullableType(targetType:string, nullable:bool, measureType:string, useOptions:bool ) =
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

    // take the sql type and give back what .net type we're using
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
    let generateInterface (typeName:string, columns: (ColumnDescription*MeasureText) seq, appendLine:int -> string -> unit, interfaceGeneratorArgs ) =
        appendLine 0 <| sprintf "// typeName:%s writeable:%A useOptions:%A" typeName interfaceGeneratorArgs.Writeable interfaceGeneratorArgs.UseOptions
        appendLine 0 (generateTypeComment (Seq.length columns))
        appendLine 0 ("type I" + typeName + (if interfaceGeneratorArgs.Writeable  then "RW" else String.Empty) + " =")
        if interfaceGeneratorArgs.Writeable then
            appendLine 1 ("inherit I" + typeName)

        columns
        |> Seq.iter (fun (cd,measureText) ->
            appendLine 1 (generateColumnComment cd)
            let mappedSqlTpe = mapSqlType(cd.Type, cd.Nullable, measureText, interfaceGeneratorArgs.UseOptions)
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

    ///useCliMutable : enables simple serialization see also http://blog.ploeh.dk/2013/10/15/easy-aspnet-web-api-dtos-with-f-climutable-records/
    let generateRecord useCliMutable (typeName:string) (columns: (ColumnDescription*MeasureText) seq, appendLine:int -> string -> unit, useOptions:bool, generateValueRecord:bool) =
        // what is a value record?
        // - a record that is fully nullable so that all fields can indicate if a value was provided (in cases where the default value has a meaning like set the value to null)
        // - something that always uses options, instead of allowing nullables?
        // - something without the pk?
        let columns = if generateValueRecord then columns |> Seq.filter(fst >> (fun c -> not c.IsPrimaryKey)) else columns

        let recordTypeName = if generateValueRecord then sprintf "type %sValueRecord" typeName else sprintf "type %sRecord" typeName
        appendLine 0 (generateTypeComment (Seq.length columns))
        if not useOptions then
            appendLine 0 "[<NoComparison>]"
        if useCliMutable then
            appendLine 0 "[<CLIMutable>]"

        appendLine 0 <| sprintf "%s =" recordTypeName //"type " + typeName + "Record =")
        appendLine 1 "{"

        columns
        |> Seq.iter (fun (cd,measureText) ->
            appendLine 1 <| generateColumnComment cd
            appendLine 1 <| cd.ColumnName + ":" + mapSqlType(cd.Type,cd.Nullable, measureText, useOptions)
        )

        appendLine 1 "}"
        if not generateValueRecord then
            appendLine 1 <| "interface I" + typeName + " with"

            columns
            |> Seq.iter (fst >> fun cd ->
                appendLine 2 <| "member x." + cd.ColumnName + " with get () = x." + cd.ColumnName
            )

        appendLine 0 String.Empty
        appendLine 1 "static member Zero () = "
        appendLine 2 "{"

        columns
        |> Seq.iter(fun (cd,measureText) ->
            let mapped = mapSqlType(cd.Type, cd.Nullable, measureText, useOptions)
            try
                appendLine 2 (cd.ColumnName + " = " + (getDefaultValue mapped measureText))
            with ex ->
                ex.Data.Add("mapped", mapped)
                ex.Data.Add("ColumnName",cd.ColumnName)
                ex.Data.Add("Measure", measureText)
                reraise()
        )

        appendLine 2 "}"
        appendLine 0 String.Empty

    /// assumes all that is needed is the first char changed, does not account for underscoring
    let toCamelCase s = // https://github.com/ayoung/Newtonsoft.Json/blob/master/Newtonsoft.Json/Utilities/StringUtils.cs
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

    let generateFromStpMethod appendLine useOptions camelType (columns :(ColumnDescription*MeasureText) seq)=
        appendLine 0 String.Empty

        appendLine 1 ("let inline toRecordStp (" + camelType + ": ^a) =")
        appendLine 2 "{"

        columns
        |> Seq.iter(fun (cd,measureText) ->
            let mapped = mapSqlType(cd.Type,cd.Nullable,measureText,useOptions)
            let measureType =
                match String.IsNullOrEmpty measureText || stringEqualsI mapped "string", cd.Nullable with
                | true, _ -> String.Empty
                | false, true -> sprintf "|> Nullable.map((*) 1<%s>)" measureText
                | false, false -> sprintf " * 1<%s>" measureText

            appendLine 3 <| sprintf "%s = (^a: (member %s: _) %s)%s" cd.ColumnName cd.ColumnName camelType measureType
        )

        appendLine 2 "}"
        ()

    let generateCreateSqlInsertTextMethod appendLine useOptions typeName schemaName tableName (columns:(ColumnDescription*MeasureText) seq) =
        let canDoInsert = columns |> Seq.exists(fst >> fun c -> c.Type = "image" || c.Type = "byte[]") |> not
        if canDoInsert then
            appendLine 0 String.Empty
            // idea: put what running the .Zero() record against this function would generate in a comment
            let insertStart = sprintf "insert into %s.%s(%s) values (%s)" schemaName tableName
            let hasIdentity = 
                columns
                |> Seq.map fst
                |> Seq.exists(fun c -> c.IsIdentity)
            columns
            |> Seq.map (fun (cd,mt) -> cd.ColumnName, mapSqlType(cd.Type, cd.Nullable, mt, useOptions) |> flip getDefaultValue null)
            |> List.ofSeq
            |> fun s -> s |> Seq.map fst, s |> Seq.map snd
            |> fun (names,values) ->
                insertStart (names |> delimit ",") (values |> delimit ",")
                |> sprintf "//%s"
                |> appendLine 1
            let insertMethodName = if hasIdentity then "createInsertReturnIdentity" else "createInsert"
            appendLine 1 <| sprintf "let %s blacklist (r:I%s) =" insertMethodName typeName
            let needsQuoted (c:ColumnDescription) = ["varchar"; "char"; "nvarchar"; "nchar";"datetime";"xml";"datetime2"] |> Seq.tryFind (fun n -> match c.Type with |IsTrue (containsI n) -> true | _ -> false)

            match columns |> Seq.filter (fst >> fun c -> not c.IsIdentity) |> Seq.choose (fst >> needsQuoted) |> Seq.tryHead with
            | Some firstTypeToNeedQuoteF ->
                firstTypeToNeedQuoteF
                |> sprintf "let quoted (s:string) = \"'\" + s.Replace(\"'\",\"''\") + \"'\" // %s"
                |> appendLine 2
            | None -> ()

            let mapValue (cd:ColumnDescription, prefix:string) :string  =
                let fullCName = prefix + cd.ColumnName
                match cd.Type.ToLower() with
                    |"varchar" -> sprintf "if String.IsNullOrEmpty %s then \"null\" else quoted %s" fullCName fullCName
                    |"int" ->  
                        if cd.Nullable then 
                            "if isNull (box " + fullCName + ") then \"null\" else " + fullCName + " |> string" 
                        else fullCName + " |> string"
                    |_ ->  if cd.Nullable then "if isNull (box " + fullCName + ") then \"null\" else " + fullCName + " |> string |> quoted" else fullCName + " |> string |> quoted"

            appendLine 2 "["

            columns
            |> Seq.map fst
            |> Seq.filter (fun c -> not c.IsIdentity)
            |> Seq.iter(fun cd->
                let mapped = "\"" + cd.ColumnName + "\", " + mapValue(cd,"r.")
                appendLine 3 mapped
            )

            appendLine 2 "]"
            appendLine 2 <| "|> Seq.filter (fun kvp -> blacklist |> Seq.contains (fst kvp) |> not)"
            appendLine 2 <| sprintf "|> fun pairs -> sprintf \"%s\" (String.Join(\",\", pairs |> Seq.map fst )) (String.Join(\",\", pairs |> Seq.map snd))" (insertStart "%s" "%s")
            // this should not be happening if there is no identity on the table
            if hasIdentity then
                appendLine 2 <| "|> sprintf \"%s;select SCOPE_IDENTITY()\""
        ()

    /// generate the helper module
    let generateModule (typeName:string, columns:(ColumnDescription*MeasureText) seq, schemaName:string, tableName:string, appendLine:int -> string -> unit, useOptions:bool ) =
        let moduleName = sprintf "%sHelpers" typeName
        let camelType = toCamelCase typeName
        appendLine 0 (sprintf "module %s =" moduleName)
        appendLine 1 String.Empty
        appendLine 1 "module Meta = "

        if not <| String.IsNullOrEmpty schemaName then
            appendLine 2 <| sprintf "let schemaName = \"%s\"" schemaName

        appendLine 2 <| sprintf "let tableName = \"%s\"" tableName
        columns
        |> Seq.iter (fst >> fun c ->
            appendLine 2 <| generateColumnComment c
            appendLine 2 <| sprintf "let %s = \"%s\"" c.ColumnName c.ColumnName
        )
        appendLine 0 String.Empty

        appendLine 1 <| sprintf "let inline toRecord (%s:I%s) =" camelType typeName
        appendLine 2 "{"

        columns
        |> Seq.iter(fun (cd,measureText)->
            let _mapped = mapSqlType(cd.Type,cd.Nullable,measureText,useOptions)
            appendLine 3 (cd.ColumnName + " = " + camelType + "." + cd.ColumnName)
        )

        appendLine 2 "}"
        // start the fromF series
        appendLine 0 String.Empty

        // Convert.ToWhat? what method off the Convert class should we use? What's the overhead for using convert instead of cast? what are the advantages?
        let mapConverter(type' : string) =
            match type'.ToLower() with
                |"char"
                |"nchar"
                |"nvarchar"
                |"xml"
                |"varchar" -> "ToString"
                |"bit" -> "ToBoolean"
                // from BReusable
                |"image" -> "ToBinaryData"
                |"date"
                |"datetime"
                |"datetime2"
                |"smalldatetime" -> "ToDateTime"
                // from BReusable
                |"uniqueidentifier" -> "ToGuid" // invalid
                |"int" -> "ToInt32"
                |"decimal" -> "ToDecimal"
                |"float"  -> "ToDouble"
                |_ -> if isNull type' then String.Empty else type'

        let nonNullables = ["string";"byte[]"]

        appendLine 1 "let fromf (f:string -> obj option) = "
        appendLine 2 "{"

        columns
        |> Seq.iter(fun (cd,measureText)->
            let mapped = mapSqlType(cd.Type,cd.Nullable,measureText,useOptions)

            let converter = mapConverter(cd.Type)
            appendLine 2 (cd.ColumnName + " = ")
            appendLine 3 <| sprintf "match f \"%s\" with // %s" cd.ColumnName mapped
            let measureType = if String.IsNullOrEmpty measureText || stringEqualsI mapped "string" then String.Empty else sprintf " |> (*) 1<%s>" measureText

            if cd.Nullable && nonNullables |> Seq.exists (stringEqualsI mapped) |> not then//(mapped <> typeof<string>.Name) && equalsI mapped "string" |> not  then
                sprintf "|Some x -> x |> Convert.%s%s |> Nullable" converter measureType
            else
                sprintf "|Some x -> x |> Convert.%s%s" converter measureType
            |> appendLine 3

            let dv = getDefaultValue mapped measureText
            appendLine 3 (sprintf "|None -> %s" dv)
        )

        appendLine 2 "}"

        appendLine 0 String.Empty

        appendLine 1 "let FromF (camelTypeF:Func<string,obj option>) = fromf (Func<_>.invoke1 camelTypeF)"

        generateFromStpMethod appendLine useOptions camelType columns
        generateCreateSqlInsertTextMethod appendLine useOptions typeName schemaName tableName columns
        appendLine 0 String.Empty

    let generateINotifyClass(typeName:string, columns:ColumnDescription seq, appendLine:int -> string -> unit, _useOptions:bool ) =
        let mapFieldNameFromType(columnName:string) =
            match toCamelCase columnName with
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
        let interfaceName = sprintf "I%s" typeName

        appendLine 1 (sprintf "interface %s with" interfaceName)

        for cd in columns do
            appendLine 2 (generateColumnComment cd)
            appendLine 2 ("member x." + cd.ColumnName + " with get () = x." + cd.ColumnName)

        appendLine 1 ("interface I" + typeName + "RW with")

        for cd in columns do
            appendLine 2 (generateColumnComment cd)
            appendLine 2 ("member x." + cd.ColumnName + " with get () = x." + cd.ColumnName + " and set v = x." + cd.ColumnName + " <- v")

        appendLine 0 String.Empty
        appendLine 1 (sprintf "member x.MakeRecord () = x :> %s |> %sHelpers.toRecord" interfaceName typeName)

        appendLine 0 String.Empty

        appendLine 1 "interface INotifyPropertyChanged with"
        appendLine 2 "[<CLIEvent>]"
        appendLine 2 "member __.PropertyChanged = propertyChanged.Publish"
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
            //to consider: this might benefit from only setting/raising changed if the value is different
            appendLine 3 (camel + " <- v")
            appendLine 3 ("x.RaisePropertyChanged \"" + cd.ColumnName + "\"")

    let getSqlMeta appendLine cgsm tables fSingularizer =
        use cn = new SqlConnection(cgsm.CString)
        cn.Open()
        sprintf "Connected to %s,%s" cn.DataSource cn.Database
        |> hoist appendLine
        |> printfn "%s"
        appendLine String.Empty

        let tableData =
            tables
            |> Seq.map (getTableGenerationData appendLine fSingularizer cn)
            |> Seq.map (fun (ti, typeName, pks,columns,identities) ->
                (ti, typeName, pks, identities, columns |> Seq.map (fun c ->
                    let measureType =
                        cgsm.Measures
                        |> Seq.tryFind (fun m -> cgsm.MeasuresBlacklist |> Seq.contains c.ColumnName |> not && containsI m c.ColumnName)
                        |> Option.getOrDefault null
                    c,measureType
                ))

            )

            |> List.ofSeq
        tableData
    type SqlSprocMeta = {
        SpecificCatalog:string
        SpecificSchema:string
        SpecificName:string
        Created:DateTime
        LastAltered: DateTime
        IsDeterministic:string
        SqlDataAccess:string
        SchemaLevelRoutine:bool option
    }

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
    type SqlParameterCollection with
        member x.ToSeq() = 
            seq { for p in x -> p }
        static member ToSeq (x: SqlParameterCollection) = x.ToSeq()
    let getSqlSprocMeta cn sprocName=
        Connections.runWithConnection cn (fun cn ->
                SqlMeta.getParms (cn :?> SqlConnection) sprocName
            )
    // WIP not hardly started
    let mapSprocParams cn (appendLine:int -> string -> unit) sp = 
        let ps = 
            cn
            |> getSqlSprocMeta 
            <| sprintf "%s.%s.%s" sp.SpecificCatalog sp.SpecificSchema sp.SpecificName
            |> SqlParameterCollection.ToSeq
            |> List.ofSeq
        let mapParamName (s: string) = 
           s.[1..] 
           |> toCamelCase 

            // change mapParam over to produce output suitable to compose into this function:
//    let mapSqlType(type' : string, nullable:bool, measureType:string, useOptions:bool) =
//        match type'.ToLower() with
//            |"char"
//            |"nchar"
//            |"nvarchar"
//            |"xml"
//            |"varchar" -> "string"
//            |"bit" -> mapNullableType("bool", nullable, measureType, useOptions)
//            |"date"
//            |"datetime"
//            |"datetime2"
//            |"smalldatetime" -> mapNullableType("DateTime", nullable, measureType, useOptions)
//            |"image" -> "byte[]"
//            |"uniqueidentifier" -> mapNullableType("Guid",nullable, measureType, useOptions)
//            |"int" -> mapNullableType("int", nullable, measureType, useOptions)
//            |"decimal" -> mapNullableType("decimal", nullable, measureType, useOptions)
//            |"float" -> mapNullableType("float", nullable, measureType, useOptions)
//            |_ -> if isNull type' then String.Empty else type'
        let mapParam = 
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
            | DbType.Int64 -> "long"


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
                |> Seq.map (fun p -> sprintf "%s: %s" (p.ParameterName |> mapParamName) (p.DbType |> mapParam)) 
                |> List.ofSeq
            match memberList with
            | [] -> ()
            | x -> 
                filtered 
                |> List.map (fun p -> p.ParameterName, p.Direction)
                |> printfn "member list for %s has %i params after filter, which are %A" sp.SpecificName filtered.Length 

                x 
                |> delimit ";" 
                |> sprintf "type %sInput = {%s}" sp.SpecificName
                |> appendLine 1 
            ()

    let generateSprocComponent cn targetNamespace appendLine (ssm:CodeGenSprocSettingMap) =
        let sprocs = 
            cn |> getSqlSprocs |> Seq.sortBy (fun sp -> sp.SpecificCatalog, sp.SpecificSchema, sp.SpecificName) 
            |> Seq.filter(fun sp -> 
                ssm.SprocBlacklist |> Seq.exists (stringEqualsI sp.SpecificName) |> not
                && ssm.SprocBlacklist |> Seq.exists (stringEqualsI <| sprintf "%s.%s" sp.SpecificSchema sp.SpecificName) |> not)
            |> List.ofSeq

        printfn "Sprocs! %i" sprocs.Length

        // this would go into the default block, not a specific and we aren't currently capturing the default block to anywhere
        sprocs |> Seq.iter (fun sp ->
            printfn "%s.%s.%s" sp.SpecificCatalog sp.SpecificSchema sp.SpecificName
        )
        if sprocs.Length > 0 && not <| (sprocs |> Seq.forall(fun sp -> sp.SpecificCatalog = sprocs.[0].SpecificCatalog)) then
            failwithf "Multiple catalogs not expected"
        // consider: having this generated after the tables, and using the generated types to map sprocs that match table shapes to accept type records as arguments instead
        appendLine 0 <| sprintf "module %s.%s" targetNamespace "StoredProcedures"
        sprocs
        |> Seq.groupBy (fun sp -> sp.SpecificSchema )
        |> Seq.iter (fun (schema, schemaSprocs) ->
            appendLine 0 <| sprintf "module %s = " (toPascalCase schema)
            schemaSprocs |> Seq.iter(fun sp ->
                appendLine 1 <| sprintf "let %s = \"%s\"" sp.SpecificName sp.SpecificName
                mapSprocParams cn appendLine sp
            )

        )
    let generate generatorId (fPluralizer:string -> string) (fSingularizer:string -> string) (cgsm:CodeGenSettingMap) (manager:MacroRunner.MultipleOutputHelper.IManager, generationEnvironment:StringBuilder, tables:TableIdentifier seq) =

        log(sprintf "DataModelToF.generate:cgsm:%A" cgsm)
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
            projects |> Option.map (Seq.find (fun p -> p.GetName() = cgsm.TargetProjectName))
            |> Option.map (fun tp -> tp.GetFullName() |> Path.GetDirectoryName)

        appendLine "Projects"

        // fails on unloaded projects, probably anything else that won't give up the full name
        projects |> Option.iter (Seq.iter (fun p ->
                let fullName = 
                    try 
                        p.GetFullName()
                        |> sprintf " %s" 
                    with _ ->
                        // project may be unloaded
                        String.Empty
                appendLine' 1 (p.GetName() + fullName)
            )
        )
        appendEmpty()
        appendLine "Tables"

        tables |> Seq.iter (fun t ->
            appendLine' 1 <| sprintf "%s.%s" t.Schema t.Name
        )
        cgsm.SprocSettingMap
        |> Option.iter (fun ssm ->
            appendEmpty()
            appendLine "Sprocs"
            match targetProjectFolder with
            | Some x -> Path.Combine(x, "Sprocs.generated.fs")
            | None -> "Sprocs.generated.fs"
            |> manager.StartNewFile

            generateSprocComponent (Connector.CreateCString cgsm.CString) cgsm.TargetNamespace appendLine' ssm

            manager.EndBlock()
        )

        appendEmpty()
        getSqlMeta appendLine cgsm tables fSingularizer
        |> Seq.iter (fun (tableInfo, typeName, pks, identities, columns) ->
            let startNewFile path = manager.StartNewFile path
            let columns =
                let fIncludeColumn (c:ColumnDescription) : bool = isNull (box cgsm.ColumnBlacklist) || cgsm.ColumnBlacklist |> Map.containsKey tableInfo.Name |> not || cgsm.ColumnBlacklist.[tableInfo.Name] |> Seq.contains c.ColumnName |> not
                columns
                |> Seq.filter (fst >> fIncludeColumn)
                |> Seq.map (Tuple2.mapFst (fun c -> if identities.Contains(c.ColumnName) then {c with IsIdentity = true} else c))
                |> Seq.map (Tuple2.mapFst (fun c -> if pks |> Seq.exists (stringEqualsI c.ColumnName) then {c with IsPrimaryKey = true} else c))
                |> List.ofSeq

            let columns = columns |> List.sortBy(fst >> fun c -> c.ColumnName)
            columns
            |> Seq.map (Tuple2.mapSnd(fun m -> if String.IsNullOrEmpty m then String.Empty else sprintf "<%s>" m))
            |> Seq.map (fun (c,m) -> sprintf "%s%s" c.ColumnName m)
            |> Seq.iter (appendLine' 1)

            match targetProjectFolder with
            | Some targetProjectFolder -> Path.Combine(targetProjectFolder,tableInfo.Name + ".generated.fs")
            | None -> tableInfo.Name + ".generated.fs"
            |> startNewFile

            (
                let subNamespaceName = fPluralizer typeName
                match tableInfo.Schema, cgsm.IncludeNonDboSchemaInNamespace with
                | "dbo", _
                | _, false ->
                    appendLine <| sprintf "namespace %s.%s // Generated by %s" cgsm.TargetNamespace subNamespaceName generatorId
                | x, true ->
                    appendLine <| sprintf "namespace %s.%s.%s // Generated by %s" cgsm.TargetNamespace x subNamespaceName generatorId
            )

            appendEmpty()

            appendLine "open System"
            appendLine "open System.ComponentModel"
            appendEmpty()
            match cgsm.GetMeasureNamepace with
            | None -> ()
            | Some f ->
                columns
                |> Seq.map snd
                |> Seq.filter(String.IsNullOrWhiteSpace >> not)
                |> Seq.distinct
                |> Seq.map f
                |> Seq.distinct
                |> Seq.iter(sprintf "open %s" >> appendLine)

            cgsm.AdditionalNamespaces
            |> Seq.iter (sprintf "open %s" >> appendLine)
            appendEmpty()

            let iga = {UseOptions=cgsm.UseOptionTypes;Writeable=false}
            generateInterface (typeName, columns, appendLine', iga)
            appendEmpty()

            generateInterface (typeName, columns, appendLine', {iga with Writeable=true})
            if cgsm.GenerateValueRecords then
                generateRecord cgsm.UseCliMutable typeName (columns, appendLine', cgsm.UseOptionTypes, true)

            generateRecord cgsm.UseCliMutable typeName (columns, appendLine', cgsm.UseOptionTypes, false)
            generateModule(typeName, columns, tableInfo.Schema, tableInfo.Name, appendLine', cgsm.UseOptionTypes)
            generateINotifyClass(typeName, columns |> Seq.map fst, appendLine', cgsm.UseOptionTypes)

            manager.EndBlock()
        )

//    // purpose: make an alias to the 'generate' method that is more C# friendly
//    // would having C# to construct the type directly with null values in the delegates, then letting this translate only those be a better option?
    let Generate generatorId addlNamespaces useCliMutable (columnBlacklist:IDictionary<string, string seq>) (manager:MacroRunner.MultipleOutputHelper.IManager, generationEnvironment:StringBuilder, targetProjectName:string, tables, cString:string) useOptions generateValueRecords (measures: string seq) (measureBlacklist: string seq) includeNonDboSchemaInNamespace targetNamespace sprocSettings (pluralizer:Func<_,_>,singularizer:Func<_,_>) (getMeasureNamespace:Func<_,_>)=
        let columnBlacklist =
            columnBlacklist |> Map.ofDictionary |> Map.toSeq |> Seq.map (fun (k,v) -> KeyValuePair(k, v |> List.ofSeq))
            |> Map.ofDictionary

        let cgsm = {TargetProjectName= targetProjectName; SprocSettingMap = sprocSettings |> Option.ofUnsafeNonNullable; AdditionalNamespaces= addlNamespaces |> List.ofSeq; TargetNamespace=targetNamespace; CString=cString; UseOptionTypes=useOptions; ColumnBlacklist = columnBlacklist; Measures=measures |> List.ofSeq; MeasuresBlacklist= measureBlacklist |> List.ofSeq; IncludeNonDboSchemaInNamespace= includeNonDboSchemaInNamespace; GenerateValueRecords=generateValueRecords; UseCliMutable=useCliMutable; GetMeasureNamepace= Option.ofObj getMeasureNamespace |> Option.map (fun f -> f.Invoke) }
        generate generatorId
            pluralizer.Invoke
            singularizer.Invoke
            cgsm
            (manager, generationEnvironment, tables)

    // dbPath was Path.GetFullPath(Path.Combine(currentDir, "..", "..","PracticeManagement","Db"));
module SqlProj =
    open System.IO
    open Macros.SqlMacros
    open DataModelToF
    type TableSpecifier = {Path:string; TableGenerationInput: TableGenerationInput}

    type DbPathOption =
        |MustExist
        |FallbackToNone
    let getTables dbPath =
        Directory.GetFiles(dbPath, "*.table.sql", SearchOption.AllDirectories)
        |> Seq.map (fun tp ->
            let ti = {
                TableIdentifier.Schema = tp |> after "Schemas\\" |> before "\\"
                Name=tp |> Path.GetFileNameWithoutExtension |> Path.GetFileNameWithoutExtension
                }
            let tgi = {TableGenerationInput.Id=ti; GenerateFull = false}
            {
                Path=tp
                TableGenerationInput = tgi
            }
        )

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
        |> Option.iter (Seq.iter (fun dt ->
            let dt = dt.TableGenerationInput.Id
            sprintf "%s.%s" dt.Schema dt.Name
            |> fAppendI 1
            )
        )
        fAppendI 0 String.Empty
        let allTables =
            match dbTablesOpt with
            | Some dbTables ->
                dbTables
                |> Seq.map (fun t ->
                    let tId = t.TableGenerationInput.Id
                    let tgi = {t.TableGenerationInput with GenerateFull = tables |> Seq.contains tId.Name || tables |> Seq.contains (sprintf "%s.%s" tId.Schema tId.Name)
                    }
                    {t with TableGenerationInput = tgi})
                |> List.ofSeq
            | None ->
                tables
                |> Seq.map (fun t ->
                    let schema,table =
                        if t |> contains "." then
                            t |> before".", t |> after "."
                        else "dbo", t
                    let tId = {Schema = schema; Name=table}
                    {TableGenerationInput= {Id=tId; GenerateFull = true}; Path = null}
                )
                |> List.ofSeq
            |> Seq.filter (fun t-> blacklist |> Seq.contains t.TableGenerationInput.Id.Name |> not)
            |> Seq.filter (fun t -> generatePartials || t.TableGenerationInput.GenerateFull)
            |> List.ofSeq
        match sqlProjRootDirOpt with
        | Some _ ->
            fAppend "allTables"
            allTables
            |> Seq.iter (fun t ->
                let tId = t.TableGenerationInput.Id
                fAppendI 1 <| sprintf "%s.%s,%s" tId.Schema tId.Name t.Path
            )
        | None -> fLog <| sprintf " didn't find it at %s" sqlProjRootDir

        allTables

    let GetTableInfoFromSqlProj (log:Action<_>) (append:Action<_>) pathOption sqlProjRootDir tables blacklist generatePartials = getTableInfoFromSqlProj log.Invoke append.Invoke pathOption sqlProjRootDir tables blacklist generatePartials

// impure! applied code, meant for specific scripts, not api
module GenerationSample =
    open Macros.SqlMacros
    open SqlMeta
    open DataModelToF

    type GenerationStrategy =
        | UseMultipleOutputHelperCode
        | UseCustomManager
    let _generate generatorId connectionString =
        let sb = StringBuilder()
        let mutable currentFile:string = null
        let pluralizer = Macros.VsMacros.createPluralizer()

        let getManager strat : IManager =
            match strat with
            |UseMultipleOutputHelperCode -> // WIP - generates a file, just to the wrong dir
                upcast MacroRunner.MultipleOutputHelper.Managers.Manager(Some "DataModels.tt",sb)
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
                        override __.GetTextSize() = sb.Length
                }

        let manager = getManager UseMultipleOutputHelperCode

        generateTable false manager sb None
            {
            Id= {TableIdentifier.Name="Users"; Schema="dbo"}
            Columns=
                [
                    { // should be [PaymentID]                     int             identity primary key,
                        Name="PaymentID"
                        Type = ColumnType.Other typeof<int>
                        Attributes = ["identity";"primary key"]
                        IsUnique = false
                        AllowNull = NotNull
                        FKey = None
                        Comments = List.empty
                    }
                ]
            }
//type TableGenerationInfo = {Id:TableIdentifier; Columns: ColumnGenerationInfo list}
        let tablesToGen = [
            {TableIdentifier.Schema="dbo"; Name="Users"}
        ]
        let _results =
            let cgsm = {
                TargetProjectName= null
                TargetNamespace="Pm.Schema"
                CString=connectionString
                UseOptionTypes=false
                ColumnBlacklist = Map.empty
                Measures=List.empty
                MeasuresBlacklist= List.empty
                IncludeNonDboSchemaInNamespace= false
                GenerateValueRecords = false
                UseCliMutable = false
                GetMeasureNamepace = None
                AdditionalNamespaces = []
                SprocSettingMap = None
            }

            DataModelToF.generate generatorId pluralizer.Pluralize pluralizer.Singularize cgsm (manager, sb, tablesToGen)


        manager.GeneratedFileNames
        |> dumpt "files generated"
        |> ignore
        sb.ToString()
        |> dumpt "generated"
