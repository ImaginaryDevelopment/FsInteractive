// file focus is on creating F# types from either a coded spec, or a sql connection (with whitelist/mapping data)
namespace CodeGeneration
open System
open System.Collections.Generic
open System.Globalization
open System.Text

open BReusable
open BReusable.StringHelpers
open MacroRunner.AdoHelper
open CodeGeneration.SqlMeta.ColumnTyping

type Path = System.IO.Path
type IManager = MacroRunner.MultipleOutputHelper.IManager

module DataModelToF =
    let hoist f x = f x |> ignore; x

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
        SprocBlacklist: string Set
        SprocInputMapBlacklist: string Set
        GenerateSprocInputRecords: bool
    }
    type TypeScriptGenSettingMap = {
        TargetProjectName:string
        ColumnBlacklist:Map<string, string Set>
        TargetFolderOpt: string option
    }
    type CodeGenSettingMap = {
        TargetProjectName:string
        TargetNamespace: string
        TypeScriptGenSettingMap:TypeScriptGenSettingMap option
        CString:string
        UseOptionTypes:bool
        ColumnBlacklist:Map<string, string Set>
        // if we are generating sql tables, these are the tables we generated we don't want types generated for
        // for instance, we don't really need a type to hold a fkey+fkey table like paymentId to ReversalPaymentId
        TypeGenerationBlacklist: string Set
        Measures: string Set
        ///Needs to include any namespace that defines:
        /// - Func<_>.invoke1 ( type System.Func<'tResult> with static member invoke1<'t> (x:System.Func<'t,'tResult>) y = x.Invoke y)
        AdditionalNamespaces: string Set
        GetMeasureNamepace: (string -> string) option
        Pluralize: string -> string
        Singularize: string -> string

        MeasuresBlacklist: string Set
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



    type SqlTableColumnChoiceItem =
        |SqlTableColumnMetaItem of ColumnDescription
        |ManualItem of ColumnInput
        with
            member x.IsPrimaryKey =
                match x with
                |SqlTableColumnMetaItem cd -> cd.IsPrimaryKey
                |ManualItem x -> x.IsPrimaryKey
            member x.Name =
                match x with
                | SqlTableColumnMetaItem cd -> cd.ColumnName
                | ManualItem x -> x.Name
            member x.IsIdentity =
                match x with
                | SqlTableColumnMetaItem cd -> cd.IsIdentity
                | ManualItem x -> x.IsIdentity
            member x.Nullable =
                match x with
                | SqlTableColumnMetaItem cd -> cd.Nullable
                | ManualItem x -> match x.Nullability with Nullability.AllowNull -> true | _ -> false
            member x.Type =
                match x with
                | SqlTableColumnMetaItem cd -> cd.Type
                | ManualItem x ->
                    match x.ColumnType with
                    | Bit -> "bit"
                    | StringMax
                    | StringColumn _ -> "varchar"
                    | Custom x -> x
                    | DateTimeColumn -> "datetime"
                    | DecimalColumn _ -> "decimal"
                    | Floater _ -> "float"
                    | IntColumn
                    | IdentityColumn -> "int"
                    | NStringMax
                    | NStringColumn _ -> "nvarchar"
                    | UniqueIdentifier -> "uniqueidentifier"
            static member MapSqlType measureText useOptions (x:SqlTableColumnChoiceItem)=
                // take the sql type and give back what .net type we're using
                let type' = x.Type
                let nullable = x.Nullable
                match type'.ToLower() with
                    |"char"
                    |"nchar"
                    |"nvarchar"
                    |"xml"
                    |"varchar" -> "string"
                    |"bit" -> mapNullableType("bool", nullable, measureText, useOptions)
                    |"date"
                    |"datetime"
                    |"datetime2"
                    |"smalldatetime" -> mapNullableType("DateTime", nullable, measureText, useOptions)
                    |"image" -> "byte[]"
                    |"uniqueidentifier" -> mapNullableType("Guid",nullable, measureText, useOptions)
                    |"int" -> mapNullableType("int", nullable, measureText, useOptions)
                    |"decimal" -> mapNullableType("decimal", nullable, measureText, useOptions)
                    |"float" -> mapNullableType("float", nullable, measureText, useOptions)
                    |_ -> if isNull type' then String.Empty else type'


    type SqlTableColumnChoice =
        | SqlTableColumnMeta of ColumnDescription list
        | Manual of ColumnInput list
        with
            member x.Length =
                x |> function
                    | Manual x -> x.Length
                    | SqlTableColumnMeta x -> x.Length
            member x.Destructure() =
                x
                |> function
                    | Manual x -> x |> List.map (ManualItem)
                    | SqlTableColumnMeta x -> x |> List.map (SqlTableColumnMetaItem)

    let generateColumnComment =
        function
        | SqlTableColumnMetaItem cd ->
            let typeName = if isNull cd.Type then "null" else cd.Type
            let suffixes =
                [
                    if cd.IsIdentity then
                        yield "identity"
                    if cd.IsPrimaryKey then
                        yield "primaryKey"
                ]
            let nullability = if cd.Nullable then "null" else "not null"
            typeName,suffixes,nullability,cd.Length
        | ManualItem x ->
            let typeName = sprintf "%A" x.ColumnType
            let suffixes =
                [
                    if x.IsIdentity then
                        yield "identity"
                    if x.IsPrimaryKey then
                        yield "primaryKey"
                ]
            let nullability = match x.Nullability with | AllowNull -> "null" | _ -> "not null"
            let length =
                match x.ColumnType with
                |ColumnTyping.StringColumn x -> x
                |ColumnTyping.NStringColumn x -> x
                | _ -> 0
            typeName,suffixes, nullability,length

        >> fun (typeName,suffixes,nullability,length) ->
        //let identity = if cd.IsIdentity then " identity" else String.Empty
            let suffix = if suffixes |> Seq.any then suffixes |> delimit " " |> (+) " " else String.Empty
            sprintf "/// %s (%i) %s%s" typeName length nullability suffix

    let generateTypeComment columnCount = sprintf "/// %i properties" columnCount

    type InterfaceGeneratorArgs = { Writeable:bool; UseOptions:bool}



    let generateInterface (typeName:string, fMeasure, columns: SqlTableColumnChoice, appendLine:int -> string -> unit, interfaceGeneratorArgs ) =
        appendLine 0 <| sprintf "// typeName:%s writeable:%A useOptions:%A" typeName interfaceGeneratorArgs.Writeable interfaceGeneratorArgs.UseOptions
        appendLine 0 (generateTypeComment columns.Length)
        appendLine 0 ("type I" + typeName + (if interfaceGeneratorArgs.Writeable  then "RW" else String.Empty) + " =")
        if interfaceGeneratorArgs.Writeable then
            appendLine 1 ("inherit I" + typeName)

        columns.Destructure()
        |> Seq.iter(fun item ->
            appendLine 1 (item |> generateColumnComment )
            let measureText = fMeasure item.Name
            let mapped = SqlTableColumnChoiceItem.MapSqlType measureText interfaceGeneratorArgs.UseOptions item

            appendLine 1 <| sprintf "abstract member %s:%s with get%s" item.Name mapped (if interfaceGeneratorArgs.Writeable then ",set" else String.Empty)
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
    let generateRecord useCliMutable (typeName:string) (fMeasure:string -> string) (columns: SqlTableColumnChoice, appendLine:int -> string -> unit, useOptions:bool, generateValueRecord:bool) =
        // what is a value record?
        // - a record that is fully nullable so that all fields can indicate if a value was provided (in cases where the default value has a meaning like set the value to null)
        // - something that always uses options, instead of allowing nullables?
        // - something without the pk?
        let columns =
            columns.Destructure()
            |> fun columns ->
                if generateValueRecord then
                    columns
                    |> Seq.filter(fun cd -> not cd.IsPrimaryKey)
                    |> List.ofSeq else columns

        let recordTypeName = if generateValueRecord then sprintf "type %sValueRecord" typeName else sprintf "type %sRecord" typeName
        appendLine 0 (generateTypeComment (Seq.length columns))
        if not useOptions then
            appendLine 0 "[<NoComparison>]"
        if useCliMutable then
            appendLine 0 "[<CLIMutable>]"

        appendLine 0 <| sprintf "%s =" recordTypeName //"type " + typeName + "Record =")
        appendLine 1 "{"

        columns
        |> Seq.iter (fun x ->
            appendLine 1 <| generateColumnComment x
            let mt = fMeasure x.Name
            let mapped = SqlTableColumnChoiceItem.MapSqlType mt useOptions x
            appendLine 1 <| x.Name + ":" + mapped
        )

        appendLine 1 "}"
        if not generateValueRecord then
            appendLine 1 <| "interface I" + typeName + " with"

            columns
            |> Seq.iter (fun cd ->
                appendLine 2 <| "member x." + cd.Name + " with get () = x." + cd.Name
            )

        appendLine 0 String.Empty
        appendLine 1 "static member Zero () = "
        appendLine 2 "{"

        columns
        |> Seq.iter(fun cd ->
            let measureText = fMeasure cd.Name
            let mapped = SqlTableColumnChoiceItem.MapSqlType measureText useOptions cd
            try
                appendLine 2 (cd.Name + " = " + (getDefaultValue mapped measureText))
            with ex ->
                ex.Data.Add("mapped", mapped)
                ex.Data.Add("ColumnName",cd.Name)
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

    let generateFromStpMethod appendLine useOptions camelType fMeasure (columns: SqlTableColumnChoice)=

        appendLine 0 String.Empty

        appendLine 1 ("let inline toRecordStp (" + camelType + ": ^a) =")
        appendLine 2 "{"

        columns.Destructure()
        |> Seq.iter(fun cd ->
            let measureText = fMeasure cd.Name
            let mapped = SqlTableColumnChoiceItem.MapSqlType measureText useOptions cd
            let measureType =
                match String.IsNullOrEmpty measureText || stringEqualsI mapped "string", cd.Nullable with
                | true, _ -> String.Empty
                | false, true -> sprintf "|> Nullable.map((*) 1<%s>)" measureText
                | false, false -> sprintf " * 1<%s>" measureText

            appendLine 3 <| sprintf "%s = (^a: (member %s: _) %s)%s" cd.Name cd.Name camelType measureType
        )

        appendLine 2 "}"
        ()

    let generateCreateSqlInsertTextMethod appendLine useOptions typeName schemaName tableName fMeasure (columns:SqlTableColumnChoice) =
        let columns = columns.Destructure()
        let canDoInsert =
            columns
            |> Seq.exists(
                function
                    | ManualItem x ->
                        match x.ColumnType with
                        | Bit
                        | IntColumn
                        | IdentityColumn
                        | StringColumn _
                        | StringMax
                        | NStringMax
                        | NStringColumn _
                        | Floater _
                        | DecimalColumn _
                        | DateTimeColumn
                        | UniqueIdentifier -> false
                        | _ -> true
                    | SqlTableColumnChoiceItem.SqlTableColumnMetaItem cd -> cd.Type = "image" || cd.Type = "byte[]"
            )
            |> not
        if canDoInsert then
            appendLine 0 String.Empty
            // idea: put what running the .Zero() record against this function would generate in a comment
            let insertStart = sprintf "insert into %s.%s(%s) values (%s)" schemaName tableName
            let hasIdentity =
                columns
                |> Seq.exists (fun cd -> cd.IsIdentity)
            columns
            |> Seq.map (fun cd ->  cd.Name, SqlTableColumnChoiceItem.MapSqlType (fMeasure cd.Name) useOptions cd |> flip getDefaultValue null)
            |> List.ofSeq
            |> fun s -> s |> Seq.map fst, s |> Seq.map snd
            |> fun (names,values) ->
                insertStart (names |> delimit ",") (values |> delimit ",")
                |> sprintf "//%s"
                |> appendLine 1
            let insertMethodName = if hasIdentity then "createInsertReturnIdentity" else "createInsert"
            appendLine 1 <| sprintf "let %s blacklist (r:I%s) =" insertMethodName typeName
            let needsQuoted =
                function
                | ManualItem x ->
                    if x.ColumnType.NeedsQuoted then
                        Some  (sprintf "%A" x.ColumnType)
                    else None

                | SqlTableColumnMetaItem c ->
                    ["varchar"; "char"; "nvarchar"; "nchar";"datetime";"xml";"datetime2"]
                    |> Seq.tryFind (fun n ->
                        match c.Type with
                        |IsTrue (containsI n) -> true
                        | _ -> false
                    )

            columns
            |> Seq.filter (fun cd -> not cd.IsIdentity)
            |> Seq.choose needsQuoted
            |> Seq.tryHead
            |> function
            | Some firstTypeToNeedQuoteF ->
                firstTypeToNeedQuoteF
                |> sprintf "let quoted (s:string) = \"'\" + s.Replace(\"'\",\"''\") + \"'\" // %s"
                |> appendLine 2
            | None -> ()
            if columns |> Seq.exists(fun cd -> cd.Nullable && cd.Type.ToLower() = "bit") then
                appendLine 2 ("let inline getValue x = (^a: (member Value: bool) x)")

            let mapValue (cd:SqlTableColumnChoiceItem, prefix:string) :string  =
                let fullCName = prefix + cd.Name
                match cd.Type.ToLower() with
                    |"varchar" -> sprintf "if String.IsNullOrEmpty %s then \"null\" else quoted %s" fullCName fullCName
                    |"int" ->
                        if cd.Nullable then
                            "if isNull (box " + fullCName + ") then \"null\" else " + fullCName + " |> string"
                        else fullCName + " |> string"
                    | "bit" ->
                        if cd.Nullable then
                            sprintf "if isNull (box %s) then \"null\" elif getValue %s then string 1 else string 0" fullCName fullCName
                        else
                            sprintf "if %s then string 1 else string 0" fullCName
                    |x ->
                        if cd.Nullable then "if isNull (box " + fullCName + ") then \"null\" else " + fullCName + " |> string |> quoted" else fullCName + " |> string |> quoted"
                        |>fun setter -> sprintf "%s // type - %s" setter x

            appendLine 2 "["

            columns
            |> Seq.filter (fun c -> not c.IsIdentity)
            |> Seq.iter(fun cd->
                let mapped = "\"" + cd.Name + "\", " + mapValue(cd,"r.")
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
    let generateModule fMeasure (typeName:string, columns:SqlTableColumnChoice, schemaName:string, tableName:string, appendLine:int -> string -> unit, useOptions:bool ) =
        let moduleName = sprintf "%sHelpers" typeName
        let camelType = toCamelCase typeName
        appendLine 0 (sprintf "module %s =" moduleName)
        appendLine 1 String.Empty
        appendLine 1 "module Meta = "

        if not <| String.IsNullOrEmpty schemaName then
            appendLine 2 <| sprintf "let schemaName = \"%s\"" schemaName

        appendLine 2 <| sprintf "let tableName = \"%s\"" tableName
        let columns' = columns
        let columns = columns.Destructure()
        columns
        |> Seq.iter (fun c ->
            appendLine 2 <| generateColumnComment c
            appendLine 2 <| sprintf "let %s = \"%s\"" c.Name c.Name
        )
        appendLine 0 String.Empty

        appendLine 1 <| sprintf "let inline toRecord (%s:I%s) =" camelType typeName
        appendLine 2 "{"

        columns
        |> Seq.iter(fun cd ->
            let measureText = fMeasure cd.Name
            appendLine 3 (cd.Name + " = " + camelType + "." + cd.Name)
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
        |> Seq.iter(fun cd ->
            let measureText = fMeasure cd.Name
            let mapped = SqlTableColumnChoiceItem.MapSqlType measureText useOptions cd

            let converter = mapConverter(cd.Type)
            appendLine 2 (cd.Name + " = ")
            appendLine 3 <| sprintf "match f \"%s\" with // %s" cd.Name mapped
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

        generateFromStpMethod appendLine useOptions camelType fMeasure columns'
        generateCreateSqlInsertTextMethod appendLine useOptions typeName schemaName tableName fMeasure columns'
        appendLine 0 String.Empty

    let generateINotifyClass(typeName:string, columns: SqlTableColumnChoice, appendLine:int -> string -> unit, _useOptions:bool ) =
        let mapFieldNameFromType(columnName:string) =
            match toCamelCase columnName with
            | "type" ->  "type'"
            | camel -> camel
        appendLine 0 (generateTypeComment columns.Length)
        appendLine 0 ("type "+ typeName + "N (model:" + typeName + "Record) = ")
        appendLine 0 String.Empty
        appendLine 1 "let propertyChanged = new Event<_, _>()"
        appendLine 0 String.Empty
        let columns = columns.Destructure()
        appendLine 0 String.Empty
        for cd in columns do
            let camel = mapFieldNameFromType(cd.Name)
            appendLine 1 ("let mutable "+ camel + " = model." + cd.Name)

        appendLine 0 String.Empty
        let interfaceName = sprintf "I%s" typeName

        appendLine 1 (sprintf "interface %s with" interfaceName)
        for cd in columns do
            appendLine 2 (generateColumnComment cd)
            appendLine 2 ("member x." + cd.Name + " with get () = x." + cd.Name)

        appendLine 1 ("interface I" + typeName + "RW with")

        for cd in columns do
            appendLine 2 (generateColumnComment cd)
            appendLine 2 ("member x." + cd.Name + " with get () = x." + cd.Name + " and set v = x." + cd.Name + " <- v")

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
            let camel = mapFieldNameFromType cd.Name
            appendLine 0 String.Empty
            appendLine 1 (generateColumnComment cd)
            appendLine 1 ("member x." + cd.Name)
            appendLine 2 ("with get() = " + camel)
            appendLine 2 "and set v = "
            //to consider: this might benefit from only setting/raising changed if the value is different
            appendLine 3 (camel + " <- v")
            appendLine 3 ("x.RaisePropertyChanged \"" + cd.Name + "\"")

    type SqlTableMeta = {TI:TableIdentifier; TypeName:string; PrimaryKeys:string Set; Identities: string Set; Columns: SqlTableColumnChoice}

    let getSqlMeta appendLine cgsm tables =
        use cn = new SqlConnection(cgsm.CString)
        cn.Open()
        sprintf "Connected to %s,%s" cn.DataSource cn.Database
        |> hoist appendLine
        |> printfn "%s"
        appendLine String.Empty
        let fGenerated =
            function
            |Unhappy(ti,ex) -> Unhappy(ti,ex)
            |Happy (ti, typeName, pks,columns,identities) ->
                let result = Happy {TI=ti; TypeName= typeName; PrimaryKeys=pks; Identities=identities; Columns=SqlTableColumnChoice.SqlTableColumnMeta columns}
                result
        let tableData =
            tables
            |> Seq.map (getTableGenerationData appendLine cgsm.Singularize cn)
            |> Seq.map fGenerated
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
        member x.ToSeq() = seq { for p in x -> p }
        static member ToSeq (x: SqlParameterCollection) = x.ToSeq()
    let getSqlSprocMeta cn sprocName=
        Connections.runWithConnection cn (fun cn ->
                SqlMeta.getParms (cn :?> SqlConnection) sprocName
            )

    let mapSprocParams cn (appendLine:int -> string -> unit) sp =
        let ps =
            cn
            |> getSqlSprocMeta
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

    let generateSprocComponent cn targetNamespace appendLine (ssm:CodeGenSprocSettingMap) (startSprocFile:unit -> IDisposable) =
        let sprocs =
            cn |> getSqlSprocs |> Seq.sortBy (fun sp -> sp.SpecificCatalog, sp.SpecificSchema, sp.SpecificName)
            |> Seq.filter(fun sp ->
                ssm.SprocBlacklist |> Seq.exists (stringEqualsI sp.SpecificName) |> not
                && ssm.SprocBlacklist |> Seq.exists (stringEqualsI <| sprintf "%s.%s" sp.SpecificSchema sp.SpecificName) |> not)
            |> List.ofSeq

        printfn "Sprocs! %i" sprocs.Length
        if sprocs.Length < 1 then
            ()
        else
            // goes into the default block, not a specific
            sprocs |> Seq.iter (fun sp ->
                appendLine 1 <| sprintf "%s.%s.%s" sp.SpecificCatalog sp.SpecificSchema sp.SpecificName
            )
            use __ = startSprocFile() // start the specific sproc file
            if sprocs.Length > 0 && not <| (sprocs |> Seq.forall(fun sp -> sp.SpecificCatalog = sprocs.[0].SpecificCatalog)) then
                failwithf "Multiple catalogs not expected"
            // consider: having this generated after the tables, and using the generated types to map sprocs that match table shapes to accept type records as arguments instead
            appendLine 0 <| sprintf "module %s.%s" targetNamespace "StoredProcedures"
            appendLine 0 "open System"
            sprocs
            |> Seq.groupBy (fun sp -> sp.SpecificSchema)
            |> Seq.iter (fun (schema, schemaSprocs) ->
                appendLine 0 <| sprintf "module %s = " (toPascalCase schema)
                schemaSprocs |> Seq.iter(fun sp ->
                    // the next line is at least partially because there is no nameof operator, but also, because even if there were, sprocNames wouldn't be somewhere you could use it
                    appendLine 1 <| sprintf "let %s = \"%s\"" sp.SpecificName sp.SpecificName
                    if ssm.SprocInputMapBlacklist |> Seq.exists (fun x -> x = sp.SpecificName || x = (sprintf "%s.%s" sp.SpecificSchema sp.SpecificName)) |> not then
                        mapSprocParams cn appendLine sp
                )

            )
    let generate generatorId (fMetaFallbackOpt) (cgsm:CodeGenSettingMap) (manager:MacroRunner.MultipleOutputHelper.IManager, generationEnvironment:StringBuilder, tables:TableIdentifier seq) =

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
            let fStartSprocFile () =
                match targetProjectFolder with
                    | Some x -> Path.Combine(x, "Sprocs.generated.fs")
                    | None -> "Sprocs.generated.fs"
                |> manager.StartNewFile

                {
                    new IDisposable with
                        member __.Dispose() = manager.EndBlock()
                }

            generateSprocComponent (Connector.CreateCString cgsm.CString) cgsm.TargetNamespace appendLine' ssm fStartSprocFile

        )

        appendEmpty()
        let meta = getSqlMeta appendLine cgsm tables
        meta
        |> List.collect( fun result ->
            match result, fMetaFallbackOpt with
            | Happy (sqlTableMeta), _ ->
                sqlTableMeta
            | Unhappy (ti,exn), Some f ->
                f ti exn
                |> function
                    | None -> raise <| InvalidOperationException("Failed to get sql data",exn)
                    | Some x -> x
            | Unhappy(_,exn), None ->
                raise <| InvalidOperationException("Failed to get sql data",exn)
            |> (fun sqlTableMeta ->

                let startNewFile path = manager.StartNewFile path
                let columns =
                    sqlTableMeta.Columns
                    |> function
                        |SqlTableColumnChoice.SqlTableColumnMeta columns ->
                            let fIncludeColumn (c:ColumnDescription) : bool = isNull (box cgsm.ColumnBlacklist) || cgsm.ColumnBlacklist |> Map.containsKey sqlTableMeta.TI.Name |> not || cgsm.ColumnBlacklist.[sqlTableMeta.TI.Name] |> Seq.contains c.ColumnName |> not
                            columns
                            |> Seq.filter fIncludeColumn
                            |> Seq.map (fun cd ->
                                if sqlTableMeta.Identities |> Seq.exists (stringEqualsI cd.ColumnName) then
                                    {cd with IsIdentity = true}
                                else
                                    cd
                            )
                            |> Seq.map (fun cd ->
                                if sqlTableMeta.PrimaryKeys |> Seq.exists (stringEqualsI cd.ColumnName) then
                                    {cd with IsPrimaryKey = true}
                                else cd
                            )
                            |> List.ofSeq
                            |> SqlTableColumnChoice.SqlTableColumnMeta
                        |SqlTableColumnChoice.Manual columns ->
                            columns
                            |> Seq.filter(fun c -> isNull (box cgsm.ColumnBlacklist) || cgsm.ColumnBlacklist |> Map.containsKey c.Name |> not)
                            |> List.ofSeq
                            |> SqlTableColumnChoice.Manual

                //if sqlTableMeta.TI.Name = "Account" then
                //    Debugger.Launch() |> ignore

                let getMeasureType (columnName) =
                        cgsm.Measures
                        |> Seq.tryFind (fun m -> cgsm.MeasuresBlacklist |> Seq.contains columnName |> not && containsI m columnName)
                        |> Option.getOrDefault null

                let columns =
                    columns
                    |> function
                    | SqlTableColumnMeta items -> items|> List.sortBy(fun cd -> cd.ColumnName) |> SqlTableColumnMeta
                    | SqlTableColumnChoice.Manual items -> items |> List.sortBy(fun x -> x.Name) |> SqlTableColumnChoice.Manual
                columns
                |> function
                    | SqlTableColumnMeta items ->
                        items |> Seq.map (fun cd -> cd.ColumnName)
                    | SqlTableColumnChoice.Manual items ->
                        items |> Seq.map (fun c -> c.Name )
                    |> Seq.map (fun name ->
                        match getMeasureType name with
                        | ValueString as m -> sprintf "<%s>" m
                        | _ -> String.Empty
                        |> sprintf "%s%s" name
                    )
                    |> Seq.iter (appendLine' 1)


                match targetProjectFolder with
                | Some targetProjectFolder -> Path.Combine(targetProjectFolder,sqlTableMeta.TI.Name + ".generated.fs")
                | None -> sqlTableMeta.TI.Name + ".generated.fs"
                |> startNewFile

                (
                    let subNamespaceName = cgsm.Pluralize sqlTableMeta.TypeName
                    match sqlTableMeta.TI.Schema, cgsm.IncludeNonDboSchemaInNamespace with
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
                    |> function
                        | SqlTableColumnChoice.SqlTableColumnMeta items ->
                            items |> Seq.map (fun cd -> cd.ColumnName)
                        | SqlTableColumnChoice.Manual items ->
                            items |> Seq.map (fun x -> x.Name)
                        |> Seq.map getMeasureType
                    |> Seq.filter(String.IsNullOrWhiteSpace >> not)
                    |> Seq.distinct
                    |> Seq.map f
                    |> Seq.distinct
                    |> Seq.iter(sprintf "open %s" >> appendLine)

                cgsm.AdditionalNamespaces
                |> Seq.iter (sprintf "open %s" >> appendLine)
                appendEmpty()

                let iga = {UseOptions=cgsm.UseOptionTypes;Writeable=false}
                let tn = sqlTableMeta.TypeName
                generateInterface (tn, getMeasureType, columns, appendLine', iga)
                appendEmpty()

                generateInterface (tn, getMeasureType,columns, appendLine', {iga with Writeable=true})
                if cgsm.GenerateValueRecords then
                    generateRecord cgsm.UseCliMutable tn getMeasureType (columns, appendLine', cgsm.UseOptionTypes, true)

                generateRecord cgsm.UseCliMutable tn getMeasureType (columns, appendLine', cgsm.UseOptionTypes, false)
                generateModule getMeasureType (tn, columns, sqlTableMeta.TI.Schema, sqlTableMeta.TI.Name, appendLine', cgsm.UseOptionTypes)
                generateINotifyClass(tn, columns, appendLine', cgsm.UseOptionTypes)

                manager.EndBlock()
            )
            meta
            |> List.choose(
                function
                |Happy x -> Some x
                |Unhappy (ti,ex) -> 
                    printfn "Bad time generating %s.%s exception: %A" ti.Schema ti.Name ex
                    None
            )
        )


//    // purpose: make an alias to the 'generate' method that is more C# friendly
//    // would having C# to construct the type directly with null values in the delegates, then letting this translate only those be a better option?
    let Generate fFallback generatorId addlNamespaces useCliMutable (columnBlacklist:IDictionary<string, string seq>) (manager:MacroRunner.MultipleOutputHelper.IManager, generationEnvironment:StringBuilder, targetProjectName:string, tables, cString:string) useOptions generateValueRecords (measures: string seq) (measureBlacklist: string seq) includeNonDboSchemaInNamespace targetNamespace sprocSettings (pluralizer:Func<_,_>,singularizer:Func<_,_>) (getMeasureNamespace:Func<_,_>) typeGenerationBlacklist =
        let columnBlacklist =
            columnBlacklist |> Map.ofDictionary |> Map.toSeq |> Seq.map (fun (k,v) -> KeyValuePair(k, v |> Set.ofSeq))
            |> Map.ofDictionary

        let cgsm = {    TargetProjectName= targetProjectName
                        SprocSettingMap = sprocSettings |> Option.ofUnsafeNonNullable
                        AdditionalNamespaces= addlNamespaces |> Set.ofSeq
                        TargetNamespace=targetNamespace
                        TypeScriptGenSettingMap = None
                        CString=cString
                        UseOptionTypes=useOptions
                        ColumnBlacklist = columnBlacklist
                        Measures=measures |> Set.ofSeq
                        MeasuresBlacklist= measureBlacklist |> Set.ofSeq
                        IncludeNonDboSchemaInNamespace= includeNonDboSchemaInNamespace
                        GenerateValueRecords=generateValueRecords
                        UseCliMutable=useCliMutable
                        Pluralize= pluralizer.Invoke
                        Singularize= singularizer.Invoke
                        TypeGenerationBlacklist = typeGenerationBlacklist |> Set.ofSeq
                        GetMeasureNamepace= Option.ofObj getMeasureNamespace |> Option.map (fun f -> f.Invoke) }
        generate generatorId
            fFallback
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
    open SqlMeta.ColumnTyping

    type GenerationStrategy =
        | UseMultipleOutputHelperCode
        | UseCustomManager
    let _generate generatorId pluralize singularize connectionString =
        let sb = StringBuilder()
        let mutable currentFile:string = null
        //let pluralizer = Macros.VsMacros.createPluralizer()

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
                        ColumnType = ColumnType.IdentityColumn
                        IsUnique = NotUnique
                        Nullability = PrimaryKey
                        FKey = None
                        Comments = List.empty
                        DefaultValue=null
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
                TypeScriptGenSettingMap = None
                CString=connectionString
                UseOptionTypes=false
                ColumnBlacklist = Map.empty
                Measures=Set.empty
                MeasuresBlacklist= Set.empty
                IncludeNonDboSchemaInNamespace= false
                GenerateValueRecords = false
                UseCliMutable = false
                GetMeasureNamepace = None
                Pluralize = pluralize
                Singularize = singularize
                AdditionalNamespaces = Set.empty
                TypeGenerationBlacklist = Set.empty
                SprocSettingMap = None
            }

            DataModelToF.generate generatorId None cgsm (manager, sb, tablesToGen)


        manager.GeneratedFileNames
        |> dumpt "files generated"
        |> ignore
        sb.ToString()
        |> dumpt "generated"
