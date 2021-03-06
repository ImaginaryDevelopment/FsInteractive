﻿// file focus is on creating F# types from either a coded spec, or a sql connection (with whitelist/mapping data)
namespace CodeGeneration
open System
open System.Collections.Generic
open System.Globalization
open System.Text

open global.BReusable
open global.BReusable.StringHelpers


type Path = System.IO.Path

module DataModelToF =

    open System.Diagnostics
    open PureCodeGeneration
    open BCore.CodeGeneration.SqlWrapCore
    open SqlScriptGeneration
    open ColumnTyping
    open BCore.CodeGeneration.DteWrapCore

    let private log (s:string) =
        let text = if s.EndsWith "\r\n" then s else sprintf "%s\r\n" s
        printfn "%s" text
        if System.Diagnostics.Debugger.IsAttached then
            System.Diagnostics.Debugger.Log(0, "Logger", text)

    type TableGenerationInput = {Id: global.BCore.CodeGeneration.SqlWrapCore.TableIdentifier; GenerateFull:bool}

    type CodeGenSprocSettingMap = {
        SprocNolist: string Set
        SprocInputMapNolist: string Set
        GenerateSprocInputRecords: bool
    }
    type TypeScriptGenSettingMap = {
        /// the Name of the Project : compared against the DteWrapper if present to find the project's folder
        TargetProjectName:string
        ColumnNolist:Map<string, string Set>
        TargetFolderOpt: string option
    }
    // we need to break this up into DUs around which generation features are desired
    [<NoComparison;NoEquality>]
    type CodeGenSettingMap = {
        /// the Name of the Project : compared against the DteWrapper if present to find the project's folder
        TargetProjectName:string
        /// what namespace each generated file should generate into
        TargetNamespace: string
        /// if TypeScript definitions are desired, provide settings here
        TypeScriptGenSettingMap:TypeScriptGenSettingMap option
        /// if a Sql connection will be used to get additional data from the database about a type, the connection string goes here
        CString:string
        /// controls if Option or Nullable should be used in records and classes
        NullableHandling:NullableHandling
        /// a Map of table name to columns that should not be included in generation
        ColumnNolist:Map<string, string Set>
        /// if we are generating sql tables, these are the tables we generated we don't want types generated for
        /// for instance, we don't really need a type to hold a fkey+fkey table like paymentId to ReversalPaymentId
        TypeGenerationNolist: string Set
        /// a list of column 'names' that should become measures
        /// for example if Set["PatientId"] any type that has a column name containing PatientId that column/property will produce/consume the measure
        Measures: string Set
        /// a block list of column/property names that match a measure, but should not be included
        MeasuresNolist: string Set
        /// this is the list of any extra namespaces you want included
        /// needs to include any namespace that defines:
        /// - Func.invoke1 ( module Func = let invoke1 (x:System.Func<_,_>) y = x.Invoke y)
        AdditionalNamespaces: string Set
        /// if provided is used to determine the namespace that should be opened for a particular measure
        /// so if Schema.PatientId was a measure then it would return Schema if given PatientId
        GetMeasureNamepace: (string -> string) option
        //Pluralize and Singularize can be provided by 
        //  - System.Data.Entity.Design.dll
        //  - Macros.VsMacros.createPluralizer()
        /// this method should take a singular noun or type name, and return a pluralized version
        /// for example: "Patient" -> "Patients"
        Pluralize: string -> string
        /// this method should take a plural noun or type name, and return a singularized version
        /// for example: "Patients" -> "Patient"
        Singularize: string -> string

        /// this tells the generator whether it should include the schema in a type's generation
        /// for instance if you had a table Accounts.Account should Accounts be part of the namespace?
        IncludeNonDboSchemaInNamespace:bool
        /// if set GenerateRecords that do not include the primary key
        GenerateValueRecords:bool
        /// Additional Setting options for Sproc name/argument generation
        SprocSettingMap: CodeGenSprocSettingMap option
        /// controls whether generated records are mutable
        Mutable:Mutability
    }

    // nullableAndHandling expects a Some _ if the type is nullable
    let mapNullableType(targetType:string, nullableAndHandling:NullableHandling option, measureType:PureMeasure option) =
        let nullability =
            match nullableAndHandling with
            | Some UseOptions -> " option"
            | Some UseNullable -> " Nullable"
            | None -> String.Empty
        let measureType =
            match measureType with
            | None -> String.Empty
            | Some pm -> sprintf "<%s>" pm.Value

        sprintf "%s%s%s" targetType measureType nullability

    // this needs to DIAF -> the generation code should not have direct dependencies on a sql implementation
    [<NoComparison>]
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
            member x.IsComputed =
                match x with
                | SqlTableColumnMetaItem cd -> cd.IsComputed
                | ManualItem x -> x.IsComputed
            member x.Nullable =
                match x with
                | SqlTableColumnMetaItem cd -> cd.Nullable
                | ManualItem x -> match x.Nullability with Nullability.AllowNull -> true | _ -> false
            member x.SqlType =
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
            static member GetNetType (x:SqlTableColumnChoiceItem) nullableAndHandling (pm:PureMeasure option) =
                let type' = x.SqlType
                match type'.ToLower() with
                    |"char"
                    |"nchar"
                    |"nvarchar"
                    |"xml"
                    |"varchar" -> "string"
                    |"bit" -> mapNullableType("bool", nullableAndHandling, pm)
                    |"date"
                    |"datetime"
                    |"datetime2"
                    |"smalldatetime" -> mapNullableType("DateTime", nullableAndHandling, pm)
                    |"image" -> "byte[]"
                    |"uniqueidentifier" -> mapNullableType("Guid",nullableAndHandling, pm)
                    |"int" -> mapNullableType("int", nullableAndHandling, pm)
                    |"decimal" -> mapNullableType("decimal", nullableAndHandling, pm)
                    |"float" -> mapNullableType("float", nullableAndHandling, pm)
                    |_ -> if isNull type' then String.Empty else type'
            // do not include nullable or option part of type, no measures either
            member x.NetBaseType = SqlTableColumnChoiceItem.GetNetType x None None
            member x.GetNetFullType useOptions measureText = SqlTableColumnChoiceItem.GetNetType x (if x.Nullable then Some useOptions else None) measureText
            static member MapSqlType (pm:PureMeasure option) useOptions (x:SqlTableColumnChoiceItem)=
                let n = if x.Nullable then Some useOptions else None
                // take the sql type and give back what .net type we're using
                let type' = x.SqlType
                let result =
                    match type'.ToLower() with
                        |"char"
                        |"nchar"
                        |"nvarchar"
                        |"xml"
                        |"varchar" -> "string"
                        |"bit" -> mapNullableType("bool", n, pm)
                        |"date"
                        |"datetime"
                        |"datetime2"
                        | "System.DateTime"
                        |"smalldatetime" -> mapNullableType("DateTime", n, pm)
                        |"image" -> "byte[]"
                        |"uniqueidentifier" -> mapNullableType("Guid",n, pm)
                        |"int" -> mapNullableType("int", n, pm)
                        |"decimal" -> mapNullableType("decimal", n, pm)
                        |"float" -> mapNullableType("float", n, pm)
                        |_ -> if isNull type' then String.Empty else type'
                result


    [<NoComparison>]
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

    let toPurish fMeasure (columns:SqlTableColumnChoice) =
        columns.Destructure()
        |> List.map(fun cd ->
            let measure :PureMeasure option = fMeasure cd.Name
            let colName =
                //let raw = SqlTableColumnChoiceItem.MapSqlType measure useOptions cd
                cd.NetBaseType

                |> PureColumnTypeName.Create
                |> function
                    | Some x -> x
                    | None -> failwithf "couldn't get column name from %s:%s" cd.Name cd.SqlType

            {   new PureColumnInput<_> with
                    member __.IsPrimaryKey = cd.IsPrimaryKey
                    member __.Item = cd
                    member __.Name = cd.Name
                    member __.TypeName = colName
                    member __.AllowsNull = cd.Nullable
                    member __.MeasureText = measure
                    member __.IsWriteable = not cd.IsComputed
            }
        )
    let generateColumnSqlComment = // (ci:ColumnInput) =
        function
        | SqlTableColumnMetaItem cd ->
            let typeName = if isNull cd.Type then "null" else cd.Type
            let suffixes =
                [
                    if cd.IsIdentity then
                        yield "identity"
                    if cd.IsPrimaryKey then
                        yield "primaryKey"
                    if cd.IsComputed then
                        yield "computed"
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
                    if x.IsComputed then
                        yield "computed"
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
            sprintf "// %s (%i) %s%s" typeName length nullability suffix


    let generateInterface (typeName:string, fMeasure, columns: SqlTableColumnChoice, appendLine:int -> string -> unit, interfaceGeneratorArgs ) =
        generateInterface (fun c -> generateColumnSqlComment c.Item) (fun pm useOptions c -> SqlTableColumnChoiceItem.MapSqlType pm useOptions c.Item) (typeName, fMeasure, columns |> toPurish fMeasure, appendLine, interfaceGeneratorArgs)

    let generateFromStpMethod appendLine camelType fMeasure (columns: SqlTableColumnChoice) =
        columns
        |> toPurish fMeasure
        |> generateFromSrtpMethod appendLine camelType fMeasure
        ()

    let generateCreateSqlInsertTextMethod appendLine typeName schemaName tableName (columns:SqlTableColumnChoice) =
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

            let insertStart = sprintf "insert into %s.%s(%s) values" schemaName tableName
            let hasIdentity =
                columns
                |> Seq.exists (fun cd -> cd.IsIdentity)
            // idea: put what running the .Zero() record against this function would generate in a comment
            (
                let mapDefaultRawValue handleQuoting (cd:SqlTableColumnChoiceItem) :string  =
                    if cd.Nullable then "null"
                    else
                        match cd.SqlType.ToLower() with
                        |"varchar" -> if handleQuoting then "''" else String.Empty
                        |"bit"
                        |"int" -> "0"
                        | _ ->
                            cd.Name
                columns
                |> Seq.filter(fun cd -> cd.IsComputed |> not)
                |> Seq.map (fun cd ->  cd.Name, cd |> mapDefaultRawValue true)
                |> List.ofSeq
                |> fun s -> s |> Seq.map fst, s |> Seq.map snd
                |> fun (names,values) ->
                    insertStart (names |> delimit ",")
                    |> sprintf "// %s"
                    |> appendLine 1
                    (values |> delimit ",")
                    |> sprintf "// (%s)"
                    |> appendLine 1
            )
            let insertMethodName = if hasIdentity then "createInsertReturnIdentity" else "createInsert"
            appendLine 0 <| sprintf "let %s nolist (r:I%s) =" insertMethodName typeName
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
                        |ContainsI n _ -> true
                        | _ -> false
                    )

            columns
            |> Seq.filter (fun cd -> not cd.IsIdentity && not cd.IsComputed)
            |> Seq.choose needsQuoted
            |> Seq.tryHead
            |> function
            | Some firstTypeToNeedQuoteF ->
                firstTypeToNeedQuoteF
                |> sprintf "let quoted (s:string) = \"'\" + s.Replace(\"'\",\"''\") + \"'\" // %s"
                |> appendLine 1
            | None -> ()
            if columns |> Seq.exists(fun cd -> cd.Nullable && cd.SqlType.ToLower() = "bit") then
                appendLine 1 ("let inline getValue x = (^a: (member Value: bool) x)")

            let mapValue (cd:SqlTableColumnChoiceItem, prefix:string) :string  =
                let fullCName = prefix + cd.Name
                match cd.SqlType.ToLower() with
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
                        |> fun setter -> sprintf "%s // type - %s" setter x

            appendLine 1 "["

            columns
            |> Seq.filter (fun c -> not c.IsIdentity && not c.IsComputed)
            |> Seq.iter(fun cd ->
                let mapped = "\"" + cd.Name + "\", " + mapValue(cd,"r.")
                appendLine 2 mapped
            )

            appendLine 1 "]"
            appendLine 1 <| "|> Seq.filter (fun kvp -> nolist |> Seq.contains (fst kvp) |> not)"
            appendLine 1 <| sprintf "|> fun pairs -> sprintf \"%s (%%s)\" (String.Join(\",\", pairs |> Seq.map fst )) (String.Join(\",\", pairs |> Seq.map snd))" (insertStart "%s")
            // this should not be happening if there is no identity on the table
            if hasIdentity then
                appendLine 1 <| "|> sprintf \"%s;select SCOPE_IDENTITY()\""
        ()

    /// generate the helper module
    let generateHelperModule fMeasure (typeName:string, columns:SqlTableColumnChoice, schemaName:string, tableName:string, appendLine:int -> string -> unit, useOptions:NullableHandling):unit=
        let igh = {new IGenerateHelper<SqlTableColumnChoiceItem> with
                       member __.AppendLine indentations text = appendLine indentations text
                       member __.GetFullType = (fun useOptions item -> SqlTableColumnChoiceItem.MapSqlType item.MeasureText useOptions item.Item)
                       member __.GetMeasureForColumnName(columnName) = fMeasure columnName
                       member __.MakeColumnComments = fun (item:PureColumnInput<SqlTableColumnChoiceItem>) -> generateColumnSqlComment item.Item
        }
        let purishColumns = toPurish igh.GetMeasureForColumnName columns
        let fOtherHelpers =
            fun apline -> generateCreateSqlInsertTextMethod apline typeName schemaName tableName columns
        generateHelperModule igh useOptions [sprintf "let schemaName = \"%s\"" schemaName; sprintf "let tableName = \"%s\"" tableName] (typeName, purishColumns) fOtherHelpers

    let generateINotifyClassSql fMeasure (notifyOptions,typeName:string, columns: SqlTableColumnChoice, appendLine:int -> string -> unit) =
        generateINotifyClass (fun c -> generateColumnSqlComment c.Item) (fun c -> not c.Item.IsComputed) (notifyOptions, typeName, columns |> toPurish fMeasure, appendLine)

    [<NoComparison>]
    type SqlTableMeta = {TI:TableIdentifier; TypeName:string; PrimaryKeys:string Set; Identities: string Set; Columns: SqlTableColumnChoice}

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


    let generateSprocComponent sqlSprocs fMapSprocParams targetNamespace appendLine (ssm:CodeGenSprocSettingMap) (startSprocFile:unit -> IDisposable) =
        let sprocs =
            sqlSprocs
            |> Seq.sortBy (fun sp -> sp.SpecificCatalog, sp.SpecificSchema, sp.SpecificName)
            |> Seq.filter(fun sp ->
                ssm.SprocNolist |> Seq.exists (stringEqualsI sp.SpecificName) |> not
                && ssm.SprocNolist |> Seq.exists (stringEqualsI <| sprintf "%s.%s" sp.SpecificSchema sp.SpecificName) |> not)
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
                appendLine 0 <| sprintf "module %s =" (toPascalCase schema)
                schemaSprocs |> Seq.iter(fun sp ->
                    // the next line is at least partially because there is no nameof operator, but also, because even if there were, sprocNames wouldn't be somewhere you could use it
                    appendLine 1 <| sprintf "let %s = \"%s\"" sp.SpecificName sp.SpecificName
                    if ssm.SprocInputMapNolist |> Seq.exists (fun x -> x = sp.SpecificName || x = (sprintf "%s.%s" sp.SpecificSchema sp.SpecificName)) |> not then
                        fMapSprocParams sp
                        |> Option.iter (appendLine 1)
                )

            )
    let getMeasureType measures measuresNoList columnName =
            measures
            |> Seq.tryFind (fun m -> measuresNoList |> Seq.contains columnName |> not && containsI m columnName)
            // fail if there is a measure, but it is invalid
            |> Option.map (fun m -> PureMeasure.create m |> Option.getOrFailMsg (sprintf "%s is not a valid measure" m))

    let generateFFile (appendLine',appendLine,appendEmpty) generatorId cgsm sqlTableMeta ncOpts =
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
        let inline getMeasureType x = getMeasureType cgsm.Measures cgsm.MeasuresNolist x
        match cgsm.GetMeasureNamepace with
        | None -> ()
        | Some f ->
            sqlTableMeta.Columns
            |> function
                | SqlTableColumnChoice.SqlTableColumnMeta items ->
                    items |> Seq.map (fun cd -> cd.ColumnName)
                | SqlTableColumnChoice.Manual items ->
                    items |> Seq.map (fun x -> x.Name)
                |> Seq.map getMeasureType
            |> Seq.choose id
            |> Seq.distinct
            |> Seq.map (fun x -> f x.Value)
            |> Seq.distinct
            |> Seq.iter(sprintf "open %s" >> appendLine)

        cgsm.AdditionalNamespaces
        |> Seq.iter (sprintf "open %s" >> appendLine)
        appendEmpty()

        let iga = {UseOptions=cgsm.NullableHandling;Writeable=false}
        let tn = sqlTableMeta.TypeName
        generateInterface (tn, getMeasureType, sqlTableMeta.Columns, appendLine', iga)
        appendEmpty()

        generateInterface (tn, getMeasureType,sqlTableMeta.Columns, appendLine', {iga with Writeable=true})
        let genIgr =
            {   new IGenerateRecords<SqlTableColumnChoiceItem> with
                    //type GetBaseTypeTextDelegate<'T> = MeasureText -> UseOptions -> PureColumnInput<'T> -> string
                    member __.GetFullType :GetFullTypeTextDelegate<_> = (fun  useOptions item -> SqlTableColumnChoiceItem.MapSqlType item.MeasureText useOptions item.Item)
                    member __.MakeColumnComments = fun  (item:PureColumnInput<SqlTableColumnChoiceItem>) -> generateColumnSqlComment item.Item
                    member __.AppendLine i txt = appendLine' i txt
                    // should include measure and nullable when needed
                    member x.GetDefaultValueForType c = x.GetFullType cgsm.NullableHandling c |> getDefaultValue c.MeasureText
                    member __.GetMeasureForColumnName x = getMeasureType x
            }

        let purishColumns = toPurish getMeasureType sqlTableMeta.Columns

        if cgsm.GenerateValueRecords then
            generateRecord genIgr tn cgsm.Mutable cgsm.NullableHandling true purishColumns //cgsm.Mutable tn getMeasureType (columns, appendLine', cgsm.UseOptionTypes, true)

        generateRecord genIgr tn cgsm.Mutable cgsm.NullableHandling false purishColumns
        generateHelperModule getMeasureType (tn, sqlTableMeta.Columns, sqlTableMeta.TI.Schema, sqlTableMeta.TI.Name, appendLine', cgsm.NullableHandling)
        let columns = toPurish genIgr.GetMeasureForColumnName sqlTableMeta.Columns
        generateClass genIgr.MakeColumnComments (fun c -> c.IsWriteable) (tn, columns,appendLine')
        appendEmpty()
        //generateINotifyClass getMeasureType (tn, columns, appendLine')
        generateINotifyClass (fun c -> generateColumnSqlComment c.Item) (fun c -> not c.Item.IsComputed) (ncOpts,tn, columns, appendLine')

    ()
    type GenerationArguments<'exn when 'exn :> exn> =
        {
            //MetaFallback:(GenMapTableItem seq -> TableIdentifier-> 'exn->SqlTableMeta) option
            // this takes an appendline with indentation level already closed over
            GetSqlMeta:(string->unit) -> CodeGenSettingMap -> TableIdentifier seq -> Rail<SqlTableMeta,TableIdentifier * 'exn> list
            SqlSprocs: SqlSprocMeta seq
            // Default if the CodeGeneration.Sql project is referenced is mapSprocParams
            MapSqlSprocParams: SqlSprocMeta -> string option
        }
        with
            // CSharp creation syntax
            static member Create (getSqlMeta, sqlSprocs, mapSqlSprocParams) =
                let guardArg name x = if x |> isNull then invalidArg name "must not be null"
                guardArg "getSqlMeta" getSqlMeta
                guardArg "mapSqlSprocParams" mapSqlSprocParams
                let inline getSqlMeta appendLine cgsm tis =
                    Func.invoke3 getSqlMeta (Action<_> appendLine) cgsm tis
                let inline mapSqlSprocParams meta =
                    let result = Func.invoke1 mapSqlSprocParams meta
                    match result with
                    | ValueString x -> Some x
                    | _ -> None
                {SqlSprocs=sqlSprocs;GetSqlMeta=getSqlMeta; MapSqlSprocParams=mapSqlSprocParams}

    let writeColumnInfoMeta cgsm appendLine' sqlTableMeta =
        let columns =
            sqlTableMeta.Columns
            |> function
                |SqlTableColumnChoice.SqlTableColumnMeta columns ->
                    let fIncludeColumn (c:ColumnDescription) : bool = isNull (box cgsm.ColumnNolist) || cgsm.ColumnNolist |> Map.containsKey sqlTableMeta.TI.Name |> not || cgsm.ColumnNolist.[sqlTableMeta.TI.Name] |> Seq.contains c.ColumnName |> not
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
                    |> Seq.filter(fun c -> isNull (box cgsm.ColumnNolist) || cgsm.ColumnNolist |> Map.containsKey c.Name |> not)
                    |> List.ofSeq
                    |> SqlTableColumnChoice.Manual

        //if sqlTableMeta.TI.Name = "Account" then
        //    Debugger.Launch() |> ignore

        let inline getMeasureType x = getMeasureType cgsm.Measures cgsm.MeasuresNolist x

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
                | Some pm -> sprintf "<%s>" pm.Value
                | _ -> String.Empty
                |> sprintf "%s%s" name
            )
            |> Seq.iter (appendLine' 1)


    let writeGenerationMeta (appendLine',appendLine,appendEmpty) templateFile projects (tables:TableIdentifier seq) =
        appendEmpty()
        appendLine <| sprintf "TemplateFile: %s" templateFile
        appendEmpty()

        //let sol,projects = Macros.VsMacros.getSP dte //EnvDteHelper.recurseSolutionProjects dte

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

    // generate a file that has fields with the names of all sprocs found in the db, and an input type fitting their shape
    let generateSprocMetaFile cgsm (appendLine',appendLine,appendEmpty) targetProjectFolder (manager:IManager) sprocs mapSqlSprocParams=
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

            generateSprocComponent sprocs mapSqlSprocParams cgsm.TargetNamespace appendLine' ssm fStartSprocFile
        )

    let generate generatorId (ga:GenerationArguments<_>) (cgsm:CodeGenSettingMap) (manager:IManager, generationEnvironment:StringBuilder, tables:TableIdentifier seq) fSettersCheckInequality =

        log(sprintf "DataModelToF.generate:cgsm:%A" cgsm)
        let appendLine text =
            match text with
            | ValueString text -> generationEnvironment.AppendLine text |> ignore
            | _ -> generationEnvironment.AppendLine String.Empty |> ignore
        let appendEmpty() = appendLine String.Empty
        let appendLine' indentLevels text =
            let indentation = String.replicate indentLevels "    "
            generationEnvironment.AppendLine(indentation + text) |> ignore
        let projects = manager.DteWrapperOpt |> Option.map (fun dte -> dte.GetProjects()) //(Macros.VsMacros.getSP>>snd) // was dte
        let targetProjectFolder =
            projects |> Option.map (Seq.find (fun p -> p.GetName() = cgsm.TargetProjectName))
            |> Option.map (fun tp -> tp.GetFullName() |> Path.GetDirectoryName)
        writeGenerationMeta (appendLine', appendLine,appendEmpty) manager.TemplateFile projects tables
        generateSprocMetaFile cgsm (appendLine',appendLine,appendEmpty) targetProjectFolder manager ga.SqlSprocs ga.MapSqlSprocParams

        appendEmpty()
        let meta = ga.GetSqlMeta appendLine cgsm tables
        meta
        |> List.collect( fun result ->
            match result with
            | Happy sqlTableMeta ->
                sqlTableMeta
            | Unhappy(_,exn) ->
                raise <| InvalidOperationException("Failed to get sql data",exn)
            |> (fun sqlTableMeta ->
                writeColumnInfoMeta cgsm appendLine' sqlTableMeta
                match targetProjectFolder with
                | Some targetProjectFolder -> Path.Combine(targetProjectFolder,sqlTableMeta.TI.Name + ".generated.fs")
                | None -> sqlTableMeta.TI.Name + ".generated.fs"
                |> manager.StartNewFile

                let ncOpts = fSettersCheckInequality sqlTableMeta.TI
                generateFFile (appendLine', appendLine, appendEmpty) generatorId cgsm sqlTableMeta ncOpts

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
    let Generate (fGetSqlMeta,sqlSprocs,fMapSprocParams) generatorId addlNamespaces mutable' (columnNolist:IDictionary<string, string seq>) (manager:IManager, generationEnvironment:StringBuilder, targetProjectName:string, tables, cString:string) nullableHandling generateValueRecords (measures: string seq) (measureNolist: string seq) includeNonDboSchemaInNamespace targetNamespace sprocSettings (pluralizer:Func<_,_>,singularizer:Func<_,_>) (getMeasureNamespace:Func<_,_>) typeGenerationNolist =
        let columnNolist =
            columnNolist |> Map.ofDictionary |> Map.toSeq |> Seq.map (fun (k,v) -> KeyValuePair(k, v |> Set.ofSeq))
            |> Map.ofDictionary

        let cgsm = {    TargetProjectName= targetProjectName
                        SprocSettingMap = sprocSettings |> Option.ofUnsafeNonNullable
                        AdditionalNamespaces= addlNamespaces |> Set.ofSeq
                        TargetNamespace=targetNamespace
                        TypeScriptGenSettingMap = None
                        CString=cString
                        NullableHandling=nullableHandling
                        ColumnNolist = columnNolist
                        Measures=measures |> Set.ofSeq
                        MeasuresNolist= measureNolist |> Set.ofSeq
                        IncludeNonDboSchemaInNamespace= includeNonDboSchemaInNamespace
                        GenerateValueRecords=generateValueRecords
                        Mutable=mutable'
                        Pluralize= pluralizer.Invoke
                        Singularize= singularizer.Invoke
                        TypeGenerationNolist = typeGenerationNolist |> Set.ofSeq
                        GetMeasureNamepace= Option.ofObj getMeasureNamespace |> Option.map (fun f -> f.Invoke) }
        let ga = GenerationArguments<_>.Create(getSqlMeta=fGetSqlMeta, sqlSprocs= sqlSprocs,mapSqlSprocParams=fMapSprocParams)
        generate generatorId ga cgsm
            (manager, generationEnvironment, tables)

    // dbPath was Path.GetFullPath(Path.Combine(currentDir, "..", "..","PracticeManagement","Db"));
module SqlProj =
    open System.IO
    open DataModelToF
    open BCore.CodeGeneration.SqlWrapCore

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
    let getTableInfoFromSqlProj fLog fAppend pathOption sqlProjRootDir tables nolist generatePartials=
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
            |> Seq.filter (fun t-> nolist |> Seq.contains t.TableGenerationInput.Id.Name |> not)
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

    let GetTableInfoFromSqlProj (log:Action<_>) (append:Action<_>) pathOption sqlProjRootDir tables nolist generatePartials = getTableInfoFromSqlProj log.Invoke append.Invoke pathOption sqlProjRootDir tables nolist generatePartials

// impure! applied code, meant for specific scripts, not api
module GenerationSample =
    open DataModelToF
    open BCore.CodeGeneration.SqlWrapCore.ColumnTyping
    open PureCodeGeneration
    open BCore.CodeGeneration.DteWrapCore
    open CodeGeneration
    open BCore.CodeGeneration.SqlWrapCore

    type GenerationStrategy =
        // work around for figuring out where  MacroRunner.MultipleOutputHelper.Managers.Manager(Some "DataModels.tt",sb,debug) would go
        | UseMultipleOutputHelperCode of IManager
        | UseCustomManager
    let _generate generatorId strat pluralize singularize ga connectionString =
        let sb = StringBuilder()
        let mutable currentFile:string = null
        //let pluralizer = Macros.VsMacros.createPluralizer()

        let getManager strat : IManager =
            match strat with
            //upcast MacroRunner.MultipleOutputHelper.Managers.Manager(Some "DataModels.tt",sb,debug)
            |UseMultipleOutputHelperCode m -> m
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

        let manager = getManager strat

        CodeGeneration.SqlScriptGeneration.generateTable false manager sb None
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
                NullableHandling = NullableHandling.UseNullable
                ColumnNolist = Map.empty
                Measures=Set.empty
                MeasuresNolist= Set.empty
                IncludeNonDboSchemaInNamespace= false
                GenerateValueRecords = false
                Mutable = Mutability.Immutable
                GetMeasureNamepace = None
                Pluralize = pluralize
                Singularize = singularize
                AdditionalNamespaces = Set.empty
                TypeGenerationNolist = Set.empty
                SprocSettingMap = None
            }
            let fGetNotifyOptions (ti:TableIdentifier) : NotifyClassOptions =
                if ti.Name="PatientInfo" then
                    {SettersCheckInequality = true; AllowPropertyChangeOverride = false}
                else
                    {SettersCheckInequality = false; AllowPropertyChangeOverride = false}
            DataModelToF.generate generatorId ga cgsm (manager, sb, tablesToGen) fGetNotifyOptions


        manager.GeneratedFileNames
        |> dumpt "files generated"
        |> ignore
        sb.ToString()
        |> dumpt "generated"
