module CodeGeneration.PureCodeGeneration
open System
open System.IO
open BReusable
open BReusable.StringHelpers

type PropertyOptions =
    | NoNotify
    | KeepNotify
    | InheritFsharpViewModule

type PromoteUnitializedStructsToNullables = | Flag of bool
type CodeGenTarget =
    |Class of PropertyOptions * PromoteUnitializedStructsToNullables
    |Record
    |Interface

type ClassMember =
    | Property of string
    | Method of string
    | Interface of string
//    |InterfaceBasedRecord
//    |RecordBasedClass of PropertyOptions * PromoteUnitializedStructsToNullables

type Declaration = { Attributes: string list; Name:string; BaseClass :string option; Fields: string list; Members: ClassMember list} with
    member x.AttributeText() = x.Attributes |> Seq.map (fun ta -> sprintf "[<%s>]" ta) |> delimit "\r\n"
    member x.FieldText spacing = x.Fields |> Seq.map (fun f -> spacing + f) |> delimit "\r\n"

module Array =
    let skip v = Seq.skip(v) >> Array.ofSeq

type PureMeasure private (text) =
    static member IsValidMeasureOpt (x:string) =
        match x with
        | null
        | ""
        | ContainsI "<"
        | Contains " "
        | ContainsI ">" -> false
        | _ -> true
    static member create(measure) =
        if PureMeasure.IsValidMeasureOpt measure then
            Some <| PureMeasure measure
        else None
    member __.Value = text

type PureColumnTypeName private(text) =
    // should be .Net types?
    // should PureColumnTypeName include measures?
    static member IsValidTypeName (x:string) =
        match x with
        | StartsWith "Nullable<"
        | Contains "?"
        // is this ok for `` type names?
        | Contains " "
        | IsTrue (endsWith " Nullable") _
        | IsAnyOf ["bit"; "varchar"; "char"; "nvarchar"; "nchar";"datetime";"xml";"datetime2"] _
            -> false
        | _ -> true

    // should not include <>

    static member Create(typeText) =
        if PureColumnTypeName.IsValidTypeName typeText then
            Some <| PureColumnTypeName(typeText)
        else None
    member __.Value = text
// this appears to unmap a type
let getDefaultValue (measureType:PureMeasure option) fullType =
    let mappedType = fullType
    if mappedType |> endsWith "Nullable" then
        "Nullable()"
    elif mappedType |> endsWith ("option") then
        "None"
    else
        let reMappedType,measuredValue =
            match measureType with
            | None -> mappedType,String.Empty
            | Some measureType ->
                try
                    match mappedType with
                    | "string" -> mappedType, String.Empty
                    // isn't this going to fail on non-measure generics?
                    | Contains "<" -> mappedType |> before "<", sprintf "<%s>"measureType.Value
                    | _ -> mappedType, sprintf "<%s>" measureType.Value
                with ex ->
                    ex.Data.Add("mappedType", mappedType)
                    ex.Data.Add("measureType", measureType)
                    reraise()

        match reMappedType.ToLower() with
            |"int" -> "0" + measuredValue
            |"int64" -> "0L" + measuredValue
            | "bit"
            |"bool" -> "false"
            |"decimal" -> "0m" + measuredValue
            |"float" -> "0." + measuredValue
            |"datetime" -> "System.DateTime.MinValue"
            |"uniqueidentifier" -> "Guid.Empty"
            |_ -> "null"


type PureColumnInput<'T> =
    abstract member IsPrimaryKey:bool
    abstract member IsWriteable:bool
    abstract member Item:'T
    abstract member Name:string
    // result of FBaseType ?
    abstract member TypeName:PureColumnTypeName
    abstract member AllowsNull:bool
    abstract member MeasureText: PureMeasure option

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PureColumnInput =

    ()
open PureColumnInput

///useCliMutable : enables simple serialization see also http://blog.ploeh.dk/2013/10/15/easy-aspnet-web-api-dtos-with-f-climutable-records/
type Mutability = | Immutable | CliMutable | Mutable

type UseOptions = bool
type GetFullTypeTextDelegate<'T> = UseOptions -> PureColumnInput<'T> -> string
type MakeColumnCommentsDelegate<'T> = PureColumnInput<'T> -> string

type IGenerateRecords<'T> =
    abstract member AppendLine: indentations:int -> text:string -> unit
    abstract member MakeColumnComments:MakeColumnCommentsDelegate<'T>
    //let base' = SqlTableColumnChoiceItem.MapSqlType mt igr.UseOptions x
    //interface IMakeColumnComments
    //abstract member GetColumnTypeText: measure:string -> useOptions:bool -> item:PureColumnInput<'T> -> string
    abstract member GetMeasureForColumnName: columnName:string -> PureMeasure option
    abstract member GetFullType: GetFullTypeTextDelegate<'T>
    // should return with nullable and measure when appropriate
    abstract member GetDefaultValueForType: PureColumnInput<'T> -> string
let generateTypeComment columnCount = sprintf "/// %i properties" columnCount

type IGenerateHelper<'T> =
    abstract member GetMeasureForColumnName: columnName:string -> PureMeasure option
    abstract member GetFullType: GetFullTypeTextDelegate<'T>
    abstract member AppendLine: indentations:int -> text:string -> unit
    abstract member MakeColumnComments:MakeColumnCommentsDelegate<'T>


    //abstract member GenerateValueRecord:bool
    //abstract member TypeName:string
    //abstract member UseOptions:bool
    //abstract member Mutability:Mutability
// consider SRTP instead of interface wrapper
let generateRecord (igr:IGenerateRecords<_>) typeName mutability useOptions generateValueRecord (columns: PureColumnInput<_> list) =
    let inline appendLine i = igr.AppendLine i

    // what could be a value record?
    // - a record that is fully nullable so that all fields can indicate if a value was provided (in cases where the default value has a meaning like set the value to null)
    // - something that always uses options, instead of allowing nullables?
    // - something without the pk?
    let columns =
        if generateValueRecord then
            columns
            |> Seq.filter(fun cd -> not cd.IsPrimaryKey)
            |> List.ofSeq
        else columns

    // value record would omit the primary key (and computed columns?)
    let recordTypeName = if generateValueRecord then sprintf "type %sValueRecord" typeName else sprintf "type %sRecord" typeName
    appendLine 0 (generateTypeComment (Seq.length columns))
    if not useOptions then
        appendLine 0 "[<NoComparison>]"
    match mutability with
    | CliMutable ->
        appendLine 0 "[<CLIMutable>]"
    | _ -> ()

    appendLine 0 <| sprintf "%s =" recordTypeName //"type " + typeName + "Record =")
    appendLine 1 "{"

    columns
    |> Seq.iter (fun x ->
        appendLine 2 <| (sprintf "/%s" <| igr.MakeColumnComments x)
        let mt = igr.GetMeasureForColumnName x.Name
        let mapped =
            let base' = igr.GetFullType useOptions x //SqlTableColumnChoiceItem.MapSqlType mt igr.UseOptions x
            match mutability with
            | Mutable -> sprintf "mutable %s" base'
            | _ -> base'

        appendLine 2 <| x.Name + ":" + mapped
    )

    appendLine 1 "}"
    if not generateValueRecord then
        appendLine 1 <| "interface I" + typeName + " with"

        columns
        |> Seq.iter (fun cd ->
            appendLine 2 <| "member x." + cd.Name + " with get () = x." + cd.Name
        )

    appendLine 0 String.Empty
    appendLine 1 "static member Zero () ="
    appendLine 2 "{"

    columns
    |> Seq.iter(fun cd ->
        let mapped = igr.GetFullType useOptions cd //SqlTableColumnChoiceItem.MapSqlType measureText igr.UseOptions cd
        let measureText = igr.GetMeasureForColumnName cd.Name
        try
            appendLine 3 (cd.Name + " = " + (igr.GetDefaultValueForType cd))
        with ex ->
            ex.Data.Add("mapped", mapped)
            ex.Data.Add("ColumnName",cd.Name)
            ex.Data.Add("Measure", measureText)
            reraise()
    )

    appendLine 2 "}"
    appendLine 0 String.Empty

let generateFromSrtpMethod appendLine camelType (fMeasure:_ -> PureMeasure option) (columns: PureColumnInput<_> list) =

    appendLine 0 String.Empty

    appendLine 1 ("let inline toRecordSrtp (" + camelType + ": ^a) =")
    appendLine 2 "{"
    columns
    |> Seq.iter(fun cd ->
        let pm = fMeasure cd.Name
        let measureType =
            if stringEqualsI cd.TypeName.Value "string" then
                null
            else
                match pm, cd.AllowsNull with
                | None, _ -> String.Empty
                | Some v, _ when v.Value |> stringEqualsI "string" -> String.Empty
                | Some v, true -> 
                    //appendLine 3 <| sprintf "// %s %s" v.Value camelType
                    sprintf "|> Nullable.map((*) 1<%s>)" v.Value
                | Some v, false -> 
                    //appendLine 3 <| sprintf "// %s %s" v.Value camelType
                    sprintf " * 1<%s>" v.Value

        appendLine 3 <| sprintf "%s = (^a: (member %s: _) %s)%s" cd.Name cd.Name camelType measureType
    )

    appendLine 2 "}"
    ()
type AppendLineDelegate = int -> string -> unit
type TypeName = string
type GetMeasureForColumnNameDelegate = string -> string
type FOtherHelpersDelegate<'T> = AppendLineDelegate -> UseOptions -> TypeName -> GetMeasureForColumnNameDelegate -> PureColumnInput<'T> list -> unit

let generateFromFMethod fMapFullType appendLine (* for things like making the type comment use the sql, or both net and sql types together *) fColumnComment columns =

    // Convert.ToWhat? what method off the Convert class should we use? What's the overhead for using convert instead of cast? what are the advantages?

    let mapConverter(type' : PureColumnTypeName)  =
        // appears focused on sql types unfortunately, but at least it's magic strings not real references to types
        match type'.Value.ToLower() with
            |"char"
            |"nchar"
            |"nvarchar"
            |"xml"
            |"string"
            |"varchar" -> "ToString"
            |"bool"
            |"bit" -> "ToBoolean"
            | "byte[]"
            // from BReusable
            |"image" -> "ToBinaryData"
            |"date"
            |"datetime"
            |"datetime2"
            |"smalldatetime" -> "ToDateTime"
            // from BReusable
            | "guid"
            |"uniqueidentifier" -> "ToGuid" // invalid
            | StartsWith "int<"
            |"int" -> "ToInt32"
            |"decimal" -> "ToDecimal"
            |"float"  -> "ToDouble"
            |_ -> if isNull type'.Value then String.Empty else type'.Value

    let nonNullables = ["string";"byte[]"]

    appendLine 1 "let fromf (f:string -> obj option) ="
    appendLine 2 "{"

    columns
    |> Seq.iter(fun (cd:PureColumnInput<_>) ->

        let converted = mapConverter cd.TypeName
        appendLine 3 <| sprintf "// %s -> %s (%A)" cd.TypeName.Value converted (cd.MeasureText |> Option.map (fun mt -> mt.Value))
        appendLine 3 <| cd.Name + " ="
        appendLine 4 <| sprintf "match f \"%s\" with %s" cd.Name (fColumnComment cd)
        let measureType =
            cd.MeasureText
            |> Option.bind (fun mt -> if cd.TypeName.Value |> stringEqualsI "string" then None else Some mt)
            |> Option.map (fun mt -> sprintf " |> (*) 1<%s>" mt.Value)
            |> Option.getOrDefault String.Empty

        if cd.AllowsNull && nonNullables |> Seq.exists (stringEqualsI cd.TypeName.Value) |> not then//(mapped <> typeof<string>.Name) && equalsI mapped "string" |> not  then
            sprintf "|Some x -> x |> Convert.%s%s |> Nullable" converted measureType
        else
            sprintf "|Some x -> x |> Convert.%s%s" converted measureType
        |> appendLine 4

        // generate the 0/None/Nullable() value
        let fullType = fMapFullType cd
        let dv = getDefaultValue cd.MeasureText fullType
        appendLine 4 (sprintf "|None -> %s" dv)
    )

    appendLine 2 "}"

    appendLine 0 String.Empty

    appendLine 1 "let FromF (camelTypeF:Func<string,obj option>) = fromf (Func.invoke1 camelTypeF)"

/// generate the helper module
let generateHelperModule (x:IGenerateHelper<_>) useOptions rawHelperItems (typeName:string, columns:PureColumnInput<_> list) fOtherHelpers =
    let inline appendLine i = x.AppendLine i
    let moduleName = sprintf "%sHelpers" typeName
    let camelType = toCamelCase typeName
    appendLine 0 (sprintf "module %s =" moduleName)
    appendLine 0 String.Empty
    appendLine 1 "module Meta ="

    //abstract member UseOptions:bool
    //abstract member TypeName:string
    //// for things like Schema and table name
    //abstract member RawHelperItems: string list
    rawHelperItems
    |> Seq.iter (appendLine 2)
    columns
    |> Seq.iter (fun c ->
        appendLine 2 <| "/" + x.MakeColumnComments c
        appendLine 2 <| sprintf "let %s = \"%s\"" c.Name c.Name
    )

    appendLine 0 String.Empty
    if useOptions && columns |> Seq.exists(fun c -> c.AllowsNull && c.TypeName.Value |> String.equalsI "string" |> not) then
        appendLine 2 <| sprintf "let toDict unwrapToUnsafe x = dict ["
    else appendLine 2 <| sprintf "let toDict x = dict ["
    columns
    |> Seq.map(fun c ->
        // handle options, we don't want them going into the unsafe dict
        if c.AllowsNull && useOptions then
                sprintf "%s, box (if unwraptoUnsafe then x.%s |> Option.getOrDefault Unchecked.defaultof<_> else x.%s)" c.Name c.Name c.Name
        else 
            sprintf "%s, box x.%s" c.Name c.Name
    )
    |> Seq.iter (appendLine 6)

    appendLine 2 "]"

    appendLine 0 String.Empty
    appendLine 2 <| sprintf "let allProps = Set.ofSeq ["
    columns
    |> Seq.map(fun c -> c.Name)
    |> delimit "; "
    |> appendLine 6
    appendLine 2 "]"
    appendLine 0 String.Empty
    appendLine 2 <| sprintf "let writableProps ="
    appendLine 3 <| "Set.ofSeq ["
    columns
    |> Seq.filter(fun c -> c.IsWriteable)
    |> Seq.map(fun c -> c.Name)
    |> delimit "; "
    |> appendLine 4
    appendLine 2 "]"
    appendLine 0 String.Empty

    appendLine 1 <| sprintf "let inline toRecord (%s:I%s) =" camelType typeName
    appendLine 2 "{"

    columns
    |> Seq.iter(fun cd ->
        // could use some Measure compat
        appendLine 3 (cd.Name + " = " + camelType + "." + cd.Name)
    )

    appendLine 2 "}"
    // start the fromF series
    appendLine 0 String.Empty
    generateFromFMethod (x.GetFullType useOptions) appendLine x.MakeColumnComments columns
    generateFromSrtpMethod appendLine camelType x.GetMeasureForColumnName columns
    // for things like :
    //generateCreateSqlInsertTextMethod appendLine useOptions typeName schemaName tableName fMeasure columns
    fOtherHelpers (fun i -> appendLine (i + 1))
    appendLine 0 String.Empty
()

type InterfaceGeneratorArgs = { Writeable:bool; UseOptions:bool}
let generateInterface fColumnComment fMap (typeName:string, fMeasure, columns: PureColumnInput<_> list, appendLine:int -> string -> unit, interfaceGeneratorArgs ) =
    appendLine 0 <| sprintf "// typeName:%s writeable:%A useOptions:%A" typeName interfaceGeneratorArgs.Writeable interfaceGeneratorArgs.UseOptions
    appendLine 0 (generateTypeComment columns.Length)
    appendLine 0 ("type I" + typeName + (if interfaceGeneratorArgs.Writeable  then "RW" else String.Empty) + " =")
    if interfaceGeneratorArgs.Writeable then
        appendLine 1 ("inherit I" + typeName)

    columns
    |> Seq.iter(fun item ->
        appendLine 1 (item |> fColumnComment |> sprintf "/%s" )
        let measureText : PureMeasure option = fMeasure item.Name
        let mapped = fMap measureText interfaceGeneratorArgs.UseOptions item

        appendLine 1 <| sprintf "abstract member %s:%s with get%s" item.Name mapped (if interfaceGeneratorArgs.Writeable && item.IsWriteable then ",set" else String.Empty)
    )

    appendLine 0 String.Empty

let generateClass fColumnComment fIsWriteable (typeName:string, columns: PureColumnInput<_> list, appendLine:int -> string -> unit) =
    let mapFieldNameFromType(columnName:string) =
        match toCamelCase columnName with
        | "type" ->  "type'"
        | camel -> camel
    appendLine 0 <| generateTypeComment columns.Length
    appendLine 0 <| "type "+ typeName + " (model:" + typeName + "Record) ="
    appendLine 0 String.Empty
    columns |> Seq.iter(fun cd ->
        let camel = mapFieldNameFromType(cd.Name)
        appendLine 1 <| "let mutable "+ camel + " = model." + cd.Name
    )

    appendLine 0 String.Empty

    columns |> Seq.iter(fun cd ->
        let camel = mapFieldNameFromType cd.Name
        appendLine 0 String.Empty
        appendLine 1 <| "/" + fColumnComment cd
        appendLine 1 <| "member __." + cd.Name
        appendLine 2 <| "with get() = " + camel
        appendLine 2 <| sprintf "and set v = %s <- v" camel
    )

    appendLine 0 String.Empty
    let interfaceName = sprintf "I%s" typeName

    appendLine 1 (sprintf "interface %s with" interfaceName)
    columns |> Seq.iter(fun cd ->
        appendLine 2 <| fColumnComment cd
        appendLine 2 <|"member x." + cd.Name + " with get () = x." + cd.Name
    )

    appendLine 1 ("interface I" + typeName + "RW with")

    columns |> Seq.iter(fun cd ->
        appendLine 2 (fColumnComment cd)
        if fIsWriteable cd then
            appendLine 2 <| "member x." + cd.Name + " with get () = x." + cd.Name + " and set v = x." + cd.Name + " <- v"
        else 
            appendLine 2 <| "member x." + cd.Name + " with get () = x." + cd.Name
    )

    appendLine 0 String.Empty
    appendLine 1 (sprintf "member x.MakeRecord () = x :> %s |> %sHelpers.toRecord" interfaceName typeName)


let generateINotifyClass fColumnComment fIsWriteable (typeName:string, columns: PureColumnInput<_> list, appendLine:int -> string -> unit) =
    let mapFieldNameFromType(columnName:string) =
        match toCamelCase columnName with
        | "type" ->  "type'"
        | camel -> camel
    appendLine 0 (generateTypeComment columns.Length)
    appendLine 0 ("type "+ typeName + "N (model:" + typeName + "Record) =")
    appendLine 0 String.Empty
    appendLine 1 "let propertyChanged = new Event<_, _>()"
    appendLine 0 String.Empty
    appendLine 0 String.Empty
    for cd in columns do
        let camel = mapFieldNameFromType(cd.Name)
        appendLine 1 ("let mutable "+ camel + " = model." + cd.Name)

    appendLine 0 String.Empty
    let interfaceName = sprintf "I%s" typeName

    appendLine 1 (sprintf "interface %s with" interfaceName)
    for cd in columns do
        appendLine 2 <| fColumnComment cd
        appendLine 2 <|"member x." + cd.Name + " with get () = x." + cd.Name

    appendLine 1 ("interface I" + typeName + "RW with")

    for cd in columns do
        appendLine 2 (fColumnComment cd)
        if fIsWriteable cd then
            appendLine 2 <| "member x." + cd.Name + " with get () = x." + cd.Name + " and set v = x." + cd.Name + " <- v"
        else 
            appendLine 2 <| "member x." + cd.Name + " with get () = x." + cd.Name

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
        appendLine 1 <| "/" + fColumnComment cd
        appendLine 1 <| "member x." + cd.Name
        appendLine 2 <| "with get() = " + camel
        appendLine 2 "and set v ="
        //to consider: this might benefit from only setting/raising changed if the value is different
        appendLine 3 (camel + " <- v")
        appendLine 3 ("x.RaisePropertyChanged \"" + cd.Name + "\"")

module TranslateCSharp =
    type PathF = |PathF of string
    type Code = |Code of (PathF option) * string
    type TargetCode =
        | File of PathF
        | CodeTarget of Code
        | ClipboardCode

    type CodeSource = | Text of Code | File of PathF
    type CodeSources = | CodeSource of CodeSource | Directory of PathF*pattern:(string option)

    type ConversionRunOptions = {Target:CodeGenTarget; TypeAttributes: string list; Source: CodeSources; }


    type Identifier = | Identifier of string

    type DebugOpt = | Yes | Indent of string | No
    type DebugVote = | Promote |Abstain (* questionable value *) | Demote
    type DebugDelegate = DebugDelegate of (DebugVote -> (DebugOpt*DebugDelegate))


        //| Promote -> let newState = DebugOpt.Yes, getDebugOpt

    open Microsoft.CodeAnalysis
    open Microsoft.CodeAnalysis.CSharp
    open Microsoft.CodeAnalysis.CSharp.Syntax
    open System.Collections.Generic

    type TypeSpecification = | Type of Type | Kind of SyntaxKind

    module Seq =
        #if F43

        #else // this function already exists in newer versions of F#
        let contains v = Seq.exists (fun f -> f = v)
        #endif
        #if LINQPAD
        let inline iterDump items = items.Dump(1)
        let inline iterDumpInd indent items =
            Util.WithStyle(items,"font-size:large;margin-left:60px").Dump(1)
        #else
        let inline iterDump items = items |> Seq.iter (fun i-> printfn "%A" i)
        let inline iterDumpInd indent items = items |> Seq.iter( fun i -> printfn "%s%A" indent i)
        #endif
        let inline dump items = printfn "%A" items; items
        let inline dumps title items =
            if Seq.isEmpty items then printfn"%s: is empty" title else printfn "%s:%A" title items
            items

        let inline ofType<'a> (source : System.Collections.IEnumerable) : 'a seq=
            let resultType = typeof<'a>
            seq {
                for item in source do
                    match item with
                    | null -> ()
                    | _ ->
                        if resultType.IsAssignableFrom (item.GetType()) then
                            yield (downcast item)
            }

    [<AutoOpen>]
    module Helpers =
        let enumerateAllFiles rootPath pattern = System.IO.Directory.EnumerateFiles(rootPath,pattern, SearchOption.AllDirectories)
        let readAllText filePath = File.ReadAllText(filePath)
        let inline flip f arg1 arg2 = f arg2 arg1

        let typeToString = function
            |Microsoft.FSharp.Quotations.Patterns.Call (e,r,children) -> printfn "Call %A,%A,%A" e (r.GetGenericArguments().[0].Name) children
            |_ as x -> failwithf "call must match typeof<Foo> %A" x

        let inline sprintForDump title o = sprintf "%s:%A" title o

        #if LINQPAD
        let inline private dumps' (s:string) (o:'a) : 'a = o.Dump(s,Nullable(1)); o
        #else
        let inline private dumps' title o = printfn "%s" (sprintForDump title o); o
        #endif
        let dumps s debugOpt o =
            match debugOpt with | Yes -> dumps' s o | Indent spc -> dumps' (sprintf "%s%s" spc s) o | No -> o
        let dump title debugOpt o  =
            dumps title debugOpt o |> ignore
        let dumpf t debugOpt f s =
            let shouldBeUnit = f s |> dumps t debugOpt
            shouldBeUnit|> ignore // ignore f of s, return s
            s
        let debugLines debug (lines:string seq) =
            match debug with
            | Yes -> Seq.iterDump lines
            | Indent spc -> Seq.iterDumpInd spc lines
            | No -> ()

    module FileWalker =

        type ModelCollector() =
            inherit CSharpSyntaxWalker()
            let implementedInterfaces = new Dictionary<string,string list>()
            member private __.ImplementedInterfaces() = implementedInterfaces
            //full of fail:
            // member private x.ImplementedInterfaces = new Dictionary<string,string list>()
            static member VisitClassInterfaces (root:CompilationUnitSyntax) =
                let mc = new ModelCollector()
                mc.Visit(root)
                mc.ImplementedInterfaces()
                //|> Seq.dumps "implemented interfaces"
            override __.VisitBaseList node =
                let parentIdentifier = (node.Parent :?> ClassDeclarationSyntax).Identifier.ValueText
                let bases =
                    node.Types
                    |> Seq.map (fun t-> t.Type)
                    |> Seq.ofType<IdentifierNameSyntax>
                    |> Seq.map (fun ins -> ins.Identifier.ValueText)
                    //|> Seq.dumps (sprintf "bases on %A" parentIdentifier)
                    |> List.ofSeq
                implementedInterfaces.Add(parentIdentifier,bases)
                base.VisitBaseList node

        let getSrcCode codeSrc =
            match codeSrc with
            | Text code -> code
            | File (PathF path) -> path |> readAllText |> (fun c -> Code (Some(PathF path),c))

        let walkCode code =
            let src = match code with |Code (_p,src) -> src
            let tree = CSharpSyntaxTree.ParseText(src)
            let root = tree.GetRoot() :?> CompilationUnitSyntax

            let classesToBases=
                let clsToBases = new Dictionary<string,string list>()
                let dic = ModelCollector.VisitClassInterfaces root
                dic
                //|> Seq.dumps "interfaces!"
                // only convert classes that inherit DataModelBase, why?
                //|> Seq.filter (fun i -> i.Key <> null && (i.Value |> (* Seq.dumps "bases" |> *) Seq.exists (fun v -> v = "DataModelBase")))
                |> Seq.iter (fun kvp -> clsToBases.Add(kvp.Key, kvp.Value))
                clsToBases
            if classesToBases.Count > 0 then
                Some (code,root,classesToBases)
            else None

    let getFiles source =
        let codeSourceMap cs =
            let codeSourceResult = cs |> FileWalker.getSrcCode |> FileWalker.walkCode
            codeSourceResult
        let files =
            match source with
            | CodeSource cs ->
                [ codeSourceMap cs ]
            | Directory (PathF dir,patternWildCardOpt) ->
                printfn "rootPath = %s" dir
                // this applies only to my specific usage
                //enumerateAllFiles dir "*DataModel.cs"
                match patternWildCardOpt with
                | Some wc -> enumerateAllFiles dir wc
                | None -> enumerateAllFiles dir "*.cs"
                |> Seq.map PathF
                |> Seq.map CodeSource.File
                |> Seq.map codeSourceMap
                |> List.ofSeq
        files


    type PropertyInfoB = { IsINotify:bool; Type:string; FieldName: string option; PropertyName:string; Getter:AccessorDeclarationSyntax option; Setter:AccessorDeclarationSyntax option}

    let mapName s =
        match trim s with
        | "" -> failwithf "name cannot be empty"
        | Id when Id.EndsWith("ID") -> Id |> before "ID" |> flip (+) "Id"
        | _ as s -> s

    let toFType promoteUninitializedStructsToNullable (t:string) =
            match trim t with
            | nullable when nullable.Contains("?") -> nullable |> before "?" |> sprintf "Nullable<%s>"
            | x when x="Guid" || x = "System.Guid" -> if promoteUninitializedStructsToNullable then sprintf"(* NullableWithoutInit*) Nullable<%s>" x else x
            | x when x="Image" -> "System.Drawing.Image"
            | _ as type' -> type'

    let toFull (node:#SyntaxNode) = node.ToFullString() |> trim

    let mapToken (token:SyntaxToken)=
        match token.ValueText with
        |"==" -> "="
        | "!=" -> "<>"
        |"&&"
        | _ as result -> result

    let toSyntaxNodeOpt x = match x with | Some n -> n :> SyntaxNode |> Some | None -> None

    let mapPropertyDeclaration (prop:PropertyDeclarationSyntax) =
        let accessorCount = prop.AccessorList.Accessors.Count
        if accessorCount > 2 then failwithf "too many accessors %s" prop.Identifier.ValueText
        let tryFindAccessor k = prop.AccessorList.Accessors |> Seq.tryFind (fun a -> a.Kind() = k)
        let getter = tryFindAccessor SyntaxKind.GetAccessorDeclaration
        let setter = tryFindAccessor SyntaxKind.SetAccessorDeclaration
        {
            IsINotify = prop.AccessorList.ToFullString().Contains("SetAndNotify")
            Type = prop.Type.ToFullString()
            PropertyName = prop.Identifier.ToFullString()
            FieldName = None
            Getter = getter
            Setter = setter
        }

    let getProperties (root:CompilationUnitSyntax) =
        let nodes = root.DescendantNodes() |> Array.ofSeq
        nodes
        |> Seq.map box
        |> Seq.ofType<PropertyDeclarationSyntax>
        |> Seq.map mapPropertyDeclaration

    type FileInfoB = {File:Code; ClassName:string; Bases: string list;Fields:FieldDeclarationSyntax list; Properties: PropertyInfoB list}
        with
        member x.getMemberNames() =
            let fieldNames = x.Fields |> Seq.collect (fun f -> f.Declaration.Variables |> Seq.map (fun v -> v.Identifier.Text ))|> List.ofSeq
            let propNames = x.Properties |> List.map (fun p -> p.PropertyName)
            // method names?
            fieldNames@propNames

    type TranslateOptions = {
        GetNextDebugState:DebugDelegate -> DebugVote -> DebugOpt * DebugDelegate
        GetIsDebugNode: SyntaxNode -> DebugVote
        Spacing:string
        SelfIdentifier:string
        GetIsDebugNodeResult:string -> DebugVote
        GetIsDebugPropPred:string -> DebugVote
        GetStartDebugState: DebugOpt option -> DebugVote -> DebugOpt*DebugDelegate
        GetIsDebugClass:string -> DebugVote
        GetIsDebugFieldPred: string -> DebugVote
        IncludeMatchTypeInComments:bool
        IncludeOriginalInComments:bool
        }
        with
            static member getDefault() =
                    let spacing = List.replicate 4 " " |> delimit String.Empty
                    let getNextDebugState debugDelegate vote =
                        match debugDelegate with |DebugDelegate getDebugOpt -> getDebugOpt vote
                    let isDebugNodeResult (_text:string) = Abstain // example: if String.contains text "AddMinutes" then Promote else Abstain

                    let rec startDebugState state vote : DebugOpt * DebugDelegate =
                        let state =
                            match state,vote with
                            | Some(x), Abstain -> x
                            | None, Abstain -> DebugOpt.No
                            | Some (DebugOpt.Yes), Promote -> DebugOpt.Indent spacing
                            | Some (DebugOpt.Indent spc), Promote -> DebugOpt.Indent(spc + spacing)
                            | Some (DebugOpt.No), Promote -> DebugOpt.Yes
                            | Some (DebugOpt.Indent spc), Demote -> if spc.Length > spacing.Length then DebugOpt.Indent (before spacing spc) else DebugOpt.Yes
                            | _, Demote -> DebugOpt.No
                            | _, Promote -> DebugOpt.Yes

                        let dd = DebugDelegate (fun vote -> startDebugState (Some state) vote)
                        state,dd
                    {   GetStartDebugState = startDebugState
                        GetNextDebugState= getNextDebugState
                        GetIsDebugNode=   fun (_node: SyntaxNode) -> Abstain
                        Spacing= spacing
                        SelfIdentifier= "x"
                        GetIsDebugNodeResult = isDebugNodeResult
                        GetIsDebugPropPred = fun name -> if name = "LOS" then Promote else Demote
                        GetIsDebugClass = fun _name -> Abstain
                        GetIsDebugFieldPred = fun _name -> Abstain
                        IncludeMatchTypeInComments=true
                        IncludeOriginalInComments=true
                    }

    let getFileInfoFromSource (source:(_*_*Dictionary<string,string list>) option seq) =

        let files' = source |> Seq.choose id |> Array.ofSeq
        //printfn "Files: %A" files'
        let filesToClassesToBases =
            files' |> Seq.length |> printfn "Checking %i files"
            files'
        query{
            for (file,root,clsToBases) in filesToClassesToBases do
            for cls in clsToBases.Keys do
            let bases = clsToBases.[cls]

            // (* already done on line 54 *)
            // where(Seq.contains "DataModelBase" bases)
            let properties = getProperties(root) |> List.ofSeq
            select {FileInfoB.File = file; ClassName = cls; Bases = bases; Fields = root.DescendantNodes() |> Seq.ofType<FieldDeclarationSyntax> |> List.ofSeq; Properties = properties}
        }

    let findModel name fileInfoBseq =
        fileInfoBseq |> Seq.tryFind(fun fib -> fib.ClassName = name || fib.ClassName.StartsWith name)

    module Declarations =
        let (|EmptyEnumerable|NonEmpty|) (items: _ IEnumerable) =
            if Seq.isEmpty items then EmptyEnumerable else NonEmpty

        let (|AutoProperty|_|) (getter'setter:AccessorDeclarationSyntax option*AccessorDeclarationSyntax option) =
            let getter,setter = fst getter'setter, snd getter'setter
            match getter,setter with
            | Some g,Some s ->
                match g.DescendantNodes(),s.DescendantNodes() with
                | EmptyEnumerable,EmptyEnumerable -> Some ()
                | _ -> None
            | _ -> None

    let getFileInfoFrom source =
        let files = getFiles source
        getFileInfoFromSource files

    let rec mapNode translateOptions promoteUninitializedStructsToNullable spacing (memberNames:Set<string>) (getDebugOpt: DebugDelegate) (node:SyntaxNode) =
        let debugOption, getDebugOpt = translateOptions.GetNextDebugState getDebugOpt (translateOptions.GetIsDebugNode node)

        let inline dumps title (o:'a) :'a = dumps title debugOption o
        let inline dump title o : unit = dumps title o |> ignore
        let toFType = toFType promoteUninitializedStructsToNullable
        let printNodeDiagnostics (n: #SyntaxNode) =
            (n.Kind()) |> dump "node diagnostics"
            let children = n.ChildNodes()
            if Seq.isEmpty children = false then
                dump "children" (children |> Seq.map (fun c -> c.Kind()) |> Array.ofSeq)
            dump "HasTrivia" (n.HasLeadingTrivia, n.HasStructuredTrivia, n.HasTrailingTrivia)
            dump "missing,isstructured" (n.IsMissing, n.IsStructuredTrivia)

        let mapNodeP node = mapNode translateOptions promoteUninitializedStructsToNullable spacing memberNames getDebugOpt node

        let mapChildren delimiter (node:#SyntaxNode) = node.ChildNodes() |> Seq.map mapNodeP |> delimit delimiter
        match node with
        | null -> failwithf "null node"
        | :? BlockSyntax as bs -> "BlockSyntax", mapChildren "\r\n" bs
        | :? ElseClauseSyntax as ecs -> "ElseClauseSyntax", sprintf " else %s " (mapNodeP ecs.Statement)
        | :? ReturnStatementSyntax as rss -> "ReturnStatementSyntax", mapChildren  "\r\n" rss
        | :? InvocationExpressionSyntax as ies ->

            let expr = mapNodeP ies.Expression
            let expr =
                if memberNames.Contains(expr) then
                    sprintf "%s.%s" translateOptions.SelfIdentifier expr
                else expr
            if  expr.Contains("AddMinutes") then
                dump "IES:AddMinutes" ies.ArgumentList.Arguments.[0]
            let ignoredResult = expr.StartsWith("builder.Append")

            //dump "ies details" <| sprintf "%A" (expr,ies.Expression.GetType().Name, ies.ArgumentList.Arguments, ies.ChildNodes())
            let arguments = ies.ArgumentList.Arguments|> Seq.map mapNodeP |> delimit ","
            let iesText = sprintf "%s(%s)%s" expr arguments (if ignoredResult then "|> ignore" else String.Empty)

            "InvocationExpressionSyntax", iesText
        | :? LocalDeclarationStatementSyntax as ldss ->
            let full = if ldss.Declaration.Variables.Count> 0 then Some (mapNodeP ldss.Declaration.Variables.[0]) else None
            match ldss.Declaration.Type.IsVar, ldss.Declaration.Variables.Count, full with
            | true,1, (Some x) ->"ldss var", "let mutable " + x
            | false, 1, (Some x) ->
                if x |> String.contains "=" then
                    "ldss nonVarX", "let mutable " + x
                else
                    dump "nonVarUninit" x
                    let type' = toFType <| mapNodeP ldss.Declaration.Type
                    "ldss nonVarUninit", sprintf "let mutable %s:%s (* %s *)" x type' "nonVar" + " = null"

            | _ ->
                dump "ldss" <| sprintf "%A" (ldss.Declaration.Type, ldss.Declaration.Variables.Count, ldss.Declaration.Variables.[0])
                "LocalDeclarationStatementSyntax", ldss |> toFull |> replace "var " "let " |> trim
        | :? ConditionalExpressionSyntax as ces ->
            "ConditionalExpressionSyntax", sprintf "(if %s then %s else %s )" (mapNodeP ces.Condition) (mapNodeP ces.WhenTrue) (mapNodeP ces.WhenFalse)
        | :? ParenthesizedExpressionSyntax as pes ->
            "ParenthesizedExpressionSyntax", sprintf "(%s)" (mapNodeP pes.Expression)
        | :? ElementAccessExpressionSyntax as eaes ->
            "ElementAccessExpressionSyntax", sprintf "%s.%s" (mapNodeP eaes.Expression) (toFull eaes.ArgumentList)
        | :? LiteralExpressionSyntax as les ->
            dump "parent" les.Parent

            match les.Kind() with
            | SyntaxKind.NullLiteralExpression ->
                if les.Parent :? BinaryExpressionSyntax then
                    "NullLiteralExpression(caes)", "null"
                else
                    dump "NullLiteralExpression" <| les.Parent.GetType().Name + (les.Parent.Kind().ToString())
                    "NullLiteralExpression","(Nullable())"
            | SyntaxKind.NumericLiteralExpression ->

                let ggpFull = toFull les.Parent.Parent.Parent
                let full = toFull les
                let gpk = les.Parent.Parent.Kind()
                if gpk <> SyntaxKind.CaseSwitchLabel && ggpFull |>  String.contains "decimal" && full |> String.contains "." |> not then
                    "NumericLiteralExpression.decimal", (full + "m")
                else
                    //printNodeDiagnostics les
                    //dump "NLE ggp" ggpFull
                    //dump "NLE gpk" gpk
                    //dump "NLE pk" <| les.Parent.Kind()
                    //dump "NLE" full

                    "NumericLiteralExpression", full
            | _ -> "NullableTypeSyntax", (toFull les)
        | :? SwitchStatementSyntax as sss ->
            let sections =  "(*switchstatement start*)\r\n" + (Array.ofSeq sss.Sections |> Seq.map mapNodeP |> delimit String.Empty)
            "SwitchStatementSyntax", sprintf "match %s with %s" (mapNodeP sss.Expression) sections

        | :? IfStatementSyntax as ifss ->
            let statement = mapNodeP ifss.Statement
            let elseblock = if ifss.Else <> null then dumps "found else!" <| mapNodeP ifss.Else |> Some else None
            let elseblock,matchType = match elseblock with Some text -> sprintf " %s" text,"IfElseStatementSyntax" | _ -> String.Empty,"IfStatementSyntax"
            let statements = if statement.Contains "\r\n" then "(*switchstatement multiline*)\r\n" + translateOptions.Spacing + (replace "\r\n" ("\r\n" + translateOptions.Spacing) statement) + "\r\n" else statement
            matchType, sprintf "if %s then %s%s" (mapNodeP ifss.Condition) statements elseblock

        | :? PrefixUnaryExpressionSyntax as pues -> "PrefixUnaryExpressionSyntax", sprintf "not <| ( %s )" (mapNodeP pues.Operand)
        | :? SwitchSectionSyntax as sss ->
                let labels,statements = sss.Labels |> Array.ofSeq, sss.Statements |> Array.ofSeq
                let label = labels |> Seq.map mapNodeP |> Array.ofSeq
                let label = delimit String.Empty label
                dump "label" label
                if Seq.isEmpty statements then
                    "SwitchSectionSyntax.nostatements", sprintf "%s" label
                else
                    let statement = statements |> Seq.map mapNodeP |> delimit ("(*switch section end*)\r\n") |> indent spacing
                    if String.IsNullOrEmpty statement then
                        "SwitchSectionSyntax.labelOnly", sprintf "%s" label
                    else if statement |> trim |> String.contains "\r\n" then
                        "SwitchSectionSyntax.multiStatement", sprintf "%s ->\r\n%s" label statement
                    else
                        "SwitchSectionSyntax", sprintf "%s ->%s" label statement
        | :? DefaultSwitchLabelSyntax as _dsls -> "DefaultSwitchLabelSyntax", "|_"
        | :? SwitchLabelSyntax as sls ->
            printNodeDiagnostics sls
            let children = sls.ChildNodes() |> Array.ofSeq
            if children.Length = 1 then
                if children.[0] :? BlockSyntax then
                    "SwitchLabelSyntax(bs)", sprintf "|%s" (mapNodeP children.[0])
                else
                    dump "SwitchLabelChild" children.[0]
                    "SwitchLabelSyntax(1)", sprintf "|%s" (mapNodeP children.[0])
            else
                dump "SwitchLabelChildren" children
                "SwitchLabelSyntax", toFull sls
        | :? AssignmentExpressionSyntax as aes -> "AssignmentExpressionSyntax", sprintf "%s <- %s" (mapNodeP aes.Left) (mapNodeP aes.Right)
        | :? BreakStatementSyntax as _bss ->
            "BreakStatementSyntax", String.Empty
        | :? MemberAccessExpressionSyntax as maes ->
            //dump "maes details" <| sprintf "%A" (maes.Name,maes.Expression, maes.OperatorToken)
            let _expr =
                match maes.Expression with
                | :? ThisExpressionSyntax as _tes -> translateOptions.SelfIdentifier
                | _ ->
                    let exprResult = mapNodeP maes.Expression
                    exprResult

            let expr = mapNodeP maes.Expression
            let token = mapToken maes.OperatorToken
            let name = mapNodeP maes.Name

            dump "maes details2" <| sprintf "%A" (expr,token,name)
            let maesResult =
                let defaultPathResult = sprintf "%s%s%s" expr token (mapName name)
                let mt, actualResult =
                    match expr, token with
                    | "this","." -> "This(maes)", sprintf "%s.%s" translateOptions.SelfIdentifier (mapName name)
                    | "string","." -> "string(maes)", (sprintf "%s.%s" "String" name|> dumps "maes result")
                    | x, _ when x.Contains("this.") -> "this.(maes)", x|> replace "this." "x."
                    |_ -> "MemberAccessExpressionSyntax", defaultPathResult
                mt,actualResult

            maesResult
        | :? ThisExpressionSyntax as _tes -> "ThisExpressionSyntax", translateOptions.SelfIdentifier
        | :? ArgumentSyntax as arg ->
            let argText = mapChildren String.Empty arg
            "ArgumentSyntax", argText
        | :? BinaryExpressionSyntax as bes -> "BinaryExpressionSyntax", sprintf "%s %s %s" (mapNodeP bes.Left) (mapToken bes.OperatorToken) (mapNodeP bes.Right)
        | :? ExpressionStatementSyntax as ess ->

            let essText =
                match ess.Expression with
                | :? InvocationExpressionSyntax as ies ->
                    let iesExprText = mapNodeP ies.Expression
                    if memberNames.Contains iesExprText then
                        sprintf "%s.%s" translateOptions.SelfIdentifier (mapChildren String.Empty ess)
                    else mapChildren String.Empty ess
                |_ ->mapChildren String.Empty ess
            if essText.StartsWith("RaiseProperty") then
                let _exprType = ess.Expression.GetType().Name
                let children = ess.ChildNodes() |> Array.ofSeq

                box children = box ess.Expression |> ignore<bool>
                ()

            "ExpressionStatementSyntax", essText
        | :? EqualsValueClauseSyntax as evcs ->
                "EqualsValueClauseSyntax", sprintf "= %s" (mapNodeP evcs.Value)
        | :? VariableDeclaratorSyntax as vds ->
                //dump "vds " (sprintf "would have been %s" (toFull vds))
                "VariableDeclaratorSyntax", sprintf "%s %s" (mapToken vds.Identifier) (mapChildren String.Empty vds)
        | :? IdentifierNameSyntax as ins ->
            if ins = null then failwithf "no identifier for ins %s" (toFull node)

            let ident = ins.Identifier
            if ident.ValueText = null then failwithf "no ValueText for ins %s" (toFull ins)
            let _value = mapName ident.ValueText

            let insText =
                let name = mapName ident.ValueText
                if name.StartsWith("_") then
                    "Ins:(_)", sprintf "%s" name
                else  if ins.Parent :? ArgumentSyntax then
                    "Ins:()", sprintf "%s" name
                else
                     "IdentifierNameSyntax", sprintf "%s" (mapName ident.ValueText)
            insText
        | :? PredefinedTypeSyntax as pts ->
            printNodeDiagnostics pts
            let text = pts.ToFullString()

            "PredefinedTypeSyntax", sprintf "%s" text

        | :? ObjectCreationExpressionSyntax as oces ->
            printNodeDiagnostics oces
            "ObjectCreationExpressionSyntax", oces.ToFullString()

        | n when n.Kind() = SyntaxKind.AddExpression ->
            //if printDebug then
                //printfn "AddExpression is type %s" (n.GetType().Name)
                //n|> dumps "mapNode:AddExpression" |> ignore
            let result = toFull n
            //if printDebug then
                //result |> dumps "mapNode:AddExpressionText" |> ignore
            "AddExpression", result
        | :? ForEachStatementSyntax as fess ->
            let children = fess.ChildNodes() |> Array.ofSeq

            let _identifier = children.[0] :?> IdentifierNameSyntax
            let mappedChildren =
                mapChildren String.Empty fess.Statement
            let fessText = sprintf "for %s in %s do\r\n%s%s" (mapToken fess.Identifier) (mapNodeP children.[1]) spacing mappedChildren
            //let fessText = mapChildren String.Empty fess |> sprintf "for %s"
            "forEach", fessText
        | :? GenericNameSyntax
        | :? NullableTypeSyntax
        | :? CastExpressionSyntax // needs to be handled for (int?)null casts

        | :? QualifiedNameSyntax ->
            let knownUnmapped = node |> toFull
            "known unmapped", knownUnmapped
        | _ -> "default",node |> toFull
        |> fun (t,o) ->
    #if LINQPAD
            if o |> String.contains "x.x" then Util.Break()
    #endif
            let debugOption,_getDebugOpt = translateOptions.GetNextDebugState getDebugOpt (translateOptions.GetIsDebugNodeResult o)
            let dumpResult matchType r = dumpf matchType debugOption (fun r -> (node.GetType().Name + "," + matchType + "," + node.Kind().ToString()) + "=\"" + r.ToString() + "\"") r
            let mapResult = dumpResult (sprintf "%s.%s" t <| node.Kind().ToString()) o
            let memberKnowsRaise = memberNames.Contains("RaisePropertyChanged")
            if not memberKnowsRaise && node.ToString().Contains("RaisePropertyChanged") then
                memberNames
                |> dumpt "memberNames"
                |> ignore
            mapResult
