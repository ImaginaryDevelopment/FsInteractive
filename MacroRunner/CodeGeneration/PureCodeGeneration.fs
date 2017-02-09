module CodeGeneration.PureCodeGeneration
open System
open System.IO
open BReusable
open BReusable.StringHelpers

type PathF = |PathF of string
type Code = |Code of (PathF option) * string
type PromoteUnitializedStructsToNullables = | Flag of bool
type TargetCode =
    | File of PathF
    | CodeTarget of Code
    | ClipboardCode
type PropertyOptions =
    | NoNotify
    | KeepNotify
    | InheritFsharpViewModule

type CodeGenTarget =
    |Class of PropertyOptions * PromoteUnitializedStructsToNullables
    |Record
    |Interface
//    |InterfaceBasedRecord
//    |RecordBasedClass of PropertyOptions * PromoteUnitializedStructsToNullables

type CodeSource = | Text of Code | File of PathF
type CodeSources = | CodeSource of CodeSource | Directory of PathF*pattern:(string option)

type ConversionRunOptions = {Target:CodeGenTarget; TypeAttributes: string list; Source: CodeSources; }
//let (|Null|Empty|Single|Multiline|) (s:string)=
//    match s with
//    | null -> Null
//    | _ when String.IsNullOrEmpty(s) -> Empty
//    | x when String.trim s |> String.contains "\r\n" -> Multiline
//    | _ -> Single




type Identifier = | Identifier of string

type DebugOpt = | Yes | Indent of string | No
type DebugVote = | Promote |Abstain (* questionable value *) | Demote
type DebugDelegate = DebugDelegate of (DebugVote -> (DebugOpt*DebugDelegate))


type ClassMember =
    | Property of string
    | Method of string
    | Interface of string

    //| Promote -> let newState = DebugOpt.Yes, getDebugOpt

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open System.Collections.Generic

type TypeSpecification = | Type of Type | Kind of SyntaxKind

//type DebugPredicate = |ByName of (string -> DebugOpt) |ByExpr of (SyntaxNode -> DebugOpt)


type ClassDeclaration = { ClassAttributes: string list; Name:string; BaseClass :string option; Fields: string list; Members: ClassMember list; Interfaces : ClassMember list} with
    member x.AttributeText() = x.ClassAttributes |> Seq.map (fun ta -> sprintf "[<%s>]" ta) |> delimit "\r\n"
    member x.FieldText spacing = x.Fields |> Seq.map (fun f -> spacing + f) |> delimit "\r\n"
module Array =
    let skip v = Seq.skip(v) >> Array.ofSeq
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

    let typeToString= function
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
let mapName s=
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

            IsINotify=prop.AccessorList.ToFullString().Contains("SetAndNotify")
            Type = prop.Type.ToFullString()
            PropertyName = prop.Identifier.ToFullString()
            FieldName = None
            Getter=getter
            Setter=setter
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
                let isDebugNodeResult (_text:string)= Abstain // example: if String.contains text "AddMinutes" then Promote else Abstain

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
        select {FileInfoB.File=file;ClassName=cls;Bases =bases;Fields= root.DescendantNodes() |> Seq.ofType<FieldDeclarationSyntax> |> List.ofSeq ;Properties=properties}
    }

let findModel name fileInfoBseq  =
    fileInfoBseq |> Seq.tryFind(fun fib -> fib.ClassName = name ||fib.ClassName.StartsWith(name))

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
        let arguments= ies.ArgumentList.Arguments|> Seq.map mapNodeP |> delimit ","
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
        let statements = if statement.Contains("\r\n") then "(*switchstatement multiline*)\r\n" + translateOptions.Spacing + (replace "\r\n" ("\r\n" + translateOptions.Spacing) statement) + "\r\n" else statement
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
                if String.IsNullOrEmpty(statement) then
                    "SwitchSectionSyntax.labelOnly", sprintf "%s" label
                else if statement |> trim |> String.contains "\r\n" then
                    "SwitchSectionSyntax.multiStatement", sprintf "%s ->\r\n%s" label statement
                else
                    "SwitchSectionSyntax", sprintf"%s ->%s" label statement
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
//            if mt <> "MemberAccessExpressionSyntax" then Util.Break()
//            if actualResult |> String.contains "x.x" then Util.Break()
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

//            Util.Break()
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
        //dump "ins" <| sprintf "(parentType %A, parent %A, isVar %A, arity %A,identifier %A,kind %A)" (ins.Parent.GetType()) ins.Parent ins.IsVar ins.Arity ins.Identifier (ins.Kind())

        let insText =
//            let isMemberAccess = memberNames.Contains value
//            if isMemberAccess && not <| ins.Parent.contvalue.Contains "." then
//                "Ins:(propName)", sprintf "x.%s" value
//            else
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
        let dumpResult matchType r = dumpf matchType debugOption (fun r-> (node.GetType().Name + "," + matchType + "," + node.Kind().ToString()) + "=\""+ r.ToString()+"\"") r
        let mapResult = dumpResult (sprintf "%s.%s" t <| node.Kind().ToString()) o
        let memberKnowsRaise = memberNames.Contains("RaisePropertyChanged")
        if not memberKnowsRaise && node.ToString().Contains("RaisePropertyChanged") then
            memberNames
            |> dumpt "memberNames"
            |> ignore
        mapResult



