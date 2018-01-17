namespace CodeGeneration
open System
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open PureCodeGeneration
open BReusable
open BReusable.StringHelpers


module FieldConversion =
//    open Declarations

    type FieldInfoB = {Type:string; Name:string; Initial:string option; Declaration:VariableDeclarationSyntax}
    let convertFileFields translateOptions promoteUninitializedStructsToNullable spacing (fileInfoB:FileInfoB) (getDebugOpt:DebugDelegate) : string list =
        let promoteUninitializedStructsToNullable = match promoteUninitializedStructsToNullable with | Flag b -> b
        let cls = fileInfoB

        let inline mapNode memberNames = mapNode translateOptions promoteUninitializedStructsToNullable memberNames
        let toFType = toFType promoteUninitializedStructsToNullable
        let _debugOption, getDebugOpt = translateOptions.GetNextDebugState getDebugOpt (translateOptions.GetIsDebugClass cls.ClassName) //(ScriptOptions.isDebugClass cls.Class')

        let fieldNames =
            cls.Fields
            |> Seq.map ( fun f ->f.Declaration.Variables |> Seq.map(fun v -> v.Identifier.ValueText))
            |> Seq.collect id
            |> Set.ofSeq
        //Seq.iterDump fieldNames

        let fields =
            let getDebugFieldOptions=  translateOptions.GetIsDebugFieldPred >> translateOptions.GetNextDebugState getDebugOpt
            cls.Fields
            //|> Seq.sortBy (fun f-> f.Declaration.Variables.Item)
            |> Seq.map (fun f -> f.Declaration.ToFullString(), f.Declaration)
            |> Seq.map ( fun (_fs,vDeclaration) ->

                if vDeclaration.Variables.Count <> 1 then failwithf "too many vars: %s" (toFull vDeclaration)
                let var = vDeclaration.Variables.[0]
                if var = null then failwithf "bad var"
                let fieldname = var.Identifier.ValueText
                if fieldname = null then failwith "bad var name"
                //printfn "mapping field %s" name
                let fieldname = mapName fieldname
                if fieldname = null then failwithf "failed to map name"

                let _debugOption, getDebugOpt = getDebugFieldOptions fieldname
                let inline mapNode node = mapNode spacing fieldNames getDebugOpt node // (memberNames:Set<string>) (getDebugOpt: DebugDelegate) (node:SyntaxNode)
                let (|EmptyInitializerMap|_|) (declaration: VariableDeclarationSyntax) =
                    match declaration.Type with
                    | null -> failwithf "declaration type was null"
                    | :? IdentifierNameSyntax as ins ->
                        match ins.Identifier.Text with
                        | "Visibility" -> Some "= Visibility.Visible"
                        | _ -> None
                    | :? PredefinedTypeSyntax as _pts -> Some "= Unchecked.defaultof<_>"
                    | :? QualifiedNameSyntax as _qns ->
                        None
                    | :? GenericNameSyntax as _gns ->
                        None
                    | x ->
                        printfn "unexpected type for declation.Type %s %A" (x.GetType().Name) x
                        None

                //let isTypeIdentifier = vDeclaration.Type :? IdentifierNameSyntax
                let initializer = if var.Initializer <> null then
                                        let initializer = mapNode var.Initializer
                                        initializer |> Some
                                  else
                                    match vDeclaration with
                                    |EmptyInitializerMap s -> Some s
                                    |_ -> None

                let result =
                    {
                        Type = toFType <| mapNode vDeclaration.Type
                        Name= fieldname
                        Initial =  initializer
                        Declaration = vDeclaration
                    }
                result
                )
            |> Seq.sortBy (fun f-> f.Name)
            |> Array.ofSeq

        let toFField (memberNames:Set<string>) (getDebugOpt:DebugDelegate) (fieldInfoB:FieldInfoB) =
            let name,type',initial,vDeclaration = mapName fieldInfoB.Name, fieldInfoB.Type, fieldInfoB.Initial, fieldInfoB.Declaration
            //printfn "starting field %s" name
            let debugLines expr lines:unit =
                let debugOpt,_ = translateOptions.GetNextDebugState getDebugOpt  (translateOptions.GetIsDebugNode expr)
                debugLines debugOpt lines //  let debugLines debug (lines:string seq) =
            let fDec init matchType =

                let comments =
                    seq {
                        if translateOptions.IncludeMatchTypeInComments then yield matchType
                        if translateOptions.IncludeOriginalInComments then yield sprintf "(%s)" (toFull vDeclaration)
                    } |> Array.ofSeq
                let comments = if Seq.isEmpty comments then String.Empty else comments |> delimit ";" |> sprintf " //%s"
                sprintf "let mutable %s : %s %s%s" name type' init comments
            match initial with
            |Some x when x = "string.Empty"|| x= "String.Empty"->
                fDec "= System.String.Empty " "(string.Empty-transform)"
            | Some x -> fDec x "mappedInitializer"
            |_ ->
                let _debugOpt,_getDebugOpt = translateOptions.GetNextDebugState getDebugOpt  (translateOptions.GetIsDebugNode (upcast vDeclaration))
                debugLines vDeclaration [
                    yield "fieldDefaultsChildNodes"
                    yield! (vDeclaration.ChildNodes() |> Seq.map (fun n -> (n :?> CSharpSyntaxNode).Kind().ToString()) |> Seq.map (fun m -> sprintf "  %s" m) |> List.ofSeq)
                    ]
                let result= fDec ("= null") "default init"
                result
        let fields = fields |> Seq.map (toFField fieldNames getDebugOpt) |> List.ofSeq
        fields

module PropConversion =
    open PureCodeGeneration.Declarations
    open System.Text.RegularExpressions

    let (|RegexMatch|_|) pattern input =
        let m = Regex.Match(input,pattern)
        if m.Success then
            Some m
        else None
    let toFProp translateOptions target spacing (memberNames:Set<string>) (pib:PropertyInfoB) (getDebugOpt:DebugDelegate) =
        let debugOpt, getDebugOpt = translateOptions.GetNextDebugState getDebugOpt (translateOptions.GetIsDebugPropPred pib.PropertyName)
        match target with
        |CodeGenTarget.Interface -> sprintf "member %s:%s" pib.PropertyName pib.Type
        |Class(propertyHandling, promote) ->
            let promote = match promote with |PromoteUnitializedStructsToNullables.Flag b -> b
            let mapNode = mapNode translateOptions promote spacing memberNames
            let fDec getter setter matchType=
                let spc = translateOptions.Spacing
                let declaration = sprintf "member x.%s //%s\r\n" pib.PropertyName matchType
                match getter,setter with
                | Some getter, Some setter -> sprintf "%s%swith get() = %s\r\n%sand set value = %s\r\n" declaration spc getter spc setter
                | Some getter, None -> sprintf "%s%swith get() = %s\r\n" declaration spc getter
                | None, Some setter -> sprintf "%s%swith set v = %s\r\n" declaration spc setter
                | None,None -> sprintf "//could not declare property %s" matchType
            let inline simpleSet fieldName = sprintf "%s <- v" fieldName
            let inline iNotifyProp fieldName propName =
                sprintf "%s;x.RaisePropertyChanged(<@ x.%s @>)" (simpleSet fieldName) propName // F# Quotations with INotifyPropertyChanged -> see also http://www.fssnip.net/4Q
            let getDebugPropOptions name = translateOptions.GetNextDebugState getDebugOpt  (translateOptions.GetIsDebugPropPred name)
            let inline debugPropLines name expr lines:unit =
                let debugOpt, getDebugOpt = getDebugPropOptions(name)

                let debugOpt,_getDebugOpt =if Option.isSome expr then translateOptions.GetNextDebugState getDebugOpt (translateOptions.GetIsDebugNode expr.Value) else debugOpt,getDebugOpt
                debugLines debugOpt lines
            let mapNode x :string = mapNode getDebugOpt x
            let mapAccessor type' (nodes:SyntaxNode[]) =

//                let dumpNodeKindInfo =
//                    nodes |> Seq.map(fun n -> (n :?> CSharpSyntaxNode).Kind()) |> (dumps <| sprintf "%s nodes for %s" type' pib.PropertyName <| debugOpt)
//                    |> ignore
                if Seq.isEmpty nodes then //autoprop
                    failwithf "map %s is not set up for empty nodes" type'
                let mapped = nodes |> Seq.map mapNode

                let mapped = delimit spacing mapped
                let mapped = if mapped.Contains("\r\n") then "\r\n" + mapped else mapped
                dumps "MapAccessorNodesResult" debugOpt mapped

            let mapGetter = mapAccessor "getter"
            let mapSetter = mapAccessor "setter"

            let mapAccessor map childnodes =
                childnodes
                |> Seq.toArray
                |> map
                |> replace "\r\n" (sprintf "\r\n%s" (spacing + spacing)) 
                |> Some
            let mapGetter (getter:AccessorDeclarationSyntax) = getter.ChildNodes()|> mapAccessor mapGetter
            let mapSetter (setter:AccessorDeclarationSyntax) = setter.ChildNodes()|> mapAccessor mapSetter
            let value = match pib.Type with | "bool" -> "false" | _ -> "null"
            match pib.Getter,pib.Setter with
            | AutoProperty ->
                debugPropLines pib.PropertyName None [sprintf "AutoProperty type,value:(%s,%s)" pib.Type value]
                let autoProp= sprintf "%smember val %s : %s = %s with get, set\r\n" spacing pib.PropertyName pib.Type value
                autoProp
            | Some getter, Some setter ->
                let getter = mapGetter getter
                let matchType,setter' =
                    let full = toFull setter
                    // for case insensitivity on the field name use @"set\s*{\s*this\.SetAndNotify\(\(\)\s*=>\s*this\.(?<name>\w+),\s*ref\s*_(?i)\k<name>,\s*value\);\s*}"
                    match full with
                    | RegexMatch @"set\s*{\s*this\.SetAndNotify\(\(\)\s*=>\s*this\.(?<name>\w+),\s*ref\s*_\k<name>,\s*value\);\s*}" m ->
                        let fieldName = match pib.FieldName with | Some fn -> fn | _ -> ("_" + m.Groups.[1].Value)
                        let debugPropLines = debugPropLines pib.PropertyName
                        let result =
                            match propertyHandling with
                            | InheritFsharpViewModule -> iNotifyProp fieldName pib.PropertyName
                            | KeepNotify -> iNotifyProp fieldName pib.PropertyName
                            | NoNotify -> simpleSet fieldName
                        let sSetter = setter :> SyntaxNode |> Some
                        debugPropLines sSetter [
                            fieldName
                            full
                            result
                        ]
                        "mapSetterMatchINotify", Some result
                    | _ ->
                        let setterText = mapSetter setter
                        "using existing getter and setter",setterText
                fDec getter setter' matchType
            | Some getter, None ->
                fDec (mapGetter getter) None "using existing getter"
            | _ -> sprintf "  // could not generate property for %A\r\n" pib.PropertyName

    let convertProperties (fIsDebugCode) translateOptions target spacing (fileInfoB:FileInfoB) =
        let _debugOpt,getDebugOpt = translateOptions.GetStartDebugState None (fIsDebugCode fileInfoB.File)
        let props promote = fileInfoB.Properties |> Seq.map(fun p -> {p with PropertyName=mapName p.PropertyName;Type=toFType promote p.Type}) |> Seq.sortBy ( fun p -> p.PropertyName)
        let memberNames = fileInfoB.getMemberNames() |> Set.ofList
        match target with
        | CodeGenTarget.Interface -> //target spacing (propertyNames:Set<string>) (pib:PropertyInfoB) (getDebugOpt:DebugDelegate)
            let f (prop:PropertyInfoB) = toFProp translateOptions target spacing memberNames prop getDebugOpt
            props false |> Seq.map f
        | CodeGenTarget.Class(_,promote) ->
            let promoteUninitializedStructsToNullable = (match promote with |Flag promote -> promote)
            let _toFType = toFType promoteUninitializedStructsToNullable
            let f (prop:PropertyInfoB) = toFProp translateOptions target spacing memberNames prop getDebugOpt
            props promoteUninitializedStructsToNullable |> Seq.map f



module FileConversion =
    type System.String with
        static member optionToStringOrEmpty (s:string option) = match s with Some text -> text | None -> String.Empty

    let convertFile (fIsDebugCode) translateOptions typeAttrs target (cls:FileInfoB) =
        let _debugOpt,getDebugOpt = translateOptions.GetStartDebugState None (fIsDebugCode cls.File)
        let convertProperties () = PropConversion.convertProperties (fIsDebugCode) translateOptions target translateOptions.Spacing cls

        match target with
        //| RecordBasedClass (propertyPrefs,promote)  -> String.Empty
        | Record ->
            let props = convertProperties()
            sprintf "type %sRecord={%s}" cls.ClassName (delimit ";" props)
        | CodeGenTarget.Interface ->
            let props = convertProperties()
            sprintf "type I%s =\r\n%s" cls.ClassName (props |> Seq.map (sprintf "abstract %s") |> delimit ";")
        | CodeGenTarget.Class (propertyPrefs,promote) ->
            let spacing = translateOptions.Spacing
            let classD = {
                ClassAttributes = typeAttrs
                Name=cls.ClassName
                BaseClass=
                    match propertyPrefs with
                    | PropertyOptions.InheritFsharpViewModule -> Some "  inherit FSharp.ViewModule.ViewModelBase()"
                    |_ -> if cls.Bases |> Seq.any then cls.Bases |> Seq.head |> sprintf "inherit %s()" |> Some else None
                Fields=  List.empty
                Members = List.empty
                Interfaces = List.empty
                }
            let classD =
                cls.Bases |> dumpt "bases" |> ignore
                let buildInterface() : ClassMember list =
                    [
                        ClassMember.Interface <| sprintf " interface %s\r\n" "System.ComponentModel.INotifyPropertyChanged"
                        ClassMember.Method "RaisePropertyChanged"
                    ]
                match propertyPrefs with
                | PropertyOptions.KeepNotify ->
                    {classD with Members = buildInterface() @ classD.Members}
                |_ -> classD

            let classD ={classD with Fields = FieldConversion.convertFileFields translateOptions promote spacing  cls getDebugOpt}
            let props = convertProperties() |> Seq.map (indent spacing)
            let filename = match cls.File with
                            |Code (Some path,_) -> match path with |PathF p -> p
                            | _ -> "unknown"
            let text = sprintf "%s\r\ntype %s() = // translated from %s\r\n%s\r\n\r\n" (classD.AttributeText()) cls.ClassName filename (translateOptions.Spacing + String.optionToStringOrEmpty classD.BaseClass)
            let text = new System.Text.StringBuilder(text)
            text
                .AppendLine(classD.FieldText spacing)
                .AppendLine(String.Empty)
                .AppendLine(delimit "\r\n" props).ToString()
