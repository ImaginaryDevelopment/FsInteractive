module DataModelToC

open System
open BReusable
open BReusable.StringHelpers

type Indentable = int*string
type Indentables = Indentable list
type Comments = Indentables
// for interfaces
type Property = {
            Name:string
            Type:string
            Comment:Comments
            IsWritable:bool
}

type ReadWriteType =
    | Readonly
    | Writable of field:string option

type PropertyGenType =
    | Auto of ReadWriteType
    | Custom of body:Indentables*field: Indentable option

// for classes
type PropertyGenArgument = {
    Name:string
    Type:string
    Comments:Comments
    PropertyGenType:PropertyGenType
}

module Impl = // generators without the indenter/delimiter
    let generateInheritanceDeclaration inheritance =
        inheritance
        |> StringHelpers.delimit ", "
        |> function
            |ValueString x ->
                sprintf " : %s" x
            | _ -> String.Empty
    let generateInterfaceDeclaration typeName inheritance =
        generateInheritanceDeclaration inheritance
        |> sprintf "public interface %s%s" typeName
    let generateClassDeclaration typeName inheritance =
        generateInheritanceDeclaration inheritance
        |> sprintf "public class %s%s" typeName
    let generateAutoProperty isPublic (prop:Property):Indentables =
        [
            yield! prop.Comment
            yield 1,
                if prop.IsWritable then
                    "{ get; set; }"
                else "{ get; }"
                |> sprintf "%s%s %s %s" (if isPublic then "public " else String.Empty) prop.Type prop.Name
        ]

    let generateBase (indenter:int*string -> string) =
        List.map indenter
        >> delimit Environment.NewLine
    let defaultIndenter indentation (i,x) = 
        String.replicate i indentation |> sprintf "%s%s" <| x

    type TypeGenerationType = 
        |Interface
        |Class
    let generateType tgt isReadOnly indenter props declaration =
        let usePublicProps = match tgt with |Interface -> false | Class -> true
        [
            yield
                0,declaration
            yield 0 ,"{"
            yield!
                props
                |> List.map (fun (prop:Property) ->
                    if isReadOnly && prop.IsWritable then {prop with IsWritable = false} else prop
                )
                |> List.collect(generateAutoProperty usePublicProps)
            yield 0,"}"
        ]
        |> generateBase indenter

open Impl

let generateInterface typeName inheritance isReadOnly props indenter =
    generateInterfaceDeclaration typeName inheritance
    |> generateType Interface isReadOnly indenter props

let generateClass typeName inheritance isReadOnly props indenter =
    generateClassDeclaration typeName inheritance
    |> generateType Class isReadOnly indenter props

let generateInpcClass typeName inheritance props indenter =
    let decl = generateClassDeclaration typeName ("INotifyPropertyChanged"::inheritance)
    let props =
        props
        |> List.map(
            // generate default get/set
            function
            |{PropType=AutoImplemented Writable} as prop ->
                let get = sprintf "get { return _%s; }" prop.Name
                let set =  [
                    2,"set"
                    2,"{"
                    3, sprintf "_%s = value;" prop.Name
                    3, sprintf "this.RaisePropertyChanged(%s)" prop.Name
                    2,"}"
                ]
                {prop with PropType = Body ([2,get],set)}

            | prop -> prop
        )
    generateType Class false indenter props decl
