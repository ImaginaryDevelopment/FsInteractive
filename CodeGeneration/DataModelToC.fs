module DataModelToC

open System
open BReusable
open BReusable.StringHelpers

type Property = {
            Name:string
            Type:string
            Comment:string
            IsWritable:bool
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
    let generateProperty isPublic prop =
        [
            if isValueString prop.Comment then
                yield 1, sprintf "// %s" prop.Comment
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
                |> List.collect(generateProperty usePublicProps)
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
