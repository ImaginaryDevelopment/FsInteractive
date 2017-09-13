﻿module CodeGeneration.GenerateJS

open BReusable.Reflection
open Macros.SqlMacros
module TypeScript = 
    open System
    module Option = 
        let ofBool =
            function
            | true -> Some()
            | false -> None

    let private n = typedefof<Nullable<_>>
    let private o = typedefof<Option<_>>
    // consider adding sproc input/output type interface generation too
    //type Cgsm2 = {
    //    CString:string
    //    ColumnBlacklist:Map<string, string Set>
    //}
    //type JsInput = 
    //    | FromTables of TableIdentifier list * connectionString:string
    //    | Types of Type list

    // all primitives besides char and bool can be mapped to numberish
    let numberTypes = [
        typeof<byte>
        typeof<double> // double = float in F#
        typeof<int16>
        typeof<int32>
        typeof<int64>
        // not sure it makes sense to map this type
        //typeof<nativeint>
        // forward compatibility with int 128?
        typeof<int> // int = int32 in the current compiler/OS
        typeof<sbyte>
        typeof<single>
        typeof<uint16>
        typeof<uint32>
        typeof<uint64>
        // not a primitive
        typeof<decimal>
    ]

    // only types that are directly number mapped, not nullables or options of those types
    let (|JSNumber|_|) (x:Type) =
        numberTypes
        |> Seq.exists ((=) x)
        |> Option.ofBool

    // matches if the type is Option<_> or Nullable<_>
    let(|MaybeIsh|_|) (x:Type) = 
        if not x.IsGenericType then 
            None
        else
            if x.GenericTypeArguments.Length <> 1 then
                None
            elif x.GetGenericTypeDefinition() = n || x.GetGenericTypeDefinition() = o then
                Some x.GenericTypeArguments.[0]
            else None

    let (|JSNumberish|_|) (x:Type) = 
        match x with
        | JSNumber -> Some()
        | MaybeIsh JSNumber -> Some()
        | _ -> None

    let (|Dateish|_|) x = 
        match x with
        | TypeOf (isType:DateTime) -> Some()
        | MaybeIsh(TypeOf(isType:DateTime)) -> Some()
        | _ -> None

    let (|Number|Date|Bool|Obj|) = 
        function
        | JSNumberish -> Number
        | Dateish -> Date
        | TypeOf(isType:bool)
        | MaybeIsh(TypeOf(isType:bool)) -> Bool
        | t ->  Obj t
    let mapType =
        function
        | JSNumber ->
            "number"
        | TypeOf(isType:DateTime)-> "Date"
        | TypeOf(isType:string) -> "string"
        | t -> t.Name

    let mapTypeIsh =
        function
        | MaybeIsh t ->
            mapType t
            |> sprintf "?:%s" 
        | t -> 
            mapType t


    type NamedProp = { Name:string; Type:Type}
    let generateProp =
        function
        |{NamedProp.Name=name;Type=t} ->
            mapTypeIsh t
            |> sprintf "%s%s" name

    let generateInterface indent props = 
        props
        |> Seq.map generateProp
        |> Seq.map(sprintf "%s%s" indent)