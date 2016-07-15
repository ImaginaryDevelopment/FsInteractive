module Macros.SqlMacros
open System
open BReusable
open BReusable.Railways

type NonBracketedName = NonBracketedName of string
    with
        member x.Value = match x with NonBracketedName.NonBracketedName s -> s
        override x.ToString() = x.Value
//shadow the constructor
let NonBracketedName input =
    if input |> contains "[" || input |> contains "]" then None
    else Some <| NonBracketedName(input)

type ObjectReference = {Schema:NonBracketedName;Name:NonBracketedName} with
    override x.ToString() = sprintf "[%O].[%O]" (x.Schema |> string) (x.Name |> string)
    static member TryCreate schema name :Railway<_,string>=
        match NonBracketedName schema, NonBracketedName name with
        | Some nbSchema, Some nbName -> Success {Schema=nbSchema;Name=nbName}
        | Some _, None -> Failure (sprintf "Could not read name '%s'" name)
        | None, Some _ -> Failure (sprintf "Could not read schema '%s'" schema)
        | None, None -> Failure (sprintf "Could not read schema nor name '%s','%s'" schema name)
type ConflictedObjectReference = {Violated:ObjectReference;Conflicted:ObjectReference} //ConflictSchema:NonBracketedName;ConflictName:NonBracketedName
type ObjectDefHolder() =
    member val Definition = String.Empty with get,set
    member val Object_Id = 0 with get,set
    member val ParentSchema = String.Empty with get,set
    member val ParentName = String.Empty with get,set

type ObjectManager = {R:ObjectReference; Add: string; Drop: string; ObjectId:int}

let getRawDisableTriggerText tblName (r:ObjectReference) :string =
    sprintf "DISABLE TRIGGER %s on %s" (r |> string) tblName
let getRawDropTriggerText (r:ObjectReference)= sprintf "DROP TRIGGER %s" (r |> string)
let getMetaText sr t =
    let schema,name = sr.Schema |> string, sr.Name |> string
    sprintf @"select
    o.[object_id]
    ,object_definition(o.[object_id]) as definition
    , object_schema_name(o.[object_id]) as [schema]
    , o.name
    , object_schema_name(p.[object_id]) as parentSchema
    , p.name as parentName
-- select *
from sys.objects o
join sys.objects p on o.parent_object_id = p.object_id
where
o.type = '%s'
and object_schema_name(o.[object_id]) = '%s'
and o.name = '%s'"
                                t schema name
let tryGetMeta (fExecuteQuery:string -> ObjectDefHolder seq) sr t=
    let text = getMetaText sr t
    fExecuteQuery text
    |> List.ofSeq
    |> function
        | [x] -> Success x
        | [] -> Failure (sprintf "No definition found for %s using %s" sr.Name.Value text)
        | x -> failwithf "More than one result in seq %A" x

let getObjExistsText sr t =
    let text = sprintf @"exists(select 1
 -- select *
from sys.objects o
where
    o.type = '%s'
    and object_schema_name(o.[object_id]) = '%s'
    and o.name = '%s')" t sr.Schema.Value sr.Name.Value
    text

let getIsSprocText (r:ObjectReference) =
    getMetaText r "P"
    |> sprintf "select case when EXISTS (%s) then 1 else 0 end"
let getDropSprocText (r:ObjectReference) : string =
    getIsSprocText r
    |> fun test -> sprintf "IF EXISTS(%s) \r\n  DROP PROCEDURE %s" test (r |> string)

let getIsTriggerText (r:ObjectReference) =
    let text =
        getMetaText r "TR"
        |> sprintf "select case when EXISTS (%s) then 1 else 0 end"
    text
let getDisableTriggerWithExists parentSchema parentName (r:ObjectReference) =
    let textTest = getObjExistsText r "TR"
    let textDisable = getRawDisableTriggerText (sprintf "[%s].[%s]" parentSchema parentName) r
    let text = sprintf "IF %s \r\n  %s" textTest textDisable
    text
let getDropTriggerWithExists (r:ObjectReference) =
    let textTest = getObjExistsText r "TR"
    let textDisable = getRawDropTriggerText r
    let text = sprintf "IF %s \r\n  %s" textTest textDisable
    text
