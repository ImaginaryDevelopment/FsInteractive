module Macros.SqlMacros
open System
open System.Data
open BReusable
open BReusable.StringHelpers
open System.Diagnostics

type TableIdentifier = {Schema:string; Name:string;}
//type FKeyInfo = {Schema:string; Table:string; Column:string}
type FKeyIdentifier = {Table: TableIdentifier; Column:string}
type ColumnLength = |Max | Length of int
type DecimalInfo = {Precision:int; Scale:int}

[<NoComparison>]
type SqlColumnType =
    |Decimal of DecimalInfo option
    |VarChar of ColumnLength
    |NVarChar of ColumnLength
    |Char of ColumnLength
    |NChar of ColumnLength
    |Other of Type

type Nullability =
    | AllowNull
    | NotNull
    | Computed of isNullable:bool
    | PrimaryKey
    with member x.IsNullable = match x with |AllowNull -> true |Computed a -> a | _ -> false
type Uniqueness =
    | Unique
    | NotUnique

[<NoComparison>]
type ColumnInfo =
    { 
        Name:string
        SqlType:SqlColumnType
        AllowNull:Nullability
        Attributes: string list
        IsUnique: Uniqueness
        FKeyOpt:FKeyIdentifier option
    }
    with
        static member Zero ct =
            {Name=null; SqlType = ct; AllowNull = NotNull;IsUnique=Uniqueness.NotUnique; Attributes = List.empty; FKeyOpt = None}
[<NoComparison>]
type TableInfo = { Id:TableIdentifier; Columns: ColumnInfo list}
// ColumnInfo looks superior to this, but perhaps this shape is needed somewhere specific
type ColumnDescription = {ColumnName:string; Type:string; Length:int; Nullable:bool; IsPrimaryKey:bool; IsIdentity:bool; IsComputed:bool}

module Strict = 
    open BReusable.StringHelpers

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
        static member TryCreate schema name :Rail<_,string>=
            match NonBracketedName schema, NonBracketedName name with
            | Some nbSchema, Some nbName -> Happy {Schema=nbSchema;Name=nbName}
            | Some _, None -> Unhappy (sprintf "Could not read name '%s'" name)
            | None, Some _ -> Unhappy (sprintf "Could not read schema '%s'" schema)
            | None, None -> Unhappy (sprintf "Could not read schema nor name '%s','%s'" schema name)
    type ConflictedObjectReference = {Violated:ObjectReference;Conflicted:ObjectReference} //ConflictSchema:NonBracketedName;ConflictName:NonBracketedName
    type ObjectDefHolder() =
        member val Definition = String.Empty with get,set
        member val Object_Id = 0 with get,set
        member val ParentSchema = String.Empty with get,set
        member val ParentName = String.Empty with get,set

    type ObjectManager = {R:ObjectReference; Add: string; Drop: string; ObjectId:int}


let dbNullToOption (x:obj) : obj option=
        if System.DBNull.Value.Equals x then
            None
        else Some x

module Seq = 
    open System.Data
    // requires f instead of just turning r into a Seq, to help guard/remind that typically, the source is:  one way, one iteration limited
    let unfoldRows f (r:IDataReader) =
        r |> Seq.unfold(fun r ->
            if r.Read() then
                Some(r :> IDataRecord, r)
            else None
        )
        |> Seq.map f
        |> List.ofSeq

let getTableData cn (tableIdentifier:TableIdentifier) =
    let cmdText = sprintf "sp_help '%s.%s'" tableIdentifier.Schema tableIdentifier.Name
    use cmd = new System.Data.SqlClient.SqlCommand(cmdText,cn)
    use r = 
        try
            cmd.ExecuteReader()
        with ex ->
            let ex = InvalidOperationException(sprintf "cmdText: %s" cmdText, ex)
            ex.Data.Add("tableIdentifier.Table", tableIdentifier.Name)
            raise ex

    r.NextResult() |> Debug.Assert // ignore the first table
    let columns =
        r
        |> Seq.unfoldRows (fun r ->
            let columnName = r.["Column_name"] |> string
            let type' = r.["Type"] |> string
            let computed = r.["Computed"] |> string
            let length = Convert.ToInt32 r.["Length"]
            // var prec = r["Prec"];
            {ColumnName=columnName; Type= type'; Length=length; Nullable = r.["Nullable"].ToString() ="yes"; IsIdentity = false; IsPrimaryKey = false; IsComputed = computed = "yes"}
        )

    r.NextResult() |> Debug.Assert

    let identities =
        r
        |> Seq.unfoldRows (fun r ->
            match r.["Seed"] |> dbNullToOption with
            | Some _ -> r.["Identity"] |> string |> Some
            | None -> None
        )
        |> Seq.choose id
        |> Set.ofSeq
    // rowGuidCol result set
    r.NextResult() |> Debug.Assert
    //Data_located_on_filegroup
    r.NextResult() |> Debug.Assert
    //indexes
    r.NextResult() |> Debug.Assert
    //multi-row constraints (some rows are more data from the previous row
    r.NextResult() |> Debug.Assert
    let (|PrimaryKey|_|) (r:IDataRecord) =
        r.["constraint_type"]
        |> dbNullToOption
        |> Option.map string
        |> function
            | None -> None
            | Some s ->
                match s with
                | ValueString ct -> if ct |> startsWithI "PRIMARY KEY" then Some() else None
                | _ -> None
    let pks = 
        seq {
            while r.Read() do // have not checked what this does on a table with a composite primary key
                match r with
                | PrimaryKey -> yield r.["constraint_keys"] |> string
                | _ -> ()
        }
        |> Set.ofSeq
    pks, columns, identities

open Strict

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
        | [x] -> Happy x
        | [] -> Unhappy (sprintf "No definition found for %s using %s" sr.Name.Value text)
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