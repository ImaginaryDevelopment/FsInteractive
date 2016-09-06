module CodeGeneration.SqlScriptGeneration
open System
open System.Text
open BReusable

let flip f y x = f x y
let append (text:string) (sb:StringBuilder) = sb.Append text
let indent i (indentValue:string) = List.replicate i indentValue |> delimit String.Empty
let indent4 i = indent i "    "
let appendLine (text:string) (sb:StringBuilder,i) = sprintf "%s%s" (indent4 i) text |> sb.AppendLine |> fun sb -> (sb,i)
let appendLine' j (text:string) (sb:StringBuilder,i) = appendLine text (sb, i + j) |> ignore; (sb,i)
let indentSb (sb,i) = sb, i + 1
let deIndentSb (sb,i) = sb, i - 1
let appendLines (lines: string seq) (sb:StringBuilder,i) = lines |> Seq.fold(flip appendLine) (sb,i)
let appendIndentLines (map: (int*string) seq) (sb:StringBuilder,i) = map |> Seq.fold (fun (sb,i) (j,l) -> appendLine' j l (sb,i)) (sb,i)
    //lines |> Seq.fold(appendLine i |> flip) sb
    //lines |> Seq.iter (flip appendLine sb >> ignore<StringBuilder>)
    //sb
type ObjIdentifier = string
type Column = ObjIdentifier
type SqlObjIdentifier = {Schema:string; Name:string;}
type Table = SqlObjIdentifier
type SqlObj = 
    | Table of Table
    | Column of name:ObjIdentifier

//type SqlIdentifier = 
//    | TableId of SqlObjIdentifier
//    | Other of SqlObjIdentifier * TableName:string

type ExistsAction =
    | Drop of printBeforeDropping:bool
    | Expression of lines:(seq<int*string>)*toPrintBeforeStarting:string option

[<RequireQualifiedAccess>]
type Droppable = 
    | Table of Table
    | Column of Table*Column
    | Trigger of string
    | Constraint of Table*string
//    | Trigger of SqlObjIdentifier
//    | Column of SqlObjIdentifier * tableName:string
//    | Table of SqlObjIdentifier
//    | Constraint of SqlObjIdentifier * tableName:string
//type SqlIdentifier with
//    static member toDroppable = 
//        function
//        | Table t -> TableId t
//        | Other o -> 
        


let includeFile = sprintf ":r %s"

let getDropper = 
    function 
    | Droppable.Table t -> sprintf "drop table %s.%s" t.Schema t.Name
    | Droppable.Column (t,c) -> sprintf "alter table %s.%s drop column %s" t.Schema t.Name c
    | Droppable.Trigger n -> sprintf "drop trigger %s" n
    | Droppable.Constraint (t,c) -> sprintf "alter table %s.%s drop constraint %s" t.Schema t.Name c

type PrintTypes = 
    |SqlExpression
    |Text 
let print pt = 
    match pt with 
    |SqlExpression -> sprintf "print %s"
    |Text -> fun text -> text |> replace "'" "''" |> sprintf "print '%s'"

let getOnExists ifClause fDropClauses existsAction elseActionsOpt : seq<int*string>= 
    let trueClauses = 
        match existsAction with
        | Drop printBeforeDropping ->
            fDropClauses printBeforeDropping 
        | Expression (lines,textBeforeStartOpt) -> [textBeforeStartOpt|> Option.map(fun t -> 0,t)]@(lines |> Seq.map Some |> List.ofSeq) |> Seq.choose id
        |> List.ofSeq
    seq{
        yield 0,ifClause
        yield 0,"begin"
        yield! trueClauses |> Seq.map (fun (i,c) -> i+1,c)
        match elseActionsOpt with 
        | Some elseClauses -> 
            yield 0,"end" 
            yield 0,"else" 
            yield 0,"begin" 
            yield! elseClauses |> Seq.map (fun c -> 1, c)
        | None -> ()
        yield 0,"end"
    }
    |> List.ofSeq
    |> Seq.ofList

let getOnColumnExists table column= 
    let fDropClause printBeforeDropping= 
        seq {
            if printBeforeDropping then
                yield 0, print Text (sprintf "dropping %s.%s.%s" table.Schema table.Name column)
            yield 0, getDropper (Droppable.Column (table,column)) // sprintf "alter table %s.%s drop column %s" schema table column
            }
    let ifClause = sprintf "if exists( select 1 from INFORMATION_SCHEMA.columns c where c.TABLE_SCHEMA='%s' and c.TABLE_NAME='%s' and c.COLUMN_NAME='%s')" table.Schema table.Name column
    getOnExists ifClause fDropClause
     
let getOnConstraintExists table name =
    let fDropClause printBeforeDropping = 
        seq{
            if printBeforeDropping then
                yield 0, print Text (sprintf "dropping %s.%s.%s" table.Schema table.Name name)
            yield 0, getDropper (Droppable.Constraint (table,name))
        }
    let ifClause = sprintf "if exists( select 1 from sys.objects WHERE SCHEMA_NAME(schema_id) = '%s' and OBJECT_NAME(parent_object_id) = '%s' and OBJECT_NAME(OBJECT_ID) = '%s' and type_desc LIKE '%%CONSTRAINT')" table.Schema table.Name name
    getOnExists ifClause fDropClause

let getOnTriggerExists schema name = 
    let fDropClause printBeforeDropping= 
         seq {
            if printBeforeDropping then
                yield 0, print Text (sprintf "dropping %s.%s" schema name)
            yield 0, getDropper (Droppable.Trigger (sprintf "%s.%s" schema name))
            }
    let ifClause = sprintf "if exists(select 1 from sys.objects o where o.type='TR' and object_schema_name(o.[object_id]) = '%s' and o.name = '%s')" schema name
    getOnExists ifClause fDropClause 

let getOnTableExists table = 
    let fDropClause printBeforeDropping =
         seq {
            if printBeforeDropping then
                yield 0, print Text (sprintf "dropping %s.%s" table.Schema table.Name)
            yield 0, getDropper (Droppable.Table table)
            }
    let ifClause = sprintf "if exists( select 1 from INFORMATION_SCHEMA.columns c where c.TABLE_SCHEMA='%s' and c.TABLE_NAME='%s')" table.Schema table.Name
    getOnExists ifClause fDropClause 

let renameTableIfExists table nextName = 
    getOnTableExists table (ExistsAction.Expression(seq [0,sprintf "exec sp_rename '%s', '%s'" (sprintf "%s.%s" table.Schema table.Name) (sprintf "%s.%s" nextName.Schema nextName.Name)],None)) 

// (StringBuilder(),0) |> appendIndentLines (getOnConstraintExists {Schema="dbo";Name="tempLanguages"} "DF_tempLanguages_IsHidden" (ExistsAction.Drop true) None) |> fst |> string;;
// (StringBuilder(),0) |> appendIndentLines (getOnTableExists {Schema="dbo";Name="tempLanguages"} (ExistsAction.Expression(seq [0,sprintf "exec sp_rename '%s' '%s'" "tempLanguages" "Language"],None)) None) |> fst |> string;;

//
//let generatePreCompareScript () = 
//    (StringBuilder(),0)
//    |> appendLine "-- run this BEFORE the sqlpackage.exe can do the compare"
//    |> appendLine (includeFile ".\FixClaims.sql")
//    |> appendLine (includeFile ".\FixGuarantorProfiles.sql")
//    |> appendLine (includeFile ".\FixClaimLevelTypes.sql")
//    |> appendLine ("starting pre-compare script"|> print Text)
//    |> appendLine "go"
//    |> appendIndentLines (getOnTriggerExists "dbo" "LookForBadGuys" (ExistsAction.Drop true) None)
//    |> appendLine "go"
//    |> appendIndentLines(
//        let t = {Schema="dbo";Name="Facilities"} 
//        let badC = "RequireRefferalSource" 
//        let goodC ="RequireReferralSource"
//        let ifGoodExists = ExistsAction.Expression ( [0,getDropper (Droppable.Column (t,badC))], None)
//        let ifGoodDoesntExist = Some [ "exec sp_rename 'dbo.Facilities.RequireRefferalSource', 'RequireReferralSource', 'COLUMN';"]
//        let getGoodExists = getOnColumnExists t goodC ifGoodExists ifGoodDoesntExist
//        
//        let ifbadExists = getOnColumnExists t badC (ExistsAction.Expression (getGoodExists,None)) None
//        ifbadExists
//        )
//    |> appendIndentLines (
//        [
//            "UpdatePathfinder"
//            "PMXpressMoveNewEncounter"
//            "PathfinderUpdateOnAdmit"
//            "PathfinderUpdateOnAdmit"
//            "UpdatePathfinder"
//            "PMXpressMoveNewEncounter"
//            "PathfinderSendCharges"
//            "PMXpressMoveNewPatient"
//            "ResetIsChecked"
//        ]
//        |> Set.ofSeq // takes out duplicates
//        |> Seq.collect (fun n -> getOnTriggerExists "dbo" n (ExistsAction.Drop true) None)
//        )
//    |> fst
//    |> string
