module CodeGeneration.SqlScriptGeneration
// can generate sql, just doesn't include the Db query parts
open System
open System.Text
open BReusable
open BReusable.StringHelpers
open BCore.CodeGeneration
open BCore.CodeGeneration.DteWrapCore
open BCore.CodeGeneration.SqlWrapCore
open System.IO

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

// ColumnInfo looks superior to this, but perhaps this shape is needed somewhere specific
type ColumnDescription = {ColumnName:string; Type:string; Length:int; Nullable:bool; IsPrimaryKey:bool; IsIdentity:bool; IsComputed:bool}

// data that would be expected from querying the database or the sql table creation file
type TableDataMeta = {PrimaryKeys:Set<string>;ColumnDescriptions:ColumnDescription list; Identities:Set<string>}

let getTableGenerationData appendLine fSingularizer (fGetTableData:TableIdentifier -> TableDataMeta) (ti:TableIdentifier) =
    let typeName = fSingularizer ti.Name
    appendLine <| sprintf "Starting table %s as type %s" ti.Name typeName
    try
    let {PrimaryKeys=pks;ColumnDescriptions=columns; Identities=identities} = fGetTableData ti
    Rail.Happy( ti, typeName, pks, columns, identities)
    with ex ->
        Rail.Unhappy (ti,ex)

let formatFKey (table:string) column (fKey:FKeyIdentifier) : string =
    let fKeyColumn = if isNull fKey.Column then column else fKey.Column
    sprintf "CONSTRAINT [FK_%s_%s_%s_%s] FOREIGN KEY ([%s]) REFERENCES [%s].[%s] ([%s])" table column fKey.Table.Name fKeyColumn column fKey.Table.Schema fKey.Table.Name fKeyColumn

let formatDefaultValue (table:string) column defaultValue : string =
    sprintf "CONSTRAINT [DF_%s_%s] DEFAULT %s" table column defaultValue

open ColumnTyping
let mapTypeToSql =
    function
    | Custom ct ->
            ct
    | Bit -> "bit"
    | IntColumn -> "int"
    | DateTimeColumn -> "datetime"
    | DecimalColumn None -> "decimal"
    | DecimalColumn (Some ps) ->
        sprintf "decimal(%i,%i)" ps.Precision.P ps.Scale.S
    | Floater None -> "Numeric"
    | Floater (Some ps) ->
        sprintf "numeric(%i,%i)" ps.Precision.P ps.Scale.S
    | IdentityColumn -> "int"
    | StringColumn l -> sprintf "varchar(%i)" l
    | StringMax -> "varchar(MAX)"
    | NStringColumn length -> sprintf "nvarchar(%i)" length
    | NStringMax -> "nvarchar(MAX)"
    | UniqueIdentifier -> sprintf "uniqueidentifier"

let composeFKeyAndDefaultValue tableName columnName defaultValue fkeyOpt =
    let fkeyText =
        fkeyOpt
        |> Option.map (
            function
            |FKeyIdentifier fk -> fk
            | FKeyWithReference rd -> rd.FKeyId
            >> formatFKey tableName columnName
         )
         |> Option.getOrDefault null
    let defaultValueText = Option.ofObj defaultValue |> Option.map (formatDefaultValue tableName columnName) |> Option.getOrDefault null
    // apparently null string + null string is empty string
    match fkeyText, defaultValueText with
    | ValueString x, ValueString y -> sprintf "%s %s" x y
    | ValueString x, _ -> x
    | _, ValueString y -> y
    | _ -> null

let (|RefValueComments|_|) x =
        match x.FKey with
        | Some (FKeyWithReference rd) when not <| isNull rd.ValuesWithComment && rd.ValuesWithComment.Count > 0 ->
            Some rd.ValuesWithComment
        | _ -> None

let formatColumnComments doDiag appendLine appendLine' tableName ci =
    let multipleComments = ci.Comments.Length > 1
    if multipleComments then
        appendLine String.Empty
        ci.Comments
        |> Seq.map (fun c -> "-- " + c)
        |> delimit "\r\n"
        |> appendLine' 1
    let comment =
        match ci.Comments with
        | [comment] -> " -- " + comment |> Some
        // if it is empty, let it fall through for ref value comments checking
        | []
        | _ :: _ :: _ ->
            match ci with
            | RefValueComments rvc ->
                if doDiag then
                    printfn "found rvc for %s -> %s! %A" tableName ci.Name rvc
                " -- " + (delimit "," rvc.Keys) |> Some
            | _ -> None
        |> Option.getOrDefault null
    multipleComments,comment

let formatAttributes hasCombinationPK fKeyText nullability tableName (columnName, isUnique,isIdentity)=
    let isPk = match nullability with PrimaryKey -> true | _ -> false
    let allowNull = match nullability with AllowNull -> true | _ -> false

    // if it is a primary key it doesn't need the starter
    let starter = if allowNull then "null" else "not null"
    if not isIdentity && not isPk && not isUnique then
        starter + (if not <| String.IsNullOrEmpty fKeyText then " " + fKeyText else null)
    else
        [
                if isIdentity then yield "identity"
                if isPk && not hasCombinationPK then yield "primary key"
                if allowNull then yield "null"
                if not allowNull && (not isPk || (isPk && hasCombinationPK)) then yield "not null"
                if isUnique then yield "CONSTRAINT UQ_" + tableName + "_" + columnName + " UNIQUE"
                match fKeyText with
                | ValueString _ -> yield fKeyText
                | _ -> ()
        ]
        |> delimit " "
        |> trim

open SqlWrapCore
let generateColumn doDiag (tableId:TableIdentifier) appendLine appendLine' hasCombinationPK isLastColumn ci =
    let fKeyTextOpt = composeFKeyAndDefaultValue tableId.Name ci.Name ci.DefaultValue ci.FKey
    let hasMultipleComments,comment = formatColumnComments doDiag appendLine appendLine' tableId.Name ci
    let attribs =
        let isIdentity = match ci.ColumnType with | IdentityColumn -> true | _ -> false
        formatAttributes
            hasCombinationPK
            fKeyTextOpt
            ci.Nullability
            tableId.Name
            (ci.Name, (match ci.IsUnique with | Unique -> true | _ -> false), isIdentity)

    let result =
        // %-32s is padRight 32chars ' '
        sprintf "%-32s%-16s%s%s%s%s"
            (sprintf "[%s]" ci.Name)
            (mapTypeToSql ci.ColumnType)
            attribs
            //(if i < columnCount - 1 || hasCombinationPK then "," else String.Empty)
            (if isLastColumn || hasCombinationPK then "," else String.Empty)
            (if hasMultipleComments then Environment.NewLine else String.Empty)
            comment
    if doDiag then
        match ci.Name with
        | "PaymentItemStatusId" as columnName ->
            printfn "%s column diag: {commentCount:%i;result:%s}" columnName ci.Comments.Length result
        | _ -> ()
    result
    |> appendLine' 1
    ()

// this appears to be doing three things:  generating the table text, starting/stopping the manager file, adding it to the project. split
let generateTable doDiag (manager:IManager) (generationEnvironment:StringBuilder) targetProjectFolderOpt (tableInfo:TableGenerationInfo) =
//    printfn "Generating a table into %A %s" targetProjectFolderOpt tableInfo.Name
    let targetFilename = Path.Combine(defaultArg targetProjectFolderOpt String.Empty, "Schema Objects", "Schemas", tableInfo.Id.Schema, "Tables", tableInfo.Id.Name + ".table.sql")
    manager.StartNewFile(targetFilename)

    let appendLine text = generationEnvironment.AppendLine(text) |> ignore<StringBuilder>
    let appendLine' indentLevel text = String.replicate indentLevel "    " + text |> appendLine
    appendLine "-- Generated file, DO NOT edit directly"
    appendLine (sprintf "CREATE TABLE [%s].[%s] (" tableInfo.Id.Schema tableInfo.Id.Name)
    // SqlGeneration.ttinclude ~ 165
    // custom type, column type

    let mutable i = 0
    let columnCount = tableInfo.Columns.Length
    let hasCombinationPK =
        let pks = tableInfo.Columns |> List.filter (fun ci -> ci.Nullability |> function | PrimaryKey -> true | _ -> false) |> List.length
        1 <  pks

    tableInfo.Columns
    |> Seq.iter (fun ci ->
        generateColumn doDiag tableInfo.Id appendLine appendLine' hasCombinationPK (i < columnCount - 1) ci
        i <- i + 1
    )
    if hasCombinationPK then
        let columns =
            tableInfo.Columns |> Seq.filter(fun ci -> ci.IsPrimaryKey)|> Seq.map(fun ci -> ci.Name)
            |> delimit ","

        sprintf "CONSTRAINT PK_%s PRIMARY KEY (%s)" tableInfo.Id.Name columns
        |> appendLine' 1
    appendLine ");"
    manager.EndBlock()

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

let getOnColumnExists (table:Table) column=
    let fDropClause printBeforeDropping=
        seq {
            if printBeforeDropping then
                yield 0, print Text (sprintf "dropping %s.%s.%s" table.Schema table.Name column)
            yield 0, getDropper (Droppable.Column (table,column)) // sprintf "alter table %s.%s drop column %s" schema table column
            }
    let ifClause = sprintf "if exists( select 1 from INFORMATION_SCHEMA.columns c where c.TABLE_SCHEMA='%s' and c.TABLE_NAME='%s' and c.COLUMN_NAME='%s')" table.Schema table.Name column
    getOnExists ifClause fDropClause

let getOnConstraintExists (table:Table) name =
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

let getOnTableExists (table:Table) =
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


type TitledReferenceData =
    | TitledReferenceData of title:string * (ReferenceData list)
let generateReferenceInsert appendLine =
    function
    | TitledReferenceData(title, referenceData) ->
        appendLine "-- Generated file, DO NOT edit directly"
        appendLine "SET ANSI_NULLS ON"
        appendLine "GO"
        appendLine "SET QUOTED_IDENTIFIER ON"
        appendLine "GO"
        appendLine (sprintf "PRINT 'Starting %s Synchronization'" title)
        appendLine "GO"
        referenceData
        |> Seq.iter (fun rd ->
            printfn "Starting table %s" rd.FKeyId.Table.Name
            let cteName = sprintf "CTE_%s" rd.FKeyId.Table.Name
            appendLine "---------------------------------------------------"
            appendLine (sprintf "PRINT 'Synchronizing [%s.%s]';" rd.FKeyId.Table.Schema rd.FKeyId.Table.Name)
            appendLine (sprintf "WITH %s(%s) AS" cteName rd.FKeyId.Column)
            appendLine "("
            appendLine (sprintf "    SELECT [%s]" rd.FKeyId.Column)
            appendLine "    FROM (VALUES"
            let mutable i = 0
            let valueCount = rd.ValuesWithComment.Keys.Count
            for k in rd.ValuesWithComment.Keys do
                let comment = match rd.ValuesWithComment.[k] with
                                |null -> String.Empty
                                |k -> " -- " + k
                appendLine (sprintf "        ('%s')%s%s" (k |> replace "'" "''") (if i < valueCount - 1 then "," else ")" ) comment )
                i <- i + 1
            appendLine (sprintf "        AS SOURCE([%s])" rd.FKeyId.Column)
            appendLine ")"
            appendLine (sprintf "MERGE INTO [%s].[%s] AS TARGET" rd.FKeyId.Table.Schema rd.FKeyId.Table.Name)
            appendLine (sprintf  "USING %s" cteName)
            appendLine (sprintf "ON %s.[%s] = TARGET.[%s]" cteName rd.FKeyId.Column rd.FKeyId.Column )
            appendLine "WHEN NOT MATCHED BY TARGET THEN"
            appendLine (sprintf "    INSERT([%s])" rd.FKeyId.Column)
            appendLine (sprintf "    VALUES([%s]);" rd.FKeyId.Column)
            appendLine String.Empty
            appendLine (sprintf "PRINT 'Done Synchronizing [%s.%s]';" rd.FKeyId.Table.Schema rd.FKeyId.Table.Name)
            appendLine "GO"
            appendLine String.Empty
        )

let generateInserts appendLine (manager:IManager) targetProjectFolder (tables:#seq<_>) igc =
    printfn "Starting inserts, template text length: %i" (manager.GetTextSize())
    // generate reference data
    let toGen =
        tables
        |> Seq.filter(fun t ->
            t.Columns
            |> Seq.exists(fun c ->
                match c.FKey with
                | Some(FKeyIdentifier _) -> false
                | Some(FKeyWithReference rd) -> rd.ValuesWithComment |> Seq.exists (fun _ -> true)
                | _ -> false
            )
        )
        |> List.ofSeq

    printfn "Generating for %i parent tables (%A)" toGen.Length (toGen |> Seq.map (fun t -> t.Id.Name) |> List.ofSeq)
    if not <| Seq.any toGen && not <| Seq.any igc.AdditionalReferenceData then
        ()
    else
        let targetFilename = Path.Combine(targetProjectFolder, igc.TargetInsertRelativePath)
        manager.StartNewFile targetFilename
        let refData =
            let addlRefData = igc.AdditionalReferenceData |> List.ofSeq
            toGen
            |> Seq.map (fun tbl ->
                let refData =
                    tbl.Columns
                    |> Seq.choose(fun c -> match c.FKey with | Some(FKeyWithReference rd) -> Some(c.Name, rd) | _ -> None)
                    |> Seq.filter (fun (cName,rd) -> rd.ValuesWithComment |> isNull |> not && rd.ValuesWithComment |> Seq.any )
                    |> Seq.map (fun (cName,rd) ->
                        if rd.FKeyId.Column |> isNull then
                            {rd with FKeyId = {rd.FKeyId with Column = cName}}
                        else rd
                    )
                    |> List.ofSeq
                refData
            )
            |> Seq.collect id
            |> List.ofSeq
            |> flip (@) addlRefData

        generateReferenceInsert appendLine (TitledReferenceData (igc.InsertTitling, refData))
        manager.EndBlock()
        printfn "Done with inserts, template text length: %i" (manager.GetTextSize())
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
