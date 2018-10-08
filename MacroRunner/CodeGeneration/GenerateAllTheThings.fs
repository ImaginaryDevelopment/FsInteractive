module CodeGeneration.GenerateAllTheThings
// purpose: run SqlMeta.generateSql >> DataModelToF.generate
open System
open System.Collections.Generic
open System.Diagnostics
//open Microsoft.VisualStudio.TextTemplating

//open MacroRunner
//open MacroRunner.DteWrap
//open MacroRunner.MultipleOutputHelper.Managers
//open Macros.SqlMacros
open CodeGeneration
open BCore.CodeGeneration.SqlWrapCore.ColumnTyping

open CodeGeneration.DataModelToF
open BReusable.StringHelpers
open CodeGeneration.GenerateJS.TypeScript
open BCore.CodeGeneration.SqlWrapCore
open BCore.CodeGeneration.DteWrapCore


let failing s=
    if Debugger.IsAttached then
        Debugger.Log(1,"fail", s)
        Debugger.Break()
    failwith s

type ColumnInput with
    static member create name columnType =
        {Name=name; ColumnType=columnType; Nullability=Nullability.NotNull; FKey= None; Comments = List.empty; IsUnique=NotUnique; DefaultValue=null;}
    static member createPKIdentity name = {ColumnInput.create name IdentityColumn with Nullability = PrimaryKey}
    static member createFKey name columnType fkeyInfo = {ColumnInput.create name columnType with FKey = Some fkeyInfo}
    static member createFKeyedInt name fkeyInfo = ColumnInput.createFKey name ColumnType.IntColumn fkeyInfo
    static member createPatientIdColumn prefix allowNull comments =
        ColumnInput.createFKeyedInt (prefix + "PatientID") (FKeyIdentifier {Table={Schema="dbo"; Name="Patients"}; Column ="PatientID"})
        |> fun x -> {x with Comments = comments; Nullability = allowNull}
    static member createUserIdColumn prefix allowNull comment =
        ColumnInput.createFKeyedInt (prefix + "UserID") (FKeyIdentifier {Table={Schema="dbo"; Name="Users"}; Column="UserID" })
        |> fun x -> {x with Comments = comment; Nullability= allowNull}
    static member makeNullable50 name =
        {Name=name; ColumnType=ColumnType.StringColumn 50; DefaultValue=null; Nullability = Nullability.AllowNull; FKey = None;Comments = List.empty; IsUnique=Uniqueness.NotUnique}

type TableInput() =
     member val Name:string = Unchecked.defaultof<_> with get,set
     member val Schema:string = Unchecked.defaultof<_> with get,set
     member val Columns:ColumnInput seq = Unchecked.defaultof<_> with get,set

[<NoComparison>]
type SqlGenerationConfig = { TargetSqlProjectName: string; SqlItems: TableInput list; InsertionConfig: InsertsGenerationConfig option}

[<NoComparison>]
type GenMapTableItem =
    | DataModelOnly of TableIdentifier
    | Detailed of TableGenerationInfo
    with
        static member GetTI =
            function
                |DataModelOnly ti -> ti
                |Detailed x -> x.Id

// although InsertsGenerationConfig doesn't apply to TableGenerationInfo, let them be grouped for convenience
//type SqlGenerationism =
//    | Focused of InsertsGenerationConfig * (GenerationTarget list)
//    | Multiple of (InsertsGenerationConfig * GenerationTarget list) list

type IGenWrapper =
    abstract member GetProjects: unit -> IProject seq
    abstract member GetTargetSqlProjectFolder: string -> IProject


let generateCode generatorId cgsm manager (fGetSqlMeta,sqlSprocs,fMapSprocParams) sb mappedTables fSettersCheckInequality =
    let metaFallback (ti:TableIdentifier) (_exn:exn) =
        mappedTables
        |> Seq.find(function | Detailed details -> details.Id = ti | DataModelOnly dmTI -> dmTI = ti)
        |> function
            |Detailed details ->
                // this is what should be happening on the main path too
                {   SqlTableMeta.TI = details.Id
                    TypeName=cgsm.Singularize details.Id.Name
                    PrimaryKeys=details.Columns |> Seq.choose (fun c -> if c.IsPrimaryKey then Some c.Name else None) |> Set.ofSeq
                    Identities= details.Columns |> Seq.choose (fun c -> if c.IsIdentity then Some c.Name else None) |> Set.ofSeq
                    Columns= SqlTableColumnChoice.Manual details.Columns
                }
            | DataModelOnly _dmInput ->
                invalidOp "Could not get sql meta"
    let ga = {MetaFallback = Some metaFallback;GetSqlMeta=fGetSqlMeta; SqlSprocs=sqlSprocs;MapSqlSprocParams=fMapSprocParams;  }
    let meta =
        DataModelToF.generate generatorId
            ga
            cgsm
            (manager, sb, mappedTables |> List.map GenMapTableItem.GetTI)
            fSettersCheckInequality
        // why do we need this, it shouldn't happen I'd think?
        |> List.distinctBy(fun x -> x.TI)
    let getGenMapItem x = mappedTables |> List.find(GenMapTableItem.GetTI >> (=) x.TI)
    let mapColumnInput (c:ColumnInput) = PropSource.Custom(c.Name, c.Nullability.IsNullable, c.ColumnType.ToString())
    let fIncludeColumn tableName columnName : bool = isNull (box cgsm.ColumnNolist) || cgsm.ColumnNolist |> Map.containsKey tableName |> not || cgsm.ColumnNolist.[tableName] |> Seq.contains columnName |> not
    cgsm.TypeScriptGenSettingMap
    |> Option.iter(fun tsgsm ->
        match meta with
        | [] -> ()
        | meta ->
            let targetProjectFolder =
                manager.DteWrapperOpt |> Option.map (fun dte -> dte.GetProjects())
                |> Option.map (Seq.find (fun p -> p.GetName() = tsgsm.TargetProjectName))
                |> Option.map (fun tp -> tp.GetFullName() |> Path.GetDirectoryName)
                |> Option.map (fun fp -> match tsgsm.TargetFolderOpt with | Some x -> Path.Combine(fp,x) | None -> fp)
                |> function
                    | Some tpf -> tpf
                    | None -> failwithf "Could not locate ts target %A" tsgsm.TargetProjectName
            manager.StartNewFile (Path.Combine(targetProjectFolder, "generated.ts"))
            meta
            |> List.map(fun x -> x.TI, getGenMapItem x, x)
            |> List.iter(
                function // need tableIdentifier and column props
                | ti, _, {Columns=SqlTableColumnMeta stcm} ->
                    ti, stcm |> List.map(fun c -> PropSource.Custom(c.ColumnName, c.Nullable, c.Type))
                | ti, _, {SqlTableMeta.Columns=Manual columns}
                | ti, Detailed {Columns=columns}, _ -> ti, columns |> List.map mapColumnInput
                >> fun (ti, columns) -> ti, columns |> List.filter (fun c -> fIncludeColumn ti.Name c.Name)
                >> fun (tid,c) -> GenerateJS.TypeScript.generateInterface ((if tid.Schema = "dbo" then String.Empty else tid.Schema) + tid.Name) String.Empty "  " c
                >> (fun text ->
                    sb.AppendLine(text) |> ignore
                )
            )
            manager.EndBlock()
            ()
    )

    //feature desired: auto-name primary keys
    // adjustment desired: put all reference values comment (when on the reference table, above the column instead of beside it
let generateTablesAndReferenceTables(manager:IManager, generationEnvironment:System.Text.StringBuilder, targeting, toGen: TableGenerationInfo seq,debug) =
    toGen
    |> Seq.iter(fun ti ->
        SqlScriptGeneration.generateTable debug manager generationEnvironment targeting ti
        ti.Columns
        |> Seq.choose(fun ci->
            match ci.FKey with
            | Some (FKeyWithReference rd) when rd.GenerateReferenceTable ->
                Some (rd, {ci with FKey = None})
            | _ -> None
        )
        // iterate reference table columns
        |> Seq.iter(fun (fKey,childCi) ->
            try
                let refPKComment =
                    if childCi.Comments.Length > 0 then
                        childCi.Comments
                    elif fKey.ValuesWithComment |> isNull |> not && fKey.ValuesWithComment.Count > 0 then [fKey.ValuesWithComment.Keys |> delimit"," ]
                    else []
                let pkeyColumn = {childCi with Nullability = PrimaryKey; FKey=None; Comments= refPKComment}
                let tId = fKey.FKeyId.Table
                let table = {TableGenerationInfo.Id={Schema = tId.Schema; Name=tId.Name}; Columns = [pkeyColumn]}
                if debug then
                    printfn "Generating ReferenceTable from %A" table
                SqlScriptGeneration.generateTable true manager generationEnvironment targeting table
            with _ ->
                if debug then
                    printfn "Failing:"
                try printfn " fKey: %A" fKey with _ -> ()
                try printfn " childCi: %A" childCi with _ -> ()
                reraise()
        )
    )

/// generatorId something to identify the generator with, in the .tt days it was the DefaultProjectNamespace the .tt was running from.
let runGeneration generatorId debug (sb: System.Text.StringBuilder) (dte: IGenWrapper) manager (cgsm: CodeGeneration.DataModelToF.CodeGenSettingMap) toGen (dataModelOnlyItems: TableIdentifier list) (fGetSqlMeta,fGetSqlSprocs,fMapSprocParams) =

    let projects = dte.GetProjects()
    let appendLine text (sb:System.Text.StringBuilder) =
        sb.AppendLine text
    sb
    |> appendLine ("Projects:")
    |> ignore

    projects
    |> Seq.choose (fun proj -> proj.Name)
    |> Seq.iter (fun projName -> sb.AppendLine (sprintf "    %s" projName) |> ignore)

    let genMapped =
        toGen
        |> Seq.filter(fun t -> t.SqlItems.Length > 0)
        |> Seq.map (fun t ->
            let targetSqlProject =
                projects
                |> Seq.tryFind (fun p -> p.Name.Value = t.TargetSqlProjectName)
                |> function
                    | Some p -> p
                    | None -> failwithf "did not find project %s, names were %A for %A sqlItems" t.TargetSqlProjectName (projects |> Seq.map (fun p -> p.Name) |> List.ofSeq) (t.SqlItems.Length)

            let targetSqlProjectFolder = Path.GetDirectoryName targetSqlProject.FullName.Value
            printfn "Going to generate into project %s via folder %s" targetSqlProject.Name.Value targetSqlProjectFolder
            targetSqlProjectFolder, t, t.SqlItems
                |> Seq.map (fun tg ->
                    {   TableGenerationInfo.Id = {Name=tg.Name; Schema=tg.Schema}
                        Columns= tg.Columns |> List.ofSeq
                    }
                )
                |> List.ofSeq
        )
        |> List.ofSeq
    printfn "%i tables to generate" genMapped.Length

    let codeGenAsm = typeof<CodeGeneration.SqlScriptGeneration.SqlObj>.Assembly
    let info = BReusable.Reflection.Assemblies.getAssemblyFullPath(codeGenAsm)
    let fileInfo = IO.FileInfo(info)
    sb |> appendLine (sprintf "Using CodeGeneration.dll from %O" fileInfo.LastWriteTime) |> ignore
    let detailed =
        genMapped
        |> Seq.collect (fun (targetSqlProjectFolder,sgi,items) ->
            generateTablesAndReferenceTables(manager, sb, Some targetSqlProjectFolder, items,debug)
            match sgi.InsertionConfig with
            | Some ic ->
                SqlScriptGeneration.generateInserts
                    (fun s -> appendLine s sb |> ignore)
                    manager
                    targetSqlProjectFolder
                    items ic
            | None -> ()
            items
        )
        |> List.ofSeq
    (dataModelOnlyItems|> List.map DataModelOnly)@(detailed |> List.map GenMapTableItem.Detailed)
    |> Seq.filter (
        function
        | DataModelOnly ti -> ti
        | Detailed details -> details.Id
        >> (fun x ->
        not <| cgsm.TypeGenerationNolist.Contains x.Name &&
            not <| cgsm.TypeGenerationNolist.Contains (sprintf "%s.%s" x.Schema x.Name)
        )
    )
    |> List.ofSeq
    |> generateCode generatorId cgsm manager (fGetSqlMeta,fGetSqlSprocs,fMapSprocParams) sb

let sb = System.Text.StringBuilder()
let appendLine text (sb:System.Text.StringBuilder) =
    sb.AppendLine text

sb
|> appendLine "Main File Output"
|> appendLine (sprintf "Started at %O" DateTime.Now)
|> ignore
