module CodeGeneration.GenerateAllTheThings
// purpose: run SqlMeta.generateSql >> DataModelToF.generate
open System
open System.Collections.Generic
open System.Diagnostics
open Microsoft.VisualStudio.TextTemplating

open MacroRunner
open MacroRunner.DteWrap
open MacroRunner.MultipleOutputHelper.Managers
open Macros.SqlMacros
open CodeGeneration
open CodeGeneration.SqlMeta
open CodeGeneration.SqlMeta.ColumnTyping

open CodeGeneration.DataModelToF
open BReusable.StringHelpers


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

type SqlGenerationConfig = { TargetSqlProjectName: string; SqlItems: TableInput list; InsertionConfig: InsertsGenerationConfig option}

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
type IProject =
        abstract member Name:string option
        abstract member FullName:string option
type ProjectWrapper internal (p:EnvDTE.Project) =
    // swallowing? =(
    let tryGet f =
        try
            f p |> Some
        with ex -> 
            System.Diagnostics.Debug.WriteLine(sprintf "%A" ex)
            None
    member __.Name = tryGet (fun p -> p.Name)
    member __.FullName = tryGet (fun p -> p.FullName)
    interface IProject with
        member x.Name = x.Name
        member x.FullName = x.FullName

type IGenWrapper =
    abstract member GetProjects: unit -> IProject seq
    abstract member GetTargetSqlProjectFolder: string -> IProject

type DteGenWrapper(dte:EnvDTE.DTE) =
    let projects = Macros.VsMacros.getSP dte |> snd |> Seq.map ProjectWrapper |> Seq.cast<IProject> |> List.ofSeq
    member __.GetProjectNames() = projects |> Seq.map (fun proj -> proj.Name)
    member __.GetProjects() = projects
    member __.GetTargetSqlProjectFolder targetSqlProjectName =
        projects
        |> Seq.tryFind(fun p -> p.Name.Value = targetSqlProjectName)
        |> function
            | Some p -> p
            | None -> failwithf "did not find project %s, names were %A" targetSqlProjectName (projects |> Seq.map (fun p -> p.Name) |> List.ofSeq)
    interface IGenWrapper with
        member x.GetProjects() = upcast x.GetProjects()
        member x.GetTargetSqlProjectFolder targetSqlProjectName = x.GetTargetSqlProjectFolder targetSqlProjectName

/// generatorId something to identify the generator with, in the .tt days it was the DefaultProjectNamespace the .tt was running from.
let runGeneration generatorId (sb: System.Text.StringBuilder) (dte: IGenWrapper) manager (cgsm: CodeGeneration.DataModelToF.CodeGenSettingMap) toGen (dataModelOnlyItems: TableIdentifier list) =

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
    let fileInfo = new IO.FileInfo(info)
    sb |> appendLine (sprintf "Using CodeGeneration.dll from %O" fileInfo.LastWriteTime) |> ignore
    let detailed = 
        genMapped
        |> Seq.collect (fun (targetSqlProjectFolder,sgi,items) ->
            SqlMeta.generateTablesAndReferenceTables(manager, sb, Some targetSqlProjectFolder, items)
            match sgi.InsertionConfig with
            | Some ic ->
                SqlMeta.generateInserts
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
        not <| cgsm.TypeGenerationBlacklist.Contains x.Name && 
            not <| cgsm.TypeGenerationBlacklist.Contains (sprintf "%s.%s" x.Schema x.Name)
        )
    )
    |> List.ofSeq
    |> fun mappedTables ->
        DataModelToF.generate generatorId
            (Some (fun ti (_exn:exn) ->
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
                        |> Some
                    | DataModelOnly _dmInput ->
                        None
            ))
            cgsm
            (manager, sb, mappedTables |> List.map GenMapTableItem.GetTI)

let sb = System.Text.StringBuilder()
let appendLine text (sb:System.Text.StringBuilder) =
    sb.AppendLine text

sb
|> appendLine "Main File Output"
|> appendLine (sprintf "Started at %O" DateTime.Now)
|> ignore

let makeManager (dte:EnvDTE.DTE) =
    // if this script is in the solution it is modifying, we need the EnvDTE.ProjectItem representing it, otherwise where does the main (non sub-file) output go?
    let scriptFullPath = Path.Combine(__SOURCE_DIRECTORY__,__SOURCE_FILE__)
    let templateProjectItem:EnvDTE.ProjectItem = dte.Solution.FindProjectItem(scriptFullPath)
    printfn "Script is at %s" scriptFullPath
    if not <| isNull templateProjectItem then
        printfn "ProjectItem= %A" (templateProjectItem.FileNames 0s)
    let dteWrapper = wrapDte dte
    MultipleOutputHelper.Managers.VsManager(Some "HelloTesting.fake.tt", dteWrapper, sb, templateProjectItem)
