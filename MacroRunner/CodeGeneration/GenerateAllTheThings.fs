module CodeGeneration.GenerateAllTheThings
// purpose: run SqlMeta.generateSql >> DataModelToF.generate
open System
open System.Collections.Generic
open System.Diagnostics
open Microsoft.VisualStudio.TextTemplating

open MacroRunner
open MacroRunner.DteWrap
open MacroRunner.MultipleOutputHelper.Managers
open CodeGeneration
open CodeGeneration.SqlMeta
open CodeGeneration.DataModelToF

let failing s= 
    if Debugger.IsAttached then
        Debugger.Log(1,"fail", s)
        Debugger.Break()
    failwith s

type ColumnInput = {
        Name:string
        Type:Type
        Length: int option
        Precision:int option
        Scale: int option
        UseMax: bool
        AllowNull: Nullability
        Attributes:string list
        FKey:FKeyInfo option
        Comments: string list
        GenerateReferenceTable: bool
        ReferenceValuesWithComment: IDictionary<string,string>
        IsUnique: bool
    } with 
        static member create name columnType = 
            {Name=name; Type=columnType; Length= None; Precision=None;Scale=None;UseMax=false; AllowNull=Nullability.NotNull; Attributes=list.Empty; FKey= None; Comments = List.empty; GenerateReferenceTable=false; ReferenceValuesWithComment = null; IsUnique=false}
        static member createFKeyedColumn<'T> name fkeyInfo = 
            ColumnInput.create name typeof<'T>
            |> fun x -> {x with FKey = Some fkeyInfo }
        static member createFKeyedNColumn<'T> name fkeyInfo =
            ColumnInput.createFKeyedColumn<'T> name fkeyInfo
            |> fun x -> { x with AllowNull = Nullability.AllowNull}
        static member createPatientIdColumn prefix allowNull comments = 
            ColumnInput.createFKeyedColumn<int> (prefix + "PatientID") {Schema="dbo"; Table="Patients"; Column ="PatientID"}
            |> fun x -> {x with Comments = comments; AllowNull = allowNull}
        static member createUserIdColumn prefix allowNull comment = 
            ColumnInput.createFKeyedColumn<int> (prefix + "UserID") {Schema="dbo"; Table="Users";Column="UserID" }
            |> fun x -> {x with Comments = comment; AllowNull= allowNull}
        static member makeNullable50 name = 
            {Name=name; Type=typeof<string>; Length=Some 50; Precision=None; Scale=None; UseMax=false; AllowNull = Nullability.AllowNull; Attributes=List.empty; FKey = None;Comments = List.empty; GenerateReferenceTable=false; ReferenceValuesWithComment=null; IsUnique=false }

type TableInput() = 
     member val Name:string = Unchecked.defaultof<_> with get,set
     member val Schema:string = Unchecked.defaultof<_> with get,set
     member val Columns:ColumnInput seq = Unchecked.defaultof<_> with get,set

type SqlGenerationConfig = { TargetSqlProjectName: string; SqlItems: TableInput list; InsertionConfig: InsertsGenerationConfig option}

// although InsertsGenerationConfig doesn't apply to TableGenerationInfo, let them be grouped for convenience
//type SqlGenerationism =
//    | Focused of InsertsGenerationConfig * (GenerationTarget list)
//    | Multiple of (InsertsGenerationConfig * GenerationTarget list) list

/// generatorId something to identify the generator with, in the .tt days it was the DefaultProjectNamespace the .tt was running from.
let runGeneration generatorId (sb:System.Text.StringBuilder) (dte:EnvDTE.DTE) manager (cgsm: CodeGeneration.DataModelToF.CodeGenSettingMap) toGen dataModelOnlyItems = 
    //guard columns having a Type = obj
    match toGen |> Seq.map (fun g -> g.SqlItems) |> Seq.collect id |> Seq.tryFind(fun g -> g.Columns |> Seq.exists(fun c -> c.Type = typeof<obj>)) with
    | Some g -> 
        printfn "failing because of %s.%s" g.Schema g.Name
        failwithf "object found"
    | _ -> ()

    let pluralizer = Macros.VsMacros.createPluralizer()
    let projects = snd <| Macros.VsMacros.getSP dte // RecurseSolutionProjects(Dte)
    let appendLine text (sb:System.Text.StringBuilder) = 
        sb.AppendLine text
    sb 
    |> appendLine ("Projects:")
    |> ignore

    projects
    |> Seq.iter (fun proj -> sb.AppendLine (sprintf "    %s" proj.Name) |> ignore)

    
    let genMapped = 
        let toColumnType t l p s u = SqlMeta.toColumnType t (Option.toNullable l) (Option.toNullable p) (Option.toNullable s) u
        toGen
        |> Seq.map (fun t ->
            let targetSqlProject = 
                projects 
                |> Seq.tryFind (fun p -> p.Name = t.TargetSqlProjectName) 
                |> function 
                    | Some p -> p 
                    | None -> failwithf "did not find project, names were %A" (projects |> Seq.map (fun p -> p.Name) |> List.ofSeq)

            let targetSqlProjectFolder = Path.GetDirectoryName targetSqlProject.FullName
            printfn "Going to generate into project %s via folder %s" targetSqlProject.Name targetSqlProjectFolder
            targetSqlProjectFolder, t, t.SqlItems
                |> Seq.map (fun tg ->
                    {   TableInfo.Name=tg.Name
                        Schema=tg.Schema
                        Columns=
                            tg.Columns 
                            |> Seq.map (fun ci ->
                                try 
                                    {   ColumnInfo.Name= ci.Name
                                        Type= toColumnType ci.Type ci.Length ci.Precision ci.Scale ci.UseMax 
                                        AllowNull= ci.AllowNull
                                        Attributes = ci.Attributes
                                        IsUnique= ci.IsUnique
                                        FKey= ci.FKey //if isNull ci.FKey then None else {FKeyInfo.Schema = ci.FKey.Schema; Table= ci.FKey.Table; Column = ci.FKey.Column} |> Some
                                        Comments = ci.Comments
                                        GenerateReferenceTable = ci.GenerateReferenceTable
                                        ReferenceValuesWithComment = ci.ReferenceValuesWithComment
                                    }
                                with _ -> 
                                    printfn "Failed to map %A for table %A" ci tg
                                    reraise()
                            )
                            |> List.ofSeq

                    }
                )
                |> List.ofSeq
        )
        |> List.ofSeq
    printfn "%i tables to generate" genMapped.Length

    let codeGenAsm= typeof<CodeGeneration.SqlScriptGeneration.SqlObj>.Assembly
    let info = BReusable.Assemblies.getAssemblyFullPath(codeGenAsm)
    let fileInfo = new System.IO.FileInfo(info)
    sb |> appendLine (sprintf "Using CodeGeneration.dll from %O" fileInfo.LastWriteTime) |> ignore
    genMapped
    |> Seq.map (fun (targetSqlProjectFolder,sgi,items) ->
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
        |> Seq.map (fun gm -> {Schema=gm.Schema; Name=gm.Name; GenerateFull= false})
        |> List.ofSeq
        |> fun items -> dataModelOnlyItems@items
    )
    |> Seq.collect id
    |> List.ofSeq
    |> fun mappedTables ->
    DataModelToF.generate generatorId 
        pluralizer.Pluralize 
        pluralizer.Singularize 
        cgsm
        (manager, sb, mappedTables) 

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
        printfn "ProjectItem= %A" (templateProjectItem.FileNames(0s))
    let dteWrapper = VsManager.WrapDte dte
    MultipleOutputHelper.Managers.VsManager(Some "HelloTesting.fake.tt", dteWrapper, sb, templateProjectItem)
