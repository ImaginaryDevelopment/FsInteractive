namespace CodeGeneration.VS

open Core.CodeGeneration
open BCore.CodeGeneration.DteWrapCore

module DteWrap =
    open System.Collections.Generic
    open Core.CodeGeneration.DteWrapCore

    type Dte = EnvDTE.DTE
    type Project = EnvDTE.Project
    type ProjectItem = EnvDTE.ProjectItem

    let iProjectItem (pi:ProjectItem):IProjectItem =
        {new IProjectItem with
            member __.Name = pi.Name
            member __.get_FileNames x = pi.FileNames x
        }

    let iProjectItemsCollection (pic:EnvDTE.ProjectItems):IProjectItemsCollection =
        {   new IProjectItemsCollection with
                member __.AddFromFile fn =
                    pic.AddFromFile fn |> Option.ofObj |> Option.map iProjectItem
            interface IEnumerable<IProjectItem> with
                member __.GetEnumerator():IEnumerator<IProjectItem>= pic |> Seq.cast<ProjectItem> |> Seq.map iProjectItem |> fun x -> x.GetEnumerator()
                member __.GetEnumerator():System.Collections.IEnumerator = pic |> Seq.cast<ProjectItem> |> Seq.map iProjectItem |> fun x -> upcast x.GetEnumerator()
        }

    type ProjectWrapper with
        static member FromProject (p:EnvDTE.Project) =
            {   GetProjectItems= fun () -> iProjectItemsCollection p.ProjectItems
                GetFullName= fun () -> p.FullName
                GetName= fun () -> p.Name
                GetKind= fun () -> p.Kind
            }

    let tryGetKind logOpt (p:ProjectWrapper) =
        try
            p.GetKind() |> Some
        with _ ->
            let nameOpt =
                try
                    p.GetName() |> Some
                with _ -> None
            logOpt
            |> Option.iter (fun f ->f <| sprintf "Failed to read project Kind:%A" nameOpt)
            None
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

let makeManager (dte:EnvDTE.DTE,debug) =
    // if this script is in the solution it is modifying, we need the EnvDTE.ProjectItem representing it, otherwise where does the main (non sub-file) output go?
    let scriptFullPath = Path.Combine(__SOURCE_DIRECTORY__,__SOURCE_FILE__)
    let templateProjectItem:EnvDTE.ProjectItem option = dte.Solution.FindProjectItem scriptFullPath |> Option.ofObj
    printfn "Script is at %s" scriptFullPath
    templateProjectItem
    |> Option.iter(fun templateProjectItem ->
        printfn "ProjectItem= %A" (templateProjectItem.FileNames 0s)
    )
    let dteWrapper = wrapDte dte
    MultipleOutputHelper.Managers.VsManager(Some "HelloTesting.fake.tt", dteWrapper, sb, templateProjectItem,debug)
