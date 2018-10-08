namespace CodeGeneration.VS

open BCore.CodeGeneration
open BCore.CodeGeneration.DteWrapCore
open CodeGeneration.GenerateAllTheThings

module DteWrap =
    open System.Collections.Generic
    open System

    type Dte = EnvDTE.DTE
    type Project = EnvDTE.Project
    type ProjectItem = EnvDTE.ProjectItem

    let rec iProjectItemsCollection (pic:EnvDTE.ProjectItems):IProjectItemsCollection =
        {   new IProjectItemsCollection with
                member __.AddFromFile fn =
                    pic.AddFromFile fn |> Option.ofObj |> Option.map iProjectItem |> Option.getOrDefault null
            interface IEnumerable<IProjectItem> with
                member __.GetEnumerator():IEnumerator<IProjectItem>= pic |> Seq.cast<ProjectItem> |> Seq.map iProjectItem |> fun x -> x.GetEnumerator()
                member __.GetEnumerator():System.Collections.IEnumerator = pic |> Seq.cast<ProjectItem> |> Seq.map iProjectItem |> fun x -> upcast x.GetEnumerator()
        }

    and iProjectItem (pi:ProjectItem):IProjectItem =
        {new IProjectItem with
            member __.Name = pi.Name
            member __.get_FileNames x = pi.FileNames x
            member __.ProjectItems = pi.ProjectItems |> iProjectItemsCollection
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

    let unloadedProject = "{67294A52-A4F0-11D2-AA88-00C04F688DDE}"

    let writeLnToOutputPane(dte:Dte) (s:string) =
        try
            let window = dte.Windows.Item(EnvDTE.Constants.vsWindowKindOutput).Object :?> EnvDTE.OutputWindow
            window.ActivePane.Activate ()
            window.ActivePane.OutputString <| s + Environment.NewLine
        with ex ->
            let output = sprintf "Failed to writeLnToOutputPane: %s with exception %A" s ex
            printfn "%s" output
            Diagnostics.Trace.WriteLine output

    let wrapDte (dte:Dte) =
        let wrapper = {
            FindProjectItemPropertyValue= fun s p -> dte.Solution.FindProjectItem(s).Properties.Item(p).Value |> string
            GetSourceControl=
                fun () ->
                    if isNull dte.SourceControl then
                        None
                    else
                        Some {SourceControlWrapper.CheckOutItem = dte.SourceControl.CheckOutItem; GetIsItemUnderSC= dte.SourceControl.IsItemUnderSCC; GetIsItemCheckedOut= dte.SourceControl.IsItemCheckedOut}

            GetProjects = fun () -> Macros.VsMacros.getSP dte |> snd |> List.map ProjectWrapper.FromProject
            Log = writeLnToOutputPane dte
            }
        wrapper

open DteWrap

type DteGenWrapper(dte:EnvDTE.DTE) =
    let projects = Macros.VsMacros.getSP dte |> snd |> Seq.map BCore.CodeGeneration.DteWrapCore.ProjectWrapper.FromProject |> Seq.cast<IProject> |> List.ofSeq
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
