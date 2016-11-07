﻿namespace MacroRunner
open System
open System.Collections.Generic
open System.IO
open BReusable

module DteWrap = 
    open EnvDTE
    type Dte = EnvDTE.DTE
    type SourceControlWrapper = {
        GetIsItemUnderSC: string -> bool
        GetIsItemCheckedOut: string -> bool
        CheckOutItem:string -> bool
    }
    // abstract away any specific needs the manager has to interact with DTE
    type DteWrapper = {
        FindProjectItemPropertyValue: string -> string -> string
        GetSourceControl: unit -> SourceControlWrapper option
//        Sync: string list -> unit
//        GetProjectFullNameFromProjectName: string -> string
        GetProjects: unit -> EnvDTE.Project list
        Log: string -> unit
        }

// MultipleOutputHelper.ttinclude
module MultipleOutputHelper = 
    open DteWrap

    [<AllowNullLiteral>]
    type Block () =
            member val Name:string = null with get,set
//            member val Project:EnvDTE.Project = null with get,set
            member val Start = 0 with get,set
            member val Length = 0 with get,set

    type CreateProcessResult =
        |Unchanged
        |Changed
    type IManager =
        abstract member StartNewFile : Filename:string -> unit
        abstract member TemplateFile: string with get
        abstract member DteWrapperOpt : DteWrapper option with get
        abstract member EndBlock: unit -> unit
        // return the input (startNewFile or null for main file) mapped to the full file path created
        abstract member Process: doMultiFile:bool -> IDictionary<string,CreateProcessResult*string>
        abstract member DefaultProjectNamespace: string with get
        abstract member GeneratedFileNames : string seq with get

    module Managers = 
        open System.Text
        open Microsoft.VisualStudio.TextTemplating
        open EnvDTE80

        type Manager (host,template) = 
        
            let mutable currentBlock:Block = null
            let files = List<Block>()
            let footer = Block()
            let header = Block()
            let host : ITextTemplatingEngineHost = host
            let template : StringBuilder = template
            //protected List<String> generatedFileNames = new List<String>();
            let mutable generatedFileNames : string list = []
    
            member __.GeneratedFileNames with get() = generatedFileNames
            member __.StartNewFile name =
                if isNull name then raise <| new ArgumentNullException("name")
                printfn "Starting new file at %s" name
//                let project = defaultArg project null
                currentBlock <- new Block(Name=name,//Project=project, 
                    Start=template.Length)
                
            member x.StartFooter () = x.CurrentBlock <- footer
            member x.StartHeader () = x.CurrentBlock <- header
            
            member __.EndBlock () =
                let log msg = System.Diagnostics.Debugger.Log(0, "EndBlock", if msg |> endsWith "\r\n" then msg else sprintf "%s\r\n" msg)
                if isNull currentBlock then
                    log "ending a null block!"
                    ()
                else
                    let len = template.Length - currentBlock.Start 

                    if len < 0 then failwithf "block length is less than zero"
                    currentBlock.Length <- len
                    log <| sprintf "ending a block of length %i!\r\nstarted at %i\r\n" currentBlock.Length currentBlock.Start
                    if currentBlock <> header && currentBlock <> footer then
                        log <| sprintf "Adding a block to the files list: %s" currentBlock.Name
                        files.Add currentBlock
                    currentBlock <- null

            abstract Process : split:bool -> IDictionary<string,CreateProcessResult*string>
            default x.Process split = 
                if isNull template then
                    failwithf "template was null"

                let len = template.Length
                if len = 0 then failwithf "No text has been added"
                let log (s:string) = 
                    let text = if s.EndsWith "\r\n" then s else sprintf "%s\r\n" s 
                    printfn "%s" text
                    if System.Diagnostics.Debugger.IsAttached then
                        System.Diagnostics.Debugger.Log(0, "Logger", text)
                log "Hello debugger\r\n"
                if split then
                    x.EndBlock()
                    let mutable pairs : (string*(CreateProcessResult*string)) list = List.empty
                    let tryGetByStartEnd s l = 
                        try
                            template.ToString(s,l)
                        with ex ->
                            raise <| InvalidOperationException(sprintf "could not access substring %i %i in length %i" s l template.Length, ex)
                    if isNull header then
                        failwithf "header was null"
                    if isNull footer then
                        failwithf "footer was null"
                    let headerText = tryGetByStartEnd header.Start header.Length
                    let footerText = tryGetByStartEnd footer.Start footer.Length
                    if isNull host then
                        failwithf "host was null"
                    if isNull host.TemplateFile then
                        failwithf "host.TemplateFile was null"
                    let outputPath = Path.GetDirectoryName(host.TemplateFile)
                    if isNull files then failwithf "files was null"
                    files.Reverse()
                    let mutable i = 0
                    for block in files do
                        log <| sprintf "Processing block %i:%s" i block.Name
                        log <| sprintf "Length is %i" template.Length
                        
                        let fileName = Path.Combine(outputPath, block.Name)
                        let content = headerText + (tryGetByStartEnd block.Start block.Length) + footerText
                        generatedFileNames <- fileName::generatedFileNames
                        let didCreate = x.CreateFile fileName content
                        if didCreate then
                            pairs <-  (block.Name, (CreateProcessResult.Changed, fileName))::pairs
                        else pairs <- (block.Name, (CreateProcessResult.Unchanged, fileName))::pairs
                        template.Remove(block.Start, block.Length) |> ignore
                        log <| sprintf "Processed block %i" i
                        log <| sprintf "Length is %i" template.Length
                        i <- i + 1
                    pairs |> dict
                else 
                    List.empty
                    |> dict

            abstract CreateFile : fileName:string -> content:string -> bool 
            default x.CreateFile fileName content = 
                if x.IsFileContentDifferent fileName content then
                    printfn "Writing a file to %s" fileName
                    File.WriteAllText(fileName, content)
                    true
                else
                    false
                    
            abstract GetCustomToolNamespace: fileName: string -> string
            default __.GetCustomToolNamespace _fileName = null
            
            abstract DefaultProjectNamespace:  string with get
            default __.DefaultProjectNamespace with get() = null
            
            abstract IsFileContentDifferent : fileName:string -> newContent:string -> bool
            default __.IsFileContentDifferent fileName newContent =
                let isFileContentDifferent =
                    (File.Exists fileName && File.ReadAllText(fileName) = newContent)
                    |> not
                isFileContentDifferent
                
            member x.CurrentBlock
                with get() = currentBlock
                and set v = 
                    if not <| isNull x.CurrentBlock then
                        x.EndBlock()
                    if not <| isNull v then
                        v.Start <- template.Length
                    currentBlock <- v

            static member Create(host:ITextTemplatingEngineHost,template:StringBuilder) =
                let beforeConLength = template.Length
                let manager = 
                    match host with 
                    | :? IServiceProvider -> VsManager(host,template) :> Manager
                    | _ -> 
                        failwithf "unable to get ahold of vsmanager"
                        Manager(host,template)
                if beforeConLength > template.Length then failwithf "Someone touched me!"
                manager

            interface IManager with
                override x.StartNewFile p = x.StartNewFile p
//                    match s with
//                    | Some project -> x.StartNewFile(p,project)
//                    | None -> x.StartNewFile(p,null)
                override x.EndBlock() = x.EndBlock ()
                override x.Process doMultiFile = x.Process doMultiFile
                override x.DefaultProjectNamespace = x.DefaultProjectNamespace
                override __.DteWrapperOpt = None
                override __.TemplateFile = host.TemplateFile
                override __.GeneratedFileNames = upcast generatedFileNames

        and VsManager (host,dteWrapper:DteWrapper,template,templateProjectItem:EnvDTE.ProjectItem) =
                inherit Manager(host,template)

//                let checkOutAction: Action<string> = checkOutAction // Action<String> 
//                let projectSyncAction: Action<string seq> = projectSyncAction //Action<IEnumerable<String>> 
    
                interface IManager with
                    override __.DteWrapperOpt = Some dteWrapper

                override __.DefaultProjectNamespace 
                    with get() = 
                        match templateProjectItem with
                        | null -> null
                        | _ -> 
                            if isNull templateProjectItem then failwithf "templateProjectItem is null"
                            if isNull templateProjectItem.ContainingProject then failwithf "templateProjectItem.ContainingProject is null"
                            if isNull templateProjectItem.Properties then failwithf "templateProjectItem.ContainingProject.Properties is null"
    //
                            templateProjectItem.ContainingProject.Properties.Item("DefaultNamespace").Value.ToString()
                override __.GetCustomToolNamespace fileName = dteWrapper.FindProjectItemPropertyValue fileName "CustomToolNamespace" //dte.Solution.FindProjectItem(fileName).Properties.Item("CustomToolNamespace").Value.ToString()

                override __.Process split =
                    if template.Length = 0 then failwithf "No text has been added"
                    let results = base.Process split
                    VsManager.ProjectSyncScriptWrapped dteWrapper templateProjectItem base.GeneratedFileNames
                    results

                override x.CreateFile fileName content =
                    if x.IsFileContentDifferent fileName content then
                        printfn "Creating File: %s" fileName
                        x.CheckoutFileIfRequired(fileName)
                        File.WriteAllText(fileName, content)
                        true
                    else false

                        
                //static member private x.CreateVsManager(
                internal new(host:ITextTemplatingEngineHost , template:StringBuilder) =
                    let hostServiceProvider = host :?> IServiceProvider
                    if isNull hostServiceProvider then
                        raise <| ArgumentNullException("Could not obtain IServiceProvider")
                    let  dte = hostServiceProvider.GetService(typeof<Dte>) :?> Dte
                    if isNull dte then
                        raise <| ArgumentNullException("Could not obtain DTE from host")
                    
                    let templateProjectItem = dte.Solution.FindProjectItem(host.TemplateFile)
                    if isNull templateProjectItem then failwithf "VsManager.new: templateProjectItem is null"
//                    let checkOutAction fileName = dte.SourceControl.CheckOutItem fileName |> ignore
//                    let projectSyncAction = fun keepFileNames -> VsManager.ProjectSync(templateProjectItem, keepFileNames)
                    let getProjects() = 
                        dte|> Macros.VsMacros.getSP |> snd 
                    let wrapper = {
                        FindProjectItemPropertyValue= (fun s p -> dte.Solution.FindProjectItem(s).Properties.Item(p).Value |> string); 
                        GetSourceControl= 
                            fun () ->
                                if isNull dte.SourceControl then 
                                    None 
                                else
                                    Some {SourceControlWrapper.CheckOutItem = dte.SourceControl.CheckOutItem; GetIsItemUnderSC= dte.SourceControl.IsItemUnderSCC; GetIsItemCheckedOut= dte.SourceControl.IsItemCheckedOut}
//                        Sync= (fun fileNames -> 
//                            VsManager.ProjectSync(templateProjectItem,fileNames)
//                        )
                        GetProjects = getProjects
                        Log = VsManager.WriteLnToOutputPane dte
//                        GetProjectFullNameFromProjectName = fun projectName -> getProjects() |> Seq.find(fun p -> p.Name = projectName) |> fun p -> p.FullName
                        }
                    VsManager(host,wrapper,template,templateProjectItem)

                static member WrapDte(dte:Dte) = 
                    let wrapper = {
                        FindProjectItemPropertyValue= (fun s p -> dte.Solution.FindProjectItem(s).Properties.Item(p).Value |> string); 
                        GetSourceControl= 
                            fun () ->
                                if isNull dte.SourceControl then 
                                    None 
                                else
                                    Some {SourceControlWrapper.CheckOutItem = dte.SourceControl.CheckOutItem; GetIsItemUnderSC= dte.SourceControl.IsItemUnderSCC; GetIsItemCheckedOut= dte.SourceControl.IsItemCheckedOut}
                        
                        GetProjects = fun () -> Macros.VsMacros.getSP dte |> snd
                        Log = VsManager.WriteLnToOutputPane dte
//                        GetProjectFullNameFromProjectName = fun projectName -> getProjects() |> Seq.find(fun p -> p.Name = projectName) |> fun p -> p.FullName
                        }
                    wrapper 
                static member WriteLnToOutputPane(dte:Dte) (s:string) =
                    let window = dte.Windows.Item(EnvDTE.Constants.vsWindowKindOutput).Object :?> EnvDTE.OutputWindow
                    window.ActivePane.Activate ()
                    window.ActivePane.OutputString (s + Environment.NewLine)

                static member AddFileToDbSqlProj (dte:DteWrapper) (targetProject:EnvDTE.Project) fileName = 
                    dte.Log (sprintf "Attempting to add a file to a dbSqlProj: %s" fileName)
                    let projectItems = ref targetProject.ProjectItems
                    let trim1 d (s:string) = s.Trim(d |> Array.ofSeq)
                    let toDescend = 
//                                fileName.Substring(System.IO.Path.GetDirectoryName(targetProject.FullName).Length).Trim('\\').Trim('/').Split(new []{"\\","/"}, StringSplitOptions.None)
                        targetProject.FullName
                        |> Path.GetDirectoryName
                        |> String.length
                        |> flip String.subString fileName
                        |> trim1 ['\\']
                        |> trim1 ['/']
                        |> String.split ["\\";"/"]
                    toDescend
//                            foreach(let td in toDescend.Take(toDescend.Length - 1))
                    |> Seq.take (toDescend.Length - 1)
                    |> Seq.iter (fun td ->
                        dte.Log ("Descending into \"" + td + "\"")
                        let childProjectItemOpt = 
                            !projectItems
                            |> Seq.cast<EnvDTE.ProjectItem>
                            |> Seq.tryFind (fun pi -> //.FirstOrDefault(pi => 
                                let isMatch = Path.GetFileName(pi.get_FileNames(0s)) = td
                                isMatch
                            )

                        match childProjectItemOpt with
                        | None ->
                            dte.Log ("Failed to find \"" + td + "\"")
                            !projectItems
                            |> Seq.cast<EnvDTE.ProjectItem>
                            |> Seq.map (fun pi -> pi.get_FileNames(0s))
                            |> Seq.iter(fun pi ->
                                dte.Log ("\tWe did however find \"" + pi + "\"")
                            )
                        | Some childProjectItem ->
                            dte.Log (sprintf "OutputPane:Appears we found \"%s\" and it has projectItems:%A" td (not <| isNull childProjectItem.ProjectItems))
                            if not <| isNull childProjectItem.ProjectItems then
                                projectItems := childProjectItem.ProjectItems
                    )

                    dte.Log ("OutputPane: Generating \"" + fileName + "\" into " + targetProject.FullName)

                    let projectItem = 
                        !projectItems
                        |> fun pi -> pi.AddFromFile(fileName)
                    dte.Log ("AddFromFile put it @" + projectItem.get_FileNames(0s))

                // from line to 221..298
                static member AddFileToProject (dte:DteWrapper) (projects: EnvDTE.Project seq) (fileName:string) = 
                    // only one this appears to be skipping in a solutionfolder
                    let unloadedProject = "{67294A52-A4F0-11D2-AA88-00C04F688DDE}";
                    let canReadProjects = projects |> Seq.filter (fun p -> p.Kind <> unloadedProject && p.Kind <> EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder) |> List.ofSeq
                    
                    // line 224
                    canReadProjects
                    |> Seq.tryFind (fun p -> //.FirstOrDefault(p => 
                            try
                                p.Kind <> EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder && fileName.StartsWith(System.IO.Path.GetDirectoryName(p.FullName))
                            with _ -> 
                                dte.Log ("failing to read project with kind= " + p.Kind)
                                dte.Log ("expected kind= " + EnvDTE80.ProjectKinds.vsProjectKindSolutionFolder)
                                dte.Log ("hi! my name is "+ p.Name)
                                reraise()
                            )
                    |> function 
                            | None ->
                                sprintf "%s, could not find in (%i) projects. %s" fileName (Seq.length projects) (canReadProjects |> Seq.map (fun p -> p.FullName) |> delimit ",") 
                                |> invalidOp
                                //InvalidOp (fileName+", could not find in ("+ projects.Count() +") projects." + canReadProjects.Select(p => p.FullName).Aggregate((s1,s2) => s1+","+s2))
                            | Some targetProject ->
                                // line 263
                                // normal adding, traversing, expanding view did not work at all.
                                if targetProject.FullName.EndsWith(".dbproj") || targetProject.FullName.EndsWith(".sqlproj") then
                                    VsManager.AddFileToDbSqlProj dte targetProject fileName
                                // line 299
                                else 
                                    dte.Log ("Generating \"" + fileName + "\" into " + targetProject.FullName)
                                    let projectItem = targetProject.ProjectItems.AddFromFile(fileName)
                                    try
                                        dte.Log ("AddFromFile put it @" + projectItem.get_FileNames(0s))
                                    with _ -> 
                                        dte.Log ("get_FileNames(0) failed for " + fileName + " into " + targetProject.Name);
                // templateProjectItem is just the item to use as a parent if any (may only function in C# projects?) can be null
                static member ProjectSyncScriptWrapped (dte:DteWrapper) (templateProjectItemOpt: EnvDTE.ProjectItem) (keepFileNames: string seq) =
                    printfn "Syncing project(s)"
                    let templateProjectItemOpt = templateProjectItemOpt |> Option.ofObj
                    let keepFileNameSet = HashSet<string>(keepFileNames)
                    let projectFiles = Dictionary<string, EnvDTE.ProjectItem>()
                    let projectOpt = templateProjectItemOpt |> Option.map (fun x -> x.Collection.ContainingProject)
                    match templateProjectItemOpt, projectOpt with
                    | Some pi, Some p-> 
                        dte.Log ("Starting ProjectSync for t4 in " + p.Name + "\\" + pi.Name)
                    | _ -> ()
                    let templateProjectDirectoryOpt = projectOpt |> Option.map (fun p -> Path.GetDirectoryName p.FullName)
                    let originalFilePrefixOpt = templateProjectItemOpt |> Option.map (fun pi -> Path.GetFileNameWithoutExtension(pi.get_FileNames(0s)) + ".")

                    templateProjectItemOpt
                    |> Option.bind(fun pi -> pi.ProjectItems |> Option.ofObj)
                    |> Option.iter(Seq.cast<EnvDTE.ProjectItem> >> Seq.iter (fun pi -> projectFiles.Add(pi.get_FileNames(0s), pi)))
                    projectFiles
                    |> Seq.filter (fun pair -> keepFileNames |> Seq.contains pair.Key |> not && not <| (Path.GetFileNameWithoutExtension(pair.Key) + ".").StartsWith(originalFilePrefixOpt.Value))
                    |> Seq.iter(fun pair ->
                        dte.Log (sprintf "removing unused item from keep list: %s" pair.Key)
                        pair.Value.Delete()
                    )
                    dte.Log("finished removing unused items")
                    let projects = dte.GetProjects()
                    let isInCurrentProject (fileName:string) = match templateProjectDirectoryOpt with | Some d -> fileName.StartsWith d | None -> false
                    // Add missing files to the project(s)
                    keepFileNameSet
                    |> Seq.iter(fun fileName ->
                        dte.Log (sprintf "adding a file %s" fileName)
                        if isInCurrentProject fileName then
                            if not <| projectFiles.ContainsKey fileName then
                                let newProjectItem = templateProjectItemOpt.Value.ProjectItems.AddFromFile fileName
                                if isNull newProjectItem then
                                    dte.Log(sprintf "add returned null newProjectItem for %s" fileName)
                        else
                            VsManager.AddFileToProject dte projects fileName
                        dte.Log "finished projectSync"
                    )

                // keep this until such time as it isn't being used by the project .tt and the above method is verified to be working
                static member ProjectSync(templateProjectItem:EnvDTE.ProjectItem, keepFileNames:string seq) =
                    printfn "Syncing project(s)"
                    let keepFileNameSet = HashSet<string>(keepFileNames)
                    let projectFiles = Dictionary<string, EnvDTE.ProjectItem>()
                    let dte = templateProjectItem.Collection.DTE
                    let dteWrapper = VsManager.WrapDte dte
                    let project = templateProjectItem.Collection.ContainingProject
                    VsManager.WriteLnToOutputPane dte ("Starting ProjectSync for t4 in " + project.Name + "\\" + templateProjectItem.Name)
                    let templateProjectDirectory = Path.GetDirectoryName project.FullName
                    let _sol,projects = Macros.VsMacros.getSP dte //EnvDteHelper.recurseSolutionProjects dte 
                    let inline isInCurrentProject (fileName:string) =  fileName.StartsWith(templateProjectDirectory)
                    let originalFilePrefix = Path.GetFileNameWithoutExtension(templateProjectItem.get_FileNames(0s)) + "."
                    if not <| isNull templateProjectItem.ProjectItems then
                        for projectItem in templateProjectItem.ProjectItems do
                            projectFiles.Add(projectItem.get_FileNames(0s), projectItem)
                    else
                        VsManager.WriteLnToOutputPane dte ("templateProjectItem.ProjectItems was null, not sure what all will fail from here on out")

                    // Remove unused items from the project
                    for pair in projectFiles do
                        if keepFileNames |> Seq.contains pair.Key |> not && not <| (Path.GetFileNameWithoutExtension(pair.Key) + ".").StartsWith(originalFilePrefix) then
                            VsManager.WriteLnToOutputPane dte (sprintf "removing unused item from keep list: %s" pair.Key)
                            pair.Value.Delete()
                    VsManager.WriteLnToOutputPane dte ("finished removing unused items")

                    // Add missing files to the project
                    for fileName in keepFileNameSet do
                        VsManager.WriteLnToOutputPane dte (sprintf "adding a file %s" fileName)
                        if isInCurrentProject fileName then
                            if not <| projectFiles.ContainsKey(fileName) then
                                templateProjectItem.ProjectItems.AddFromFile(fileName) |> ignore
                                VsManager.WriteLnToOutputPane dte ("added " + fileName)
                        else // add to another project
                            VsManager.AddFileToProject dteWrapper projects fileName
                    VsManager.WriteLnToOutputPane dte "OutputPane: finished projectSync"

                member __.CheckoutFileIfRequired fileName =
                    printfn "Checking out file if needed: %s" fileName
                    dteWrapper.GetSourceControl()
                    |> Option.iter (fun sc -> 
                        if sc.GetIsItemUnderSC fileName && not <| sc.GetIsItemCheckedOut fileName then
                            sc.CheckOutItem fileName |> ignore<bool>
//                            checkOutAction.EndInvoke(checkOutAction.BeginInvoke(fileName, null, null))
                    )




