namespace MacroRunner
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
        Sync: string list -> unit
//        GetProjectFullNameFromProjectName: string -> string
        GetProjects: unit -> EnvDTE.Project list
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

    type IManager =
        abstract member StartNewFile : Filename:string -> unit
        abstract member TemplateFile: string with get
        abstract member DteWrapperOpt : DteWrapper option with get
        abstract member EndBlock: unit -> unit
        abstract member Process: doMultiFile:bool -> unit
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
                    let len =template.Length - currentBlock.Start 

                    if len < 0 then failwithf "block length is less than zero"
                    currentBlock.Length <- len
                    log <| sprintf "ending a block of length %i!\r\nstarted at %i\r\n" currentBlock.Length currentBlock.Start
                    if currentBlock <> header && currentBlock <> footer then
                        log <| sprintf "Adding a block to the files list: %s" currentBlock.Name
                        files.Add currentBlock
                    currentBlock <- null
                    
            abstract Process : split:bool -> unit
            default x.Process split = 
                let len = template.Length
                if len = 0 then failwithf "No text has been added"
                let log (s:string) = 
                    if System.Diagnostics.Debugger.IsAttached then
                        System.Diagnostics.Debugger.Log(0, "Logger", if s.EndsWith "\r\n" then s else sprintf "%s\r\n" s)
                log "Hello debugger\r\n"
                if split then
                    x.EndBlock()
                    let tryGetByStartEnd s l = 
                        try
                            template.ToString(s,l)
                        with ex ->
                            raise <| InvalidOperationException(sprintf "could not access substring %i %i in length %i" s l template.Length, ex)
                    let headerText = tryGetByStartEnd header.Start header.Length
                    let footerText = tryGetByStartEnd footer.Start footer.Length
                    let outputPath = Path.GetDirectoryName(host.TemplateFile)
                    files.Reverse()
                    let mutable i = 0
                    for block in files do
                        
                        log <| sprintf "Processing block %i:%s" i block.Name
                        log <| sprintf "Length is %i" template.Length
                        
                        let fileName = Path.Combine(outputPath, block.Name)
                        let content = headerText + (tryGetByStartEnd block.Start block.Length) + footerText
                        generatedFileNames <- fileName::generatedFileNames
                        x.CreateFile fileName content
                        template.Remove(block.Start, block.Length) |> ignore
                        log <| sprintf "Processed block %i" i
                        log <| sprintf "Length is %i" template.Length
                        i <- i + 1
        
            abstract CreateFile : fileName:string -> content:string -> unit
            default x.CreateFile fileName content = 
                if x.IsFileContentDifferent fileName content then
                    printfn "Writing a file to %s" fileName
                    File.WriteAllText(fileName, content)
                    
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

        and VsManager private (host,dteWrapper:DteWrapper,template,templateProjectItem:EnvDTE.ProjectItem) =
                inherit Manager(host,template)

//                let checkOutAction: Action<string> = checkOutAction // Action<String> 
//                let projectSyncAction: Action<string seq> = projectSyncAction //Action<IEnumerable<String>> 
    
                interface IManager with
                    override __.DteWrapperOpt = Some dteWrapper
                    
                override __.DefaultProjectNamespace 
                    with get() = 
                        if isNull templateProjectItem then failwithf "templateProjectItem is null"
                        if isNull templateProjectItem.ContainingProject then failwithf "templateProjectItem.ContainingProject is null"
                        if isNull templateProjectItem.Properties then failwithf "templateProjectItem.ContainingProject.Properties is null"
//
                        templateProjectItem.ContainingProject.Properties.Item("DefaultNamespace").Value.ToString()
                override __.GetCustomToolNamespace fileName = dteWrapper.FindProjectItemPropertyValue fileName "CustomToolNamespace" //dte.Solution.FindProjectItem(fileName).Properties.Item("CustomToolNamespace").Value.ToString()
                
                override __.Process split =
                    if isNull templateProjectItem.ProjectItems then
                        ()
                    else 
                        if template.Length = 0 then failwithf "No text has been added"
                        base.Process split
                    dteWrapper.Sync base.GeneratedFileNames
//                    projectSyncAction.EndInvoke(projectSyncAction.BeginInvoke(base.GeneratedFileNames, null, null))
        
                override x.CreateFile fileName content =
                    if x.IsFileContentDifferent fileName content then
                        x.CheckoutFileIfRequired(fileName)
                        File.WriteAllText(fileName, content)
                        
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
                        Sync= (fun fileNames -> 
                            VsManager.ProjectSync(templateProjectItem,fileNames)
                        )
                        GetProjects = getProjects
//                        GetProjectFullNameFromProjectName = fun projectName -> getProjects() |> Seq.find(fun p -> p.Name = projectName) |> fun p -> p.FullName
                        }
                    VsManager(host,wrapper,template,templateProjectItem)

                static member WriteLnToOutputPane(dte:Dte) (s:string) =
                    let window = dte.Windows.Item(EnvDTE.Constants.vsWindowKindOutput).Object :?> EnvDTE.OutputWindow
                    window.ActivePane.Activate ()
                    window.ActivePane.OutputString (s + Environment.NewLine)
        
                static member ProjectSync(templateProjectItem:EnvDTE.ProjectItem, keepFileNames:string seq)  =
                    let keepFileNameSet = HashSet<string>(keepFileNames)
                    let projectFiles = Dictionary<string, EnvDTE.ProjectItem>()
                    let dte = templateProjectItem.Collection.DTE
                    let project = templateProjectItem.Collection.ContainingProject
                    VsManager.WriteLnToOutputPane dte ("Starting ProjectSync for t4 in " + project.Name + "\\" + templateProjectItem.Name)
                    let templateProjectDirectory = Path.GetDirectoryName project.FullName
                    let _sol,projects = Macros.VsMacros.getSP dte //EnvDteHelper.recurseSolutionProjects dte 
                    let inline isInCurrentProject (fileName:string) =  fileName.StartsWith(templateProjectDirectory)
                    let originalFilePrefix = Path.GetFileNameWithoutExtension(templateProjectItem.get_FileNames(0s)) + "."
                    for projectItem in templateProjectItem.ProjectItems do
                        projectFiles.Add(projectItem.get_FileNames(0s), projectItem)
        
                    // Remove unused items from the project
                    for pair in projectFiles do
                        if keepFileNames |> Seq.contains pair.Key |> not && not <| (Path.GetFileNameWithoutExtension(pair.Key) + ".").StartsWith(originalFilePrefix) then
                            pair.Value.Delete()
        
                    // Add missing files to the project
                    for fileName in keepFileNameSet do
                        if isInCurrentProject fileName then
                            if not <| projectFiles.ContainsKey(fileName) then
                                templateProjectItem.ProjectItems.AddFromFile(fileName) |> ignore
                                VsManager.WriteLnToOutputPane dte ("added " + fileName)
                        else // add to another project
                            let targetProject = projects |> Seq.find (fun p -> p.Kind <> ProjectKinds.vsProjectKindSolutionFolder && fileName.StartsWith(Path.GetDirectoryName p.FullName))
                            VsManager.WriteLnToOutputPane dte ("Generating into " + targetProject.FullName)
                            targetProject.ProjectItems.AddFromFile fileName |> ignore

                member __.CheckoutFileIfRequired fileName =
                    dteWrapper.GetSourceControl()
                    |> Option.iter (fun sc -> 
                        if sc.GetIsItemUnderSC fileName && not <| sc.GetIsItemCheckedOut fileName then
                            sc.CheckOutItem fileName |> ignore<bool>
//                            checkOutAction.EndInvoke(checkOutAction.BeginInvoke(fileName, null, null))
                    )

                    



