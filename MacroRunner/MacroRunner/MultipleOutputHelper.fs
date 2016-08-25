namespace MacroRunner
open System
open System.Collections.Generic
open System.IO

type Dte = EnvDTE.DTE
// MultipleOutputHelper.ttinclude
module MultipleOutputHelper = 
    open EnvDTE

    [<AllowNullLiteral>]
    type Block () =
            member val Name:string = null with get,set
            member val Project:EnvDTE.Project = null with get,set
            member val Start = 0 with get,set
            member val Length = 0 with get,set
    
    type IManager =
        abstract member StartNewFile : Filename:string*Project option -> unit
        abstract member TemplateFile: string with get
        abstract member Dte : Dte option with get
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
            member __.StartNewFile(name, ?project) =
                if isNull name then raise <| new ArgumentNullException("name")
                printfn "Starting new file at %s" name
                let project = defaultArg project null
                currentBlock <- new Block(Name=name,Project=project)
                
            member x.StartFooter () = x.CurrentBlock <- footer
            member x.StartHeader () = x.CurrentBlock <- header
            
            member x.EndBlock () =
                if isNull currentBlock then
                    ()
                else
                    currentBlock.Length <- template.Length - currentBlock.Start
                    if currentBlock <> header && currentBlock <> footer then
                        files.Add currentBlock
                    currentBlock <- null
                    
            abstract Process : split:bool -> unit
            default x.Process split = 
                if split then
                    x.EndBlock()
                    let headerText = template.ToString(header.Start, header.Length)
                    let footerText = template.ToString(footer.Start, footer.Length)
                    let outputPath = Path.GetDirectoryName(host.TemplateFile)
                    files.Reverse()
                    for block in files do
                        let fileName = Path.Combine(outputPath, block.Name)
                        let content = headerText + template.ToString(block.Start, block.Length) + footerText
                        generatedFileNames <- fileName::generatedFileNames
                        x.CreateFile fileName content
                        template.Remove(block.Start, block.Length) |> ignore
        
            abstract CreateFile : fileName:string -> content:string -> unit
            default x.CreateFile fileName content = 
                if x.IsFileContentDifferent fileName content then
                    printfn "Writing a file to %s" fileName
                    File.WriteAllText(fileName, content)
                    
            abstract GetCustomToolNamespace: fileName: string -> string
            default x.GetCustomToolNamespace fileName = null
            
            abstract DefaultProjectNamespace:  string with get
            default x.DefaultProjectNamespace with get() = null
            
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
                    
            static member Create(host:ITextTemplatingEngineHost,template) =
                match host with 
                | :? IServiceProvider -> VsManager(host,template) :> Manager
                | _ -> Manager(host,template)
                
            interface IManager with
                override x.StartNewFile (p,s) =
                    match s with
                    | Some project -> x.StartNewFile(p,project)
                    | None -> x.StartNewFile(p,null)
                override x.EndBlock() = x.EndBlock ()
                override x.Process doMultiFile = x.Process doMultiFile
                override x.DefaultProjectNamespace = x.DefaultProjectNamespace
                override x.Dte = None
                override x.TemplateFile = host.TemplateFile
                override x.GeneratedFileNames = upcast generatedFileNames
                
        and VsManager private (host,dte,template,_templateProjectItem,checkOutAction, projectSyncAction) =
                inherit Manager(host,template)
                
                let templateProjectItem:ProjectItem = null
                let dte: Dte = dte
                let checkOutAction: Action<string> = checkOutAction // Action<String> 
                let projectSyncAction: Action<string seq> = projectSyncAction //Action<IEnumerable<String>> 
    
                interface IManager with
                    override x.Dte = Some dte
                    
                override __.DefaultProjectNamespace with get() = templateProjectItem.ContainingProject.Properties.Item("DefaultNamespace").Value.ToString()
                override __.GetCustomToolNamespace fileName = dte.Solution.FindProjectItem(fileName).Properties.Item("CustomToolNamespace").Value.ToString()
                
                override __.Process split =
                    if isNull templateProjectItem.ProjectItems then
                        ()
                    else 
                        base.Process split
                    projectSyncAction.EndInvoke(projectSyncAction.BeginInvoke(base.GeneratedFileNames, null, null))
        
                override x.CreateFile fileName content =
                    if x.IsFileContentDifferent fileName content then
                        x.CheckoutFileIfRequired(fileName)
                        File.WriteAllText(fileName, content)
                        
                //static member private x.CreateVsManager(
                internal new(host:ITextTemplatingEngineHost , template:StringBuilder ) =
                    let hostServiceProvider = host :?> IServiceProvider
                    if isNull hostServiceProvider then
                        raise <| ArgumentNullException("Could not obtain IServiceProvider")
                    let  dte = hostServiceProvider.GetService(typeof<Dte>) :?> Dte
                    if isNull dte then
                        raise <| ArgumentNullException("Could not obtain DTE from host")
                    
                    let templateProjectItem = dte.Solution.FindProjectItem(host.TemplateFile)
                    let checkOutAction fileName = dte.SourceControl.CheckOutItem fileName |> ignore
                    let projectSyncAction = fun keepFileNames -> VsManager.ProjectSync(templateProjectItem, keepFileNames)
                    VsManager(host,dte,template,templateProjectItem,Action<_>(checkOutAction), Action<_>(projectSyncAction))
                
                static member WriteLnToOutputPane(dte:Dte) (s:string) =
                    let window = dte.Windows.Item(EnvDTE.Constants.vsWindowKindOutput).Object :?> EnvDTE.OutputWindow
                    window.ActivePane.Activate ()
                    window.ActivePane.OutputString (s + Environment.NewLine)
        
                static member ProjectSync(templateProjectItem:ProjectItem, keepFileNames:string seq)  =
                    let keepFileNameSet = HashSet<string>(keepFileNames)
                    let projectFiles = Dictionary<string, ProjectItem>()
                    let dte = templateProjectItem.Collection.DTE
                    let project = templateProjectItem.Collection.ContainingProject
                    VsManager.WriteLnToOutputPane dte ("Starting ProjectSync for t4 in " + project.Name + "\\" + templateProjectItem.Name)
                    let templateProjectDirectory = Path.GetDirectoryName project.FullName
                    let sol,projects = Macros.VsMacros.getSP dte //EnvDteHelper.recurseSolutionProjects dte 
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
                            
                member x.CheckoutFileIfRequired fileName =
                    let sc = dte.SourceControl
                    if not <| isNull sc && sc.IsItemUnderSCC fileName && not <| sc.IsItemCheckedOut fileName then
                        checkOutAction.EndInvoke(checkOutAction.BeginInvoke(fileName, null, null))
                    



