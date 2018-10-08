module CodeGeneration.VS.MultipleOutputHelper

open System
open System.IO
open System.Collections.Generic

open global.BReusable
open global.BReusable.StringHelpers

open BCore.CodeGeneration.DteWrapCore
open CodeGeneration.VS.DteWrap

type Dte = EnvDTE.DTE
type Project = EnvDTE.Project
type ProjectItem = EnvDTE.ProjectItem



// leaky, project is in EnvDTE namespace
let addFileToDbSqlProj logger (targetProject:ProjectWrapper) fileName =
    logger <| sprintf "Attempting to add a file to a dbSqlProj: %s" fileName
    let fullName = targetProject.GetFullName()
    let projectItems = ref (targetProject.GetProjectItems())
    let trim1 d (s:string) = d |> Array.ofSeq |> s.Trim
    // this uses the length of full name to substring a length of fileName, that doesn't seem correct
    let toDescend =
        let parentLength = Path.GetDirectoryName fullName |> String.length 
        fileName.[parentLength ..]
        |> trim1 ['\\']
        |> trim1 ['/']
        |> String.split ["\\";"/"]
    toDescend
    |> Seq.take (toDescend.Length - 1)
    |> Seq.iter (fun td ->
        let childProjectItemOpt =
            !projectItems
            |> Seq.cast<IProjectItem>
            |> Seq.tryFind (fun pi ->
                let isMatch = Path.GetFileName(pi.get_FileNames 0s) = td
                isMatch
            )

        match childProjectItemOpt with
        | None ->
            logger <| "Failed to find \"" + td + "\""
            !projectItems
            |> Seq.cast<IProjectItem>
            |> Seq.map (fun pi -> pi.get_FileNames 0s)
            |> Seq.iter(fun pi ->
                logger <| "\tWe did however find \"" + pi + "\""
            )
        | Some childProjectItem ->
//                logger <| sprintf "OutputPane:Appears we found \"%s\" and it has projectItems:%A" td (not <| isNull childProjectItem.ProjectItems)
            if not <| isNull childProjectItem.ProjectItems then
                projectItems := childProjectItem.ProjectItems
    )

    logger <| "OutputPane: Generating \"" + fileName + "\" into " + fullName

    let projectItem =
        !projectItems
        |> fun pi -> pi.AddFromFile fileName
    logger <| "AddFromFile put it @" + projectItem.get_FileNames 0s


[<AllowNullLiteral>]
type Block () =
        member val Name:string = null with get,set
        member val Start = 0 with get,set
        member val Length = 0 with get,set


module Managers =
    open System
    open System.Text
    open Microsoft.VisualStudio.TextTemplating
    open EnvDTE80
    open BCore.CodeGeneration.DteWrapCore
    open CodeGeneration.VS

    // of the interface features we are only using the TemplateFile property, and casting it to IServiceProvider
    [<NoComparison>]
    type TemplatingEngineHost =
        | ServiceProvider of IServiceProvider * templateFileOpt:string option
        | DteDirect of Dte * templateFileOpt: string option
        | Legacy of ITextTemplatingEngineHost
        with
            member x.GetTemplateFileOpt =
                match x with
                | Legacy host -> Some host.TemplateFile
                | ServiceProvider(_,tfOpt)
                | DteDirect (_,tfOpt) -> tfOpt


    let getReadableProjects logOpt (projects: ProjectWrapper seq) =
        projects
        |> Seq.choose(fun p ->
            match DteWrap.tryGetKind logOpt p with
            | Some k ->
                if k <> unloadedProject && k <> ProjectKinds.vsProjectKindSolutionFolder then
                    Some (k,p)
                else None
            | None -> None
        )
        |> List.ofSeq

    let getIsParentProject (project:ProjectWrapper) =
        let projectDir =
            try
                project.GetFullName() |> System.IO.Path.GetDirectoryName
            with ex ->
                ex.Data.Add("getIsParentProject", project.GetName())
                reraise()
        startsWith projectDir

    type Manager (templateFilePathOpt,sb,debug) =
        let printfn f = Printf.kprintf( fun s -> if debug then printfn "%s" s) f


        let mutable currentBlock:Block = null
        let files = List<Block>()
        let footer = Block()
        let header = Block()
        let sb : StringBuilder =
            if isNull sb then invalidOp "sb must be provided"
            sb
        let mutable generatedFileNames : string list = []

        member __.GeneratedFileNames with get() = generatedFileNames
        member __.StartNewFile name =
            if isNull name then raise <| ArgumentNullException("name")
            printfn "Starting new block. destination: %s" name
            currentBlock <- new Block(Name=name,
                Start=sb.Length)

        member x.StartFooter () = x.CurrentBlock <- footer
        member x.StartHeader () = x.CurrentBlock <- header

        member __.EndBlock () =
            let log msg =
                printfn "EndBlock: %s" msg
                System.Diagnostics.Debugger.Log(0, "EndBlock", if msg |> endsWith "\r\n" then msg else sprintf "%s\r\n" msg)
            if isNull currentBlock then
                log "ending a null block!"
                ()
            else
                let len = sb.Length - currentBlock.Start

                if len < 0 then failwithf "block length is less than zero"
                currentBlock.Length <- len
                log <| sprintf "ending a block of length %i!\r\nstarted at %i\r\n" currentBlock.Length currentBlock.Start
                if currentBlock <> header && currentBlock <> footer then
                    log <| sprintf "Adding a block to the files list: %s" currentBlock.Name
                    files.Add currentBlock
                currentBlock <- null

        abstract Process : split:bool -> IDictionary<string,CreateProcessResult*string>
        default x.Process split =
            if isNull sb then
                failwithf "sb was null"

            let len = sb.Length
            if len = 0 then failwithf "No text has been added"
            let log (s:string) =
                let text = if s.EndsWith "\r\n" then s else sprintf "%s\r\n" s
                printfn "%s" text
                if System.Diagnostics.Debugger.IsAttached then
                    System.Diagnostics.Debugger.Log(0, "Logger", text)
            log "Hello debugger\r\n"
            if split then
                x.EndBlock()
                if isNull header then
                    failwithf "header was null"
                if isNull footer then
                    failwithf "footer was null"
                if isNull files then failwithf "files was null"

                let tryGetByStartEnd s l =
                    try
                        sb.ToString(s,l)
                    with ex ->
                        raise <| InvalidOperationException(sprintf "could not access substring %i %i in length %i" s l sb.Length, ex)
                let headerText = tryGetByStartEnd header.Start header.Length
                let footerText = tryGetByStartEnd footer.Start footer.Length
                let outputPath =
                    match templateFilePathOpt with
                    | Some (ValueString templateFile) -> Path.GetDirectoryName templateFile
                    | _ -> String.Empty // Path.Combine ignores string.empty, but throws on null
                log <| sprintf "outputPath is: %s" outputPath
                files.Reverse()
                files
                |> Seq.mapi (fun i f -> f,i)
                |> Seq.fold( fun (pairs: _ list) (block:Block,i:int) ->
//                    let mutable pairs : (string*(CreateProcessResult*string)) list = List.empty
                    log <| sprintf "Processing block %i:%s\r\nLength is %i" (i + 1) block.Name sb.Length

                    let fileName = Path.Combine(outputPath, block.Name)
                    let content = headerText + (tryGetByStartEnd block.Start block.Length) + footerText
                    generatedFileNames <- fileName::generatedFileNames
                    let didCreate = x.CreateFileIfDifferent fileName content
                    let pairs =
                        let creationType = if didCreate then Changed else Unchanged
                        (block.Name, (creationType, fileName))::pairs
                    sb.Remove(block.Start, block.Length) |> ignore
                    log <| sprintf "Processed block %i of %i" (i + 1) files.Count
                    pairs
                    ) List.empty
                |> dict
            else
                List.empty
                |> dict

        abstract CreateFileIfDifferent: fileName:string -> content:string -> bool
        default x.CreateFileIfDifferent fileName content =
            if x.IsFileContentDifferent fileName content then
                printfn "Writing a file to %s" fileName
                File.WriteAllText(fileName, content)
                true
            else
                false

        abstract GetCustomToolNamespace: fileName: string -> string
        default __.GetCustomToolNamespace _ = null

        abstract DefaultProjectNamespace:  string with get
        default __.DefaultProjectNamespace with get() = null

        abstract IsFileContentDifferent : fileName:string -> newContent:string -> bool
        default __.IsFileContentDifferent fileName newContent =
            let result = not <| File.Exists fileName || (File.ReadAllText fileName = newContent |> not)
            result

        member x.CurrentBlock
            with get() = currentBlock
            and set v =
                if not <| isNull x.CurrentBlock then
                    x.EndBlock()
                if not <| isNull v then
                    v.Start <- sb.Length
                currentBlock <- v

        static member Create(host:TemplatingEngineHost,sb:StringBuilder,debug) =
            let beforeConLength = sb.Length
            let manager =
                VsManager(host,sb,debug)
            if beforeConLength > sb.Length then failwithf "Someone touched me!"
            manager

        interface IManager with
            override x.StartNewFile p = x.StartNewFile p
            override x.EndBlock() = x.EndBlock ()
            override x.Process doMultiFile = x.Process doMultiFile
            override x.DefaultProjectNamespace = x.DefaultProjectNamespace
            override __.DteWrapperOpt = None
            override __.TemplateFile = templateFilePathOpt |> Option.getOrDefault String.Empty
            override __.GeneratedFileNames = upcast generatedFileNames
            override __.GetTextSize() = sb.Length

    and VsManager (templateFilePathOpt,dteWrapper:DteWrapper,sb,templateProjectItem:ProjectItem option,debug) =
            inherit Manager(templateFilePathOpt |> Option.bind Option.ofObj,sb,debug)
            let templateProjectItem = templateProjectItem |> Option.bind Option.ofObj
            let dteWrapper =
                {dteWrapper with Log =
                                    fun s ->
                                        if debug then
                                            printfn "%s" s
                                        if System.Diagnostics.Debugger.IsAttached then
                                            System.Diagnostics.Debugger.Log(0, "Logger", s)
                                        try
                                            dteWrapper.Log s
                                        with ex -> printfn "VsManager.dteWrapper.Log failed with %A" ex
                }

            interface IManager with
                override __.DteWrapperOpt = Some dteWrapper

            override __.DefaultProjectNamespace
                with get() =
                    match templateProjectItem with
                    | None -> null
                    | Some templateProjectItem ->
                        if isNull templateProjectItem then failwithf "templateProjectItem is null"
                        if isNull templateProjectItem.ContainingProject then failwithf "templateProjectItem.ContainingProject is null"
                        if isNull templateProjectItem.Properties then failwithf "templateProjectItem.ContainingProject.Properties is null"
                        templateProjectItem.ContainingProject.Properties.Item("DefaultNamespace").Value.ToString()
            override __.GetCustomToolNamespace fileName = dteWrapper.FindProjectItemPropertyValue fileName "CustomToolNamespace" //dte.Solution.FindProjectItem(fileName).Properties.Item("CustomToolNamespace").Value.ToString()

            override __.Process split =
                let log s = dteWrapper.Log s

                if sb.Length = 0 then failwithf "No text has been added"
                let results = base.Process split
                log "VsManager: Finished base.Process"

                VsManager.ProjectSyncScriptWrapped dteWrapper (templateProjectItem |> Option.getOrDefault null) base.GeneratedFileNames
                results

            override x.CreateFileIfDifferent fileName content =
                if x.IsFileContentDifferent fileName content then
                    dteWrapper.Log <| sprintf "Creating File: %s" fileName
                    x.CheckoutFileIfRequired fileName
                    File.WriteAllText(fileName, content)
                    true
                else
                    printfn "File content wasn't different, not creating: %s" fileName
                    false

            //static member private x.CreateVsManager(
            // this makes the necessary calls to get a Dte for you, as such, it should probably not exist here, rather be a code sample of a way to call this class
            internal new(host:TemplatingEngineHost, sb:StringBuilder,debug) =
                printfn "internal new VsManager current use case does actuall use it?"
                let templateFileOpt,dte =
                    let getSpFromIsp (isp:IServiceProvider) =
                            if isNull isp then
                                raise <| ArgumentNullException("Could not obtain IServiceProvider")
                            isp.GetService typeof<Dte> :?> Dte
                            |> fun dte ->
                                if isNull dte then
                                    raise <| ArgumentNullException("Could not obtain DTE from host")
                                dte
                    match host with
                    | Legacy host ->
                        Some host.TemplateFile,
                            let isp = host :?> IServiceProvider
                            getSpFromIsp isp
                    |DteDirect (dte,tfOpt) -> tfOpt,dte
                    |ServiceProvider (sp,tfOpt) -> tfOpt, getSpFromIsp sp

                // can dte.Solution even work when given String.Empty?
                let templateProjectItem =
                    templateFileOpt
                    |> Option.bind (dte.Solution.FindProjectItem >> Option.ofUnsafeNonNullable)

                //if isNull templateProjectItem then failwithf "VsManager.new: templateProjectItem is null"
                let wrapper = wrapDte dte

                VsManager(templateFileOpt,wrapper,sb,templateProjectItem,debug)

            static member FindParentProject logger (readableProjects:(string*ProjectWrapper) seq) childFileName =
                readableProjects
                |> Seq.tryFind (fun (k,p) ->
                    // let it sail past projects it could not read, if none of them match, we are throwing in the next step of the pipeline
                    try
                        getIsParentProject p childFileName
                    with _ ->
                        logger <| "expected kind= " + ProjectKinds.vsProjectKindSolutionFolder
                        // what if the exception was thrown trying to read p.Kind?
                        try
                            logger <| "failing to read project with kind= " + k
                        with _ -> ()
                        logger <| "hi! my name is " + (p.GetName())
                        reraise()
                    )
                |> Option.map snd

            // from line to 221..298
            // does this always try to add, not check and see if it needs to be added? is this always called for each file?
            static member AddFileToProject (dte:DteWrapper) (projects: ProjectWrapper seq) (fileName:string) =
                // only one this appears to be skipping in a solutionfolder
                dte.Log <| sprintf "AddFileToProject: %s" fileName
                let canReadProjects = getReadableProjects (Some dte.Log) projects
                dte.Log "Finished checking the project kinds, and getting the list of readable projects"
                // line 224
                canReadProjects

                |> Seq.tryFind (fun (k,p) ->
                    // let it sail past projects it could not read, if none of them match, we are throwing in the next step of the pipeline
                    try
                        getIsParentProject p fileName
                    with _ ->
                        dte.Log <| "expected kind= " + ProjectKinds.vsProjectKindSolutionFolder
                        // what if the exception was thrown trying to read p.Kind?
                        try
                            dte.Log <| "failing to read project with kind= " + k
                        with _ -> ()
                        dte.Log <| "hi! my name is " + (p.GetName())
                        reraise()
                    )
                |> Option.map snd
                |> function
                        | None ->
                            sprintf "%s, could not find in (%i) projects. %s" fileName (Seq.length projects) (canReadProjects |> Seq.map (fun (_,p) -> p.GetFullName()) |> delimit ",")
                            |> invalidOp
                        | Some (targetProject) ->
                            dte.Log <| sprintf "found targetProject, checking extension %s" (targetProject.GetName())
                            let fullName = targetProject.GetFullName()
                            // line 263
                            // normal adding, traversing, expanding view did not work at all.
                            if fullName.EndsWith ".dbproj" || fullName.EndsWith ".sqlproj" then
                                addFileToDbSqlProj dte.Log targetProject fileName
                            // line 299
                            else
                                dte.Log <| sprintf @"Generating ""%s"" into %s" fileName fullName
                                let projectItem = targetProject.GetProjectItems().AddFromFile fileName
                                try
                                    dte.Log <| "AddFromFile put it @" + projectItem.get_FileNames 0s
                                with _ ->
                                    dte.Log <| "get_FileNames(0) failed for " + fileName + " into " + (targetProject.GetName())
            // templateProjectItem is just the item to use as a parent if any (may only function in C# projects?) can be null
            static member ProjectSyncScriptWrapped (dte:DteWrapper) (templateProjectItemOpt: ProjectItem) (keepFileNames: string seq) =
                let log txt =
                    printfn "ProjectSyncScriptWrapped: %s" txt
                    dte.Log txt

                log "Syncing project(s)"
                let templateProjectItemOpt = templateProjectItemOpt |> Option.ofObj
                let keepFileNameSet = HashSet<string>(keepFileNames)
                let projectFiles = Dictionary<string, ProjectItem>()
                let projectOpt = templateProjectItemOpt |> Option.map (fun x -> x.Collection.ContainingProject)
                match templateProjectItemOpt, projectOpt with
                | Some pi, Some p->
                    let toLog = sprintf @"Starting ProjectSync for t4 in %s \\ %s" p.Name pi.Name
                    log toLog
                | _ -> ()
                let templateProjectDirectoryOpt = projectOpt |> Option.map (fun p -> Path.GetDirectoryName p.FullName)
                let originalFilePrefixOpt = templateProjectItemOpt |> Option.map (fun pi -> pi.get_FileNames 0s |> Path.GetFileNameWithoutExtension |> flip (+) ".")

                templateProjectItemOpt
                |> Option.bind(fun pi -> pi.ProjectItems |> Option.ofObj)
                |> Option.iter(Seq.cast<ProjectItem> >> Seq.iter (fun pi -> projectFiles.Add(pi.get_FileNames 0s, pi)))
                projectFiles
                |> Seq.filter (fun pair -> keepFileNames |> Seq.contains pair.Key |> not && not <| (Path.GetFileNameWithoutExtension pair.Key + ".").StartsWith originalFilePrefixOpt.Value)
                |> List.ofSeq
                |> fun x ->
                    printf "Removing %i items that are no longer used" x.Length
                    printfn "Names: %A" (x |> Seq.map (fun pair -> pair.Key |> List.ofSeq))
                    x
                |> Seq.iter(fun pair ->
                    log <| sprintf "removing unused item from keep list: %s" pair.Key
                    pair.Value.Delete()
                )
                log "finished removing unused items"
                let projects = dte.GetProjects()
                let isInCurrentProject (fileName:string) = match templateProjectDirectoryOpt with | Some d -> fileName.StartsWith d | None -> false
                // Add missing files to the project(s)
                keepFileNameSet
                |> Seq.iter(fun fileName ->

                    if isInCurrentProject fileName then
                        if not <| projectFiles.ContainsKey fileName then
                            try
                                templateProjectItemOpt
                                |> Option.map (fun tpi ->
                                    log <| sprintf "adding a file to Project %s" fileName
                                    tpi.ProjectItems.AddFromFile fileName)
                                |> Option.iter (fun newProjectItem ->
                                    if isNull newProjectItem then
                                        log <| sprintf "add returned null newProjectItem for %s" fileName
                                )
                            with ex ->
                                let text = sprintf "failed to AddFromFile: %s, %A" fileName ex
                                log text
                    else
                        log <| sprintf "calling AddFileToProject for %s" fileName
                        VsManager.AddFileToProject dte projects fileName
                    log "finished projectSync"
                )

            // keep this until such time as it isn't being used by the project .tt and the above method is verified to be working
            static member ProjectSync(templateProjectItem:ProjectItem, keepFileNames:string seq) =
                printfn "Syncing project(s)"
                let keepFileNameSet = HashSet<string>(keepFileNames)
                let projectFiles = Dictionary<string, ProjectItem>()
                let dte = templateProjectItem.Collection.DTE
                let dteWrapper = wrapDte dte
                let project = templateProjectItem.Collection.ContainingProject
                writeLnToOutputPane dte <| "Starting ProjectSync for t4 in " + project.Name + "\\" + templateProjectItem.Name
                let templateProjectDirectory = Path.GetDirectoryName project.FullName
                let projects = Macros.VsMacros.getSP dte |> snd |> List.map ProjectWrapper.FromProject
                let inline isInCurrentProject (fileName:string) =  fileName.StartsWith templateProjectDirectory
                let originalFilePrefix = Path.GetFileNameWithoutExtension(templateProjectItem.get_FileNames 0s) + "."
                if not <| isNull templateProjectItem.ProjectItems then
                    for projectItem in templateProjectItem.ProjectItems do
                        projectFiles.Add(projectItem.get_FileNames 0s, projectItem)
                else
                    writeLnToOutputPane dte "templateProjectItem.ProjectItems was null, not sure what all will fail from here on out"

                // Remove unused items from the project
                for pair in projectFiles do
                    if keepFileNames |> Seq.contains pair.Key |> not && not <| (Path.GetFileNameWithoutExtension pair.Key + ".").StartsWith originalFilePrefix then
                        writeLnToOutputPane dte <| sprintf "removing unused item from keep list: %s" pair.Key
                        pair.Value.Delete()
                writeLnToOutputPane dte "finished removing unused items"

                // Add missing files to the project
                for fileName in keepFileNameSet do
                    writeLnToOutputPane dte <| sprintf "adding a file %s" fileName
                    if isInCurrentProject fileName then
                        if not <| projectFiles.ContainsKey fileName then
                            templateProjectItem.ProjectItems.AddFromFile fileName |> ignore
                            writeLnToOutputPane dte <| "added " + fileName
                    else // add to another project
                        printfn "Calling AddFileToProject for external project file: %s" fileName
                        VsManager.AddFileToProject dteWrapper projects fileName
                writeLnToOutputPane dte "OutputPane: finished projectSync"

            member __.CheckoutFileIfRequired fileName =
                dteWrapper.Log <| sprintf "Checking out file if needed: %s" fileName
                dteWrapper.GetSourceControl()
                |> Option.iter (fun sc ->
                    if sc.GetIsItemUnderSC fileName && not <| sc.GetIsItemCheckedOut fileName then
                        sc.CheckOutItem fileName |> ignore<bool>
                )


    let makeManager (dte:EnvDTE.DTE,sb,debug) =
        // if this script is in the solution it is modifying, we need the EnvDTE.ProjectItem representing it, otherwise where does the main (non sub-file) output go?
        let scriptFullPath = System.IO.Path.Combine(__SOURCE_DIRECTORY__,__SOURCE_FILE__)
        let templateProjectItem:EnvDTE.ProjectItem option = dte.Solution.FindProjectItem scriptFullPath |> Option.ofObj
        printfn "Script is at %s" scriptFullPath
        templateProjectItem
        |> Option.iter(fun templateProjectItem ->
            printfn "ProjectItem= %A" (templateProjectItem.FileNames 0s)
        )
        let dteWrapper = wrapDte dte
        VsManager(Some "HelloTesting.fake.tt", dteWrapper, sb, templateProjectItem,debug)




