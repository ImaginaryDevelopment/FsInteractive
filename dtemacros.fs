namespace Macros
open EnvDTE
open EnvDTE80
open VSLangProj
module Helpers =
    type System.String with
        member x.ContainsI(s:string) = if x = null then false else x.IndexOf(s,System.StringComparison.InvariantCultureIgnoreCase) >= 0
open Helpers
module VsMacros =
  type CultureInfo = System.Globalization.CultureInfo
  type PluralizationService = System.Data.Entity.Design.PluralizationServices.PluralizationService
  let createPluralizer () = 
    try
        PluralizationService.CreateService(CultureInfo "en") // https://msdn.microsoft.com/en-us/library/system.data.entity.design.pluralizationservices.pluralizationservice(v=vs.110).aspx
        |> Choice1Of2
    with ex -> Choice2Of2 ex

// sources:
// http://eduardoclaudio.wordpress.com/2011/07/04/creating-visual-studio-macros-in-f
// http://fortysix-and-two.blogspot.com/2010/05/accessing-visual-studios-automation-api.html
// 
// compilation : 
//  fsc --out:dtemacros.dll --reference:envdte.dll --reference:System.Management.dll --reference:vslangproj.dll --target:library vsfsmacros.fs
// usage: 
//  #r @"C:\projects\fsi\dtemacros.dll";#r "EnvDTE"; open Macros.VsMacros;; let dte=getDte();;
// alternate usage if installed to public assemblies:
//  #r "dtemacros.dll";#r "EnvDTE"; open Macros.VsMacros; let dte=getDte();;

// sources:
//  http://naveensrinivasan.com/2010/05/16/visual-studio-keymaps-using-f/
// get commands would be: dte.Commands |> Seq.cast<EnvDTE.Command> |> Seq.map(fun c->c.Name,c.Bindings) |>  Array.ofSeq;;

  open System
  open System.Runtime.InteropServices 
  open System.Runtime.InteropServices.ComTypes
  open System.Text.RegularExpressions
  open System.Management 
//  open System.Linq 

  [<DllImport("ole32.dll")>] 
  extern int internal GetRunningObjectTable(uint32 reserved, IRunningObjectTable& pprot) 

  [<DllImport("ole32.dll")>] 
  extern int internal CreateBindCtx(uint32 reserved, IBindCtx& pctx) 

  let getParentPid (currentPid:int32) =
      let osearch = new ManagementObjectSearcher("root\\cimv2", System.String.Format("Select * from Win32_Process where ProcessId={0}",currentPid))
      let moCol = osearch.Get()
      let thisProc = [for mo in moCol do yield mo] |> List.head
      let pp = thisProc.GetPropertyValue("ParentProcessId")
      pp :?> uint32

  let getWindowNames () = 
        let dvproc = System.Diagnostics.Process.GetProcessesByName("devenv")
        let p = Array.map (fun (px:System.Diagnostics.Process) -> px.MainWindowTitle) dvproc
        p
  let getDteByWindowName windowName =
    let getPidByWindowName (wn:string) =
        let dvproc = System.Diagnostics.Process.GetProcessesByName("devenv")
        let p = Array.tryFind (fun (px:System.Diagnostics.Process) -> 
                px.MainWindowTitle.ToLower().Contains(wn.ToLower())) dvproc
        match p with
        |Some x -> x.Id
        |None -> failwith "No process contains a window with this title"
    let mutable (prot:IRunningObjectTable) = null  
    let mutable (pmonkenum:IEnumMoniker) = null 
    let (monikers:IMoniker[]) =  Array.create 1 null 
    let pfeteched = System.IntPtr.Zero 
    let mutable (ret:obj) = null 
    let pid = getPidByWindowName windowName
    let endpid = sprintf ":%d" pid 
    try
       if (GetRunningObjectTable(0u, &prot) <> 0) || (prot = null) then  
           failwith "Error opening the ROT" 
       prot.EnumRunning(&pmonkenum) 
       pmonkenum.Reset() 
       while pmonkenum.Next(1, monikers, pfeteched) = 0 do 
           let mutable (insname:string) = null 
           let mutable (pctx:IBindCtx) = null 
           CreateBindCtx(0u, &pctx) |> ignore 
           (monikers.[0]).GetDisplayName(pctx, null, &insname); 
           Marshal.ReleaseComObject(pctx) |> ignore 
           if insname.StartsWith("!VisualStudio.DTE") && insname.EndsWith(endpid) then 
               prot.GetObject(monikers.[0], &ret) |> ignore 
    finally 
       if prot <> null then Marshal.ReleaseComObject(prot) |> ignore 
       if pmonkenum <> null then Marshal.ReleaseComObject(pmonkenum) |> ignore 
    (ret :?> EnvDTE.DTE)

  let getDte () = 
     let mutable (prot:IRunningObjectTable) = null  
     let mutable (pmonkenum:IEnumMoniker) = null 
     let (monikers:IMoniker[]) =  Array.create 1 null 
     let pfeteched = System.IntPtr.Zero 
     let mutable (ret:obj) = null 
     let pid = getParentPid (System.Diagnostics.Process.GetCurrentProcess().Id)
     let endpid = sprintf ":%d" pid 
     try
         if (GetRunningObjectTable(0u, &prot) <> 0) || (prot = null) then  
             failwith "Error opening the ROT" 
         prot.EnumRunning(&pmonkenum) 
         pmonkenum.Reset() 
         while pmonkenum.Next(1, monikers, pfeteched) = 0 do 
             let mutable (insname:string) = null 
             let mutable (pctx:IBindCtx) = null 
             CreateBindCtx(0u, &pctx) |> ignore 
             (monikers.[0]).GetDisplayName(pctx, null, &insname); 
             Marshal.ReleaseComObject(pctx) |> ignore 
             if insname.StartsWith("!VisualStudio.DTE") && insname.EndsWith(endpid) then 
                 prot.GetObject(monikers.[0], &ret) |> ignore 
     finally 
         if prot <> null then Marshal.ReleaseComObject(prot) |> ignore 
         if pmonkenum <> null then Marshal.ReleaseComObject(pmonkenum) |> ignore 
     let dte = (ret :?> EnvDTE.DTE) 
     if dte<>null && System.IO.File.Exists <| dte.Solution.FullName+".fs" then 
      printfn "%s \"%s\"" "#load" <| dte.Solution.FullName+".fs" // load up the solution.fs file
     dte

  let rec getSolutionFolderProjects(solutionFolder:EnvDTE.Project) : EnvDTE.Project list = 

      [
          for i in 1..solutionFolder.ProjectItems.Count do
              let subProject = solutionFolder.ProjectItems.Item(i).SubProject
              if subProject <> null then 
                match subProject.Kind with
                |EnvDTE.Constants.vsProjectKindSolutionItems  -> printfn "yay a solution Folder! %s" subProject.Name ; yield! getSolutionFolderProjects subProject
                |EnvDTE.Constants.vsProjectItemKindMisc -> printfn "found kindmisc %A %s" subProject.Kind subProject.Name
                | _ -> printfn "found wildcard %A %s" subProject.Kind subProject.Name; yield subProject
      ]

  let getSolutionProjects (sol:EnvDTE.Solution) =
      let en = sol.Projects.GetEnumerator()
      [while en.MoveNext() do
          let project = en.Current :?> EnvDTE.Project
          match project with
          | null -> ()
          | p when 
            p.Kind= EnvDTE.Constants.vsProjectItemKindMisc ||
            p.Kind= EnvDTE.Constants.vsProjectKindSolutionItems ||
            p.Kind= EnvDTE.Constants.vsProjectKindMisc -> 
                //printfn "found container kind %A %s" p.Kind p.Name
                yield! getSolutionFolderProjects p
          | _ -> 
            //printfn "found a solution project %s %A" project.Name project.Kind
            yield project
          ] (* ,p.Object :?> VSLangProj.VSProject *)

  let getSP (dte:EnvDTE.DTE) = // get solution with projects
      let sol = dte.Solution
      let proj = getSolutionProjects sol
      (sol,proj)
  [<NoComparison>]
  type ProjReference = {Name:string;R:VSLangProj.Reference;Project:EnvDTE.Project option} with 
    member this.IsResolved = String.IsNullOrEmpty this.R.Path = false
    member this.IsGac = this.IsResolved && this.R.Path.ContainsI "gac"
    override this.ToString() = 
        let proj = match this.Project with | Some p -> sprintf """;Proj="%s""" p.Name + "\"" |None -> String.Empty
        sprintf """{Name="%s";IsResolved=%A;IsGac=%A;Path="%s"%s}""" this.Name this.IsResolved this.IsGac this.R.Path proj

  [<NoComparison>]
  type ReferencesByProj = { ProjectName:string; Refs:ProjReference seq; VsProj:VSLangProj.VSProject; EnvProj:EnvDTE.Project; } with
    override this.ToString() = sprintf """{ProjectName="%s";Refs=%A} """ this.ProjectName (this.Refs |> Seq.map  string)

  let mapReferences (vsProj:VSLangProj.VSProject): ProjReference seq = 
    vsProj.References 
    |> Seq.cast<VSLangProj.Reference> 
    |> Seq.map(fun r -> 
        {
            ProjReference.Name= r.Name
            R= r
            Project = if r.SourceProject  = null then None else Some r.SourceProject
        }
    )
  let getReferences (proj : EnvDTE.Project) =  // created with help from https://mhusseini.wordpress.com/2013/05/29/get-project-references-from-envdte-project/
    proj.FullName |> ignore
    let vsProj = proj.Object :?> VSLangProj.VSProject
    {ReferencesByProj.ProjectName = proj.Name; Refs = mapReferences vsProj; EnvProj = proj; VsProj=vsProj }

  let getDteCommands (dte:EnvDTE.DTE) = dte.Commands |> Seq.cast<EnvDTE.Command>

  let getDteCommandsByName (dte:EnvDTE.DTE) search = getDteCommands dte |> Seq.filter(fun f -> f.Name.Contains(search)) |> Seq.map(fun f-> f.Name) |> Array.ofSeq

  let getDteCommandsByBinding (dte:EnvDTE.DTE) (binding:string) = 
    let convert (c:EnvDTE.Command) = ((c.Bindings) :?> System.Object[] |> Seq.cast<string>)
    getDteCommands dte 
    |> Seq.filter(fun f-> convert f |> Seq.exists(fun (b:string) -> b.Contains(binding)))
    |> Seq.map(fun f-> f.Name,f.Bindings)
    |> Array.ofSeq

  let getTextSelection (dte:EnvDTE.DTE) = dte.ActiveDocument.Selection
  let getDocuments (dte:EnvDTE.DTE) = dte.Documents |> Seq.cast<Document>
  let closeDocument dte documentFullName = dte |> getDocuments |> Seq.tryFind(fun d-> d.FullName = documentFullName)
  let activateDocument dte documentFullName = 
    dte
    |> getDocuments
    |> Seq.tryFind (fun d -> d.FullName = documentFullName)
    |> Option.iter (fun d -> d.Activate())

  let SolutionExplorerWindow = "{3AE79031-E1BC-11D0-8F78-00A0C9110057}"
  let SolutionFolder = "{66A26720-8FB5-11D2-AA7E-00C04F688DDE}"

  let getProjectsByRegex regex (pl:EnvDTE.Project list) =
    let re = new Regex (regex, System.Text.RegularExpressions.RegexOptions.IgnoreCase)
    pl |> List.filter (fun p -> re.Match(p.Name).Success)
      
  let getVSProjectsByRegex re pl =
    (getProjectsByRegex re pl) |> List.map (fun p -> p.Object :?> VSProject)

  let addProjectToSolution (sol:EnvDTE.Solution) (path:string) (suffix:string) (project:string) =
      sol.AddFromFile((path + "\\" + project + "."+ suffix), false) |> ignore
      ()
  let getDebugOutput (dte2:EnvDTE80.DTE2) = // http://stackoverflow.com/questions/25097387/send-the-entire-debug-console-output-to-clipboard
    let sel = dte2.ToolWindows.OutputWindow.OutputWindowPanes.Item("Debug").TextDocument.Selection
    sel.StartOfDocument(false)
    sel.EndOfDocument(true)
    sel.Text
  let getStartupProject (dte:EnvDTE.DTE) = dte.Solution.Properties.Item("StartupProject").Value
  let setStartupProject (dte:EnvDTE.DTE) name = dte.Solution.Properties.Item("StartupProject").Value <- name
  let gotoLine (dte:EnvDTE.DTE) line = dte.ActiveDocument.Activate();dte.ExecuteCommand("Edit.GoTo",line.ToString());;

module CodeModel =  // mostly from EnvDteHelper.ttinclude
    let rec FindCodeModelInterfaces (ces: CodeElement seq) = 
      [
        for ce in ces do
            match ce.Kind with
            | vsCMElement.vsCMElementInterface -> yield ce :?> CodeInterface
            | vsCMElement.vsCMElementNamespace -> yield! FindCodeModelInterfaces((ce :?> CodeNamespace).Members |> Seq.cast<CodeElement>)
            | _ -> printfn "Kind not covered %s %A" ce.Name ce.Kind
      ]

    let FindCodeModelProperties (ci:CodeInterface) = 
        [ for cp in ci.Members |> Seq.cast<CodeElement> |> Seq.filter (fun e -> e :?> CodeProperty <> null) do
            yield cp :?> CodeProperty]
    let rec FindCodeModelImplementedInterfaceProperties (ci:CodeInterface) =
        if ci.Bases = null then List.empty else
            [
                for ii in ci.Bases |> Seq.cast<CodeInterface> do
                    yield! FindCodeModelProperties ii
                    yield! FindCodeModelImplementedInterfaceProperties ii
            ]



module ProjFiles = 
    module Seq = // static extensions on Seq
            let mapCarry f = Seq.map (fun value -> (value,f value))
            let mapCarrySnd f = Seq.map( fun (x,v) -> (x, v, f v))
            
    open System.Xml.Linq

    type ProjFileItem = {Name:string;FullPath:string}

    //Filters on project name contains "test"
    let getProjFile (proj : EnvDTE.Project)= 
        {ProjFileItem.Name = proj.Name; FullPath= proj.FullName}

    let getNonTestProjFiles projs = Seq.map getProjFile projs |> Seq.filter( fun fp -> fp.Name.ContainsI("test") = false)

    let getDoc (pfi:ProjFileItem) = System.Xml.Linq.XDocument.Load(pfi.FullPath)
    [<NoComparison>]
    type ItemGroupItem = {Condition:string option; Include:string option; Raw: XElement}
    [<NoComparison>]
    type ItemGroup = {Condition:string option;Items: ItemGroupItem seq}
    [<NoComparison>]
    type ProjFile = {Doc:XDocument;ItemGroups: ItemGroup seq}
    let private getAttribValueOrNone  name (x:XElement) = 
            let xa= x.Attribute(XNamespace.None + name)
            if xa=null then None
            else
                if xa.Value = null then None
                else Some xa.Value
    let getReferences (projDoc:XDocument) =
        let rootns s = projDoc.Root.Name.Namespace + s
        let projEl = projDoc.Element(rootns "Project")
        let itemGroupElements = projEl.Elements(rootns "ItemGroup") |> Seq.cast<XElement>
        let itemGroups : ItemGroup seq = 
            let igiFromXE xe = {ItemGroupItem.Condition = getAttribValueOrNone "Condition" xe; Include= getAttribValueOrNone "Include" xe; Raw=xe }
            itemGroupElements
            |> Seq.map (fun igElement -> {Condition =getAttribValueOrNone "Condition" igElement;Items=Seq.map igiFromXE <| igElement.Elements() })
        itemGroups
    
    [<NoComparison>]
    type ProjFileDteCollection = {MatchedRefs: VsMacros.ProjReference*XElement seq; ProjRefs: VsMacros.ProjReference seq; Refs: ItemGroup seq  }
    [<NoComparison>]
    type ProjFilesMated = {ProjectName:string;ProjFileDteItems:ProjFileDteCollection}
//    let joinRefsToProject (proj:EnvDTE.Project) =
//        let projRefs = VsMacros.getReferences proj
//        let pfi = getProjFile proj 
//        let doc = getDoc pfi
//        let itemGroups = getReferences doc
//        let flattenIG= Seq.map(fun ig -> ig.Items |> Seq.collect ( fun i-> (ig.Condition,i)))
//        let igsWithCondition = itemGroups |> Seq.map flattenIG
//
//        let matches = 
//            query{
//                for ref in projRefs.Refs do
//                join (groupCondition,igi) in igsWithCondition on (ref.Name = igi.Include )
//                select (ref,item)
//            }
//        let unmatchedRefs =
//            query{
//                for ref in projRefs.Refs do
//                where (items|> Seq.exists (fun item ->item= ref.Name) |> not)
//            }
//        {ProjFilesMated.ProjectName=proj.Name; ProjFileDteItems= {ProjFileDteCollection.MatchedRefs = matches;ProjRefs = projRefs.Refs; Refs = itemGroups}}
(* SVsTextManager section *)
    // let getSVsTextManager () = (IVsTextManager) (ServiceProvider.GetService(typeof<SVsTextManager>))


// more helpful resources:
// http://www.viva64.com/en/b/0169/