module fsmacros 
// http://eduardoclaudio.wordpress.com/2011/07/04/creating-visual-studio-macros-in-f
// http://fortysix-and-two.blogspot.com/2010/05/accessing-visual-studios-automation-api.html
// 
// compilation : fsc --out:fsmacros.dll --reference:envdte.dll --reference:System.Management.dll --reference:vslangproj.dll --target:library vsfsmacros.fs
// usage: 
//  #r "envdte";;
//  #r "c:\projects\fsi\fsmacros.dll";;
//  let dte = vsfsmacros.getDte();;

// http://naveensrinivasan.com/2010/05/16/visual-studio-keymaps-using-f/
// get commands would be: dte.Commands |> Seq.cast<EnvDTE.Command> |> Seq.map(fun c->c.Name,c.Bindings) |>  Array.ofSeq;;

    open EnvDTE
    open VSLangProj
    open System.Runtime.InteropServices 
    open System.Runtime.InteropServices.ComTypes
    open System.Diagnostics
    open System.Text.RegularExpressions
    open System.Management 
    open System.Management.Instrumentation
    open System.Linq 

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
       if dte<>null then 
        printfn "#load" // load up the solution.fs file
       dte
        
    
    let getSolutionProjects (sol:EnvDTE.Solution) =
        let en = sol.Projects.GetEnumerator()
        [while en.MoveNext() do yield en.Current :?> EnvDTE.Project] |> List.map (fun p -> (p.Name, p, p.Object :?> VSLangProj.VSProject))

    let getSP (dte:EnvDTE.DTE) =
        let sol = dte.Solution
        let proj = getSolutionProjects sol
        (sol,proj)
    let getDteCommands (dte:EnvDTE.DTE) = dte.Commands.Cast<EnvDTE.Command>()
    let getTextSelection (dte:EnvDTE.DTE) = dte.ActiveDocument.Selection
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
    let getStartupProject (dte:EnvDTE.DTE) = dte.Solution.Properties.Item("StartupProject").Value
    let setStartupProject (dte:EnvDTE.DTE) name = dte.Solution.Properties.Item("StartupProject").Value <- name
(* SVsTextManager section *)
    // let getSVsTextManager () = (IVsTextManager) (ServiceProvider.GetService(typeof<SVsTextManager>))
