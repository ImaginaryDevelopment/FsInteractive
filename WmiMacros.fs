namespace Macros

open System
open System.Management
open Microsoft.FSharp.Reflection
open BReusable
// http://www.iislogs.com/articles/12/
// usage: http://stackoverflow.com/a/26111634/57883

module WmiMacros =

    
    type Win32_Process = {
        Caption:string
        CommandLine:string
        CreationClassName:string
        CreationDate:DateTime
        CSCreationClassName:string
        CSName:string
        Description:string
        ExecutablePath:string
        ExecutionState:UInt16
        Handle:string
        HandleCount:UInt32
        InstallDate:DateTime
        KernelModeTime:UInt64
        MaximumWorkingSetSize:UInt32
        MinimumWorkingSetSize:UInt32
        Name:string
        OSCreationClassName:string
        OSName:string
        OtherOperationCount:UInt64
        OtherTransferCount:UInt64
        PageFaults:UInt32
        PageFileUsage:UInt32
        ParentProcessId:UInt32
        PeakPageFileUsage:UInt32
        PeakVirtualSize:UInt64
        PeakWorkingSetSize:UInt32
        Priority:UInt32
        PrivatePageCount:UInt64
        ProcessId:UInt32
        QuotaNonPagedPoolUsage:UInt32
        QuotaPagedPoolUsage:UInt32
        QuotaPeakNonPagedPoolUsage:UInt32
        QuotaPeakPagedPoolUsage:UInt32
        ReadOperationCount:UInt64
        ReadTransferCount:UInt64
        SessionId:UInt32
        Status:string
        TerminationDate:DateTime
        ThreadCount:UInt32
        UserModeTime:UInt64
        VirtualSize:UInt64
        WindowsVersion:string
        WorkingSetSize:UInt64
        WriteOperationCount:UInt64
        WriteTransferCount:UInt64
    }  
    type ScopeItem = 
        | Scope of ManagementScope
        | Creation of string*bool*bool
    type ProcessDisplay = { 
                        ProcessId: UInt32
                        ThreadCount: UInt32
                        AppPool:string
                        Name:string
                        Config:string
                        CreationDate:string
                        PageFileUsage:string
                        PeakPageFileUsage:string
                        VirtualSize:string
                        PeakVirtualSize:string
                        PeakWorkingSetSize:string
                        PrivatePageCount:string
                        InstallDate:string
                        } 
        with static member FromWin32Process (p:Win32_Process) = 
                // "C:\Windows\SysWOW64\inetsrv\w3wp.exe -ap "MarketOnce.WebApi" -v "v4.0" -l "webengine4.dll" -a \\.\pipe\iisipm31653017-1eb6-4324-8724-afdc023ff248 -h "C:\inetpub\temp\apppools\MarketOnce.WebApi\MarketOnce.WebApi.config" -w "" -m 0";
                printfn "cmd: %s" p.CommandLine
                let appPool = if p.CommandLine.Contains("-ap ") then p.CommandLine |> after "-ap \"" |> before "\"" else String.Empty
                let cleanCommandLine i = i |> after "-h" |> after "\"" |> before "\""
                let memoryToUi32 (n:UInt32) = 
                    if n> 2048u then n.ToString("n0")
                    else (n / 1024u).ToString() + "Kb"
                let memoryToUi64 (n:UInt64) = 
                    let kb = n / 1024UL
                    if kb<2000UL then kb.ToString("n0") + "Kb"
                    else (kb/1024UL).ToString("n0") + "Mb"
                let cleaned = cleanCommandLine p.CommandLine
                { 
                    AppPool = appPool
                    ProcessDisplay.ProcessId = p.ProcessId
                    ThreadCount = p.ThreadCount
                    Name = cleaned |> before ".config" |> afterLast "\\"
                    Config = p.CommandLine
                    CreationDate = p.CreationDate.ToString()
                    PageFileUsage = memoryToUi32 p.PageFileUsage
                    PeakPageFileUsage = memoryToUi32 p.PeakPageFileUsage
                    VirtualSize = memoryToUi64 p.VirtualSize
                    PeakVirtualSize = memoryToUi64 p.PeakVirtualSize
                    PeakWorkingSetSize = memoryToUi32 p.PeakWorkingSetSize
                    PrivatePageCount = memoryToUi64 p.PrivatePageCount
                    InstallDate = if p.InstallDate < DateTime.Now.AddYears(-50) then String.Empty else p.InstallDate.ToString()
                }

    let private createAdvancedScope (path:string) requiresDomainSecurity requiresPacketSecurity = 
        let scope = 
            if requiresDomainSecurity then  
                let conn = ConnectionOptions(Authority=sprintf "ntlmdomain:%s" Environment.UserDomainName)
                ManagementScope(path, conn)
            else 
            ManagementScope(path, null)
        if requiresPacketSecurity then scope.Options.Authentication <- AuthenticationLevel.PacketPrivacy
        scope.Connect()
        scope

    let private createNamedScope ``namespace`` machineName requiresSecurity =
        let path = sprintf "\\\\%s\\%s" machineName ``namespace``
        printfn "using path %s" path
        createAdvancedScope path (machineName <> "localhost") requiresSecurity
        // scope.Options.EnablePrivileges <- true
        // scope.Options.Impersonation <- ImpersonationLevel.Impersonate

    let private createScope machineName = 
        createNamedScope "root\\CIMV2" machineName false

    let private GetPropertyNames (pds:PropertyData seq) = 
        pds                
        |> Seq.map( fun pd -> pd.Name) 
        |> Array.ofSeq

    let private GetPropertyCollectionNames (mo:PropertyDataCollection) = 
        mo
        |> Seq.cast<PropertyData> 
        |> GetPropertyNames

    let private MapManagementProperty name (t:Type) (value:obj) :obj = 
        try
            if value = null then null else 
                match name with 
                | "CreationDate" | "InstallDate" ->
                    // printfn "doing %s of type %A with inputType %A" name t (value.GetType())
                    upcast ManagementDateTimeConverter.ToDateTime(value :?> String)
                | _ -> value
        with ex -> 
            printfn "fail: %s of type %A" name t
            printfn "%A" ex
            null

    let private mapWin32Process (managementObjects:ManagementObjectCollection) = 
        let fields = typeof<Win32_Process>.GetProperties() |> Seq.map (fun p -> p.Name,p.PropertyType)
        managementObjects
        |> Seq.cast<ManagementObject>
        |> Seq.map(fun mo -> printfn "commandLine=%A" mo.Properties.["CommandLine"].Value; mo)
            // |> Seq.map(fun mo -> MakeRecord(typeof<Win32_Process>,Array.ma
        |> Seq.map (fun mo -> fields |> Seq.map(fun (name,propType) -> MapManagementProperty name propType mo.[name] )  |> Array.ofSeq)
        |> Seq.map (fun values -> FSharpValue.MakeRecord(typeof<Win32_Process>, values) :?> Win32_Process)

    let QueryFiles machineName =  // scans just the root of the c drive currently
        let scope = createScope machineName
        let drive = "c:"
        let query = new ObjectQuery(sprintf "SELECT * FROM CIM_DataFile Where Drive='%s' and Path='%s'" drive "\\\\")
        use searcher = new ManagementObjectSearcher(scope, query)
        for wmiObj in searcher.Get() do 
            printfn "%A" wmiObj.["FileName"]

    /// Sample Usage: Macros.WmiMacros.QueryWmiAdvanced  (Macros.WmiMacros.ScopeItem.Creation("\\\\servername\\root\\MicrosoftIISv2", true,true)) "SELECT Name,ServerComment FROM IIsWebServerSetting" 
    /// |> Seq.map (fun e -> e.Properties.["Name"].Value,e.Properties.["ServerComment"].Value)
    let QueryWmiAdvanced (scopeInput: ScopeItem) query = 
        let scope = 
            match scopeInput with
            | Scope s -> s
            | Creation (path, requiresDomainSecurity, requiresPacketSecurity) -> createAdvancedScope path requiresDomainSecurity requiresPacketSecurity
            // createAdvancedScope path requiresDomainSecurity requiresPacketSecurity
        let query = new ObjectQuery(query)
        use searcher = new ManagementObjectSearcher(scope, query)
        use results = searcher.Get()
        results |> Seq.cast<ManagementObject>  |> Array.ofSeq

    let QueryWmi machineName ``namespace`` query requireSecurity = 
        let scope = createNamedScope ``namespace`` machineName requireSecurity // requiresSecurity
        let query = new ObjectQuery(query)
        use searcher = new ManagementObjectSearcher(scope, query)
        use results = searcher.Get()
        results |> Seq.cast<ManagementObject>  |> Array.ofSeq

    let QueryIis machineName = 
        let scope = createScope machineName
        let query = new SelectQuery( sprintf "select * FROM Win32_Process WHERE name='w3wp.exe'")
        use searcher = new ManagementObjectSearcher(scope, query)
        use results = searcher.Get()
        let records = mapWin32Process results
        printfn "%A" records
        records
        |> Seq.map ProcessDisplay.FromWin32Process
        |> Array.ofSeq

    let QueryIisV2 machineName = 
        let scope = createNamedScope "root\\MicrosoftIISv2" machineName true // requiresSecurity
        let queryText = sprintf "SELECT * FROM IIsWebVirtualDir"
        printfn "Query = %s" queryText
        let query = new SelectQuery(queryText)
        use searcher = new ManagementObjectSearcher(scope,query)
        use results = searcher.Get()
        let allResults = 
            results 
            |> Seq.cast<ManagementObject> 
            //|> Seq.map(fun mo -> mo.Properties)
            |> Array.ofSeq
        printfn "length: %i" allResults.Length
        allResults

    let QueryIisV2Virtuals machineName = 
        let scope = createNamedScope "root\\MicrosoftIISv2" machineName true // requiresSecurity
        let query = new SelectQuery (sprintf "SELECT * FROM IIsWebVirtualDir_IIsWebVirtualDir")
        use searcher = new ManagementObjectSearcher(scope,query)
        use results = searcher.Get()
        let allResults = 
            results 
            |> Seq.cast<ManagementObject> 
            //|> Seq.map(fun mo -> mo.Properties)
            |> Array.ofSeq
        printfn "length: %i" allResults.Length
        allResults

    let QueryProcesses machineName = 
        let scope = createScope machineName
        // let query = new ObjectQuery(sprintf "SELECT CommandLine FROM Win32_Process WHERE Name LIKE '%s%%'" "explorer") //" AND Name LIKE '%%%s'"
        let query = new ObjectQuery(sprintf "SELECT * FROM Win32_Process") //" AND Name LIKE '%%%s'"
        let fields = typeof<Win32_Process>.GetProperties() |> Seq.map (fun p -> p.Name,p.PropertyType)
        use searcher = new ManagementObjectSearcher(scope, query)
        let values =
            searcher.Get()
            |> mapWin32Process
        printfn "values %A" values
        let procs = values |> Array.ofSeq
        procs

    // maybe try http://www.nullskull.com/faq/283/c-net-get-the-logged-on-user-on-windows-7-using-wmi-query.aspx ?
    let QueryUsers machineName = 
        let scope = createScope machineName
        let query = new ObjectQuery("SELECT username FROM Win32_ComputerSystem")
        use searcher = new ManagementObjectSearcher(scope, query)
        searcher.Get()
        |> Seq.cast<ManagementObject>
        |> Seq.map (fun mo -> mo.["username"])
        |> Array.ofSeq

    