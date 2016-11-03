module MacroRunner.NuGetAlternative
open System
open System.IO
//open System.IO.Compression.FileSystem

#if INTERACTIVE
#r "System.IO.Compression.FileSystem"
open System.IO //open it again, since it has changed
#I __SOURCE_DIRECTORY__
#endif
// not really needed if not interactive ( you can just add a project nuget reference )
module NugetAlternative = 

    let srcDir = __SOURCE_DIRECTORY__
    let srcFile = __SOURCE_FILE__

    if String.IsNullOrEmpty srcDir = false && Directory.Exists srcDir then
        Environment.CurrentDirectory <- srcDir
    //needs package

    let flip f x1 x2 = f x2 x1
    let combine basePath segment= Path.Combine(basePath,segment)
    let combine2 basePath segment1 segment2 = Path.Combine(basePath,segment1,segment2)
    let combine3 basePath segment1 segment2 segment3 = Path.Combine(basePath,segment1,segment2,segment3)
    let packagesTemp = combine (Path.GetTempPath()) (Path.GetFileNameWithoutExtension srcFile)

    type PackageLocation = 
        | ById of string
        | ByIdVer of string * string
        | NameAndUrl of string * string

    let getPackage packageLocation = //http://stackoverflow.com/a/14895173/57883
        let filename,url = 
            match packageLocation with
            | ById packageId -> sprintf "%s.zip" packageId, sprintf "https://www.nuget.org/api/v2/package/%s/" packageId
            | ByIdVer (packageId,ver) -> sprintf "%s.%s.zip" packageId ver,sprintf "https://www.nuget.org/api/v2/package/%s/%s" packageId ver
            | NameAndUrl (name,loc) -> name, loc
        if Directory.Exists packagesTemp = false then
            Directory.CreateDirectory packagesTemp |> ignore
        let targetPath = combine packagesTemp filename 
        printfn "targetPath for package is %s" targetPath
        if File.Exists(targetPath) = false then
            printfn "downloading package from %s" url
            printfn "downloading package to %s as %s" packagesTemp filename
            use wc = new System.Net.WebClient()
            wc.DownloadFile(url,targetPath)
            printfn "downloaded package to %s" targetPath
        targetPath

    let extractPackage fullPath = 
        printfn "extractPackage from %s" fullPath
        if Path.IsPathRooted fullPath = false then
            failwithf "unrooted path passed to extract package: %s" fullPath
        if fullPath.EndsWith(".zip") = false && fullPath.EndsWith(".nupkg") = false then
            failwithf "package path must be a full package path including the extension %s" fullPath
        if File.Exists fullPath = false then
            failwithf "package must exist: %s" fullPath

        let target = combine (Path.GetDirectoryName fullPath) (Path.GetFileNameWithoutExtension fullPath)
        if Path.IsPathRooted target = false then
            failwithf "unrooted path to extract package to : %s" fullPath

        printfn "extracting package to %s" target
        Compression.ZipFile.ExtractToDirectory(fullPath,target)
        if Directory.Exists target = false then
            failwithf "Extraction occurred but did not create dir: %s" target
        printfn "extracted to %s" target

    let copyPackageDll packageFullPath dllRelPath (dll:string) = 
        if Path.IsPathRooted packageFullPath = false then
            failwithf "packagePath must be absolute and rooted:%s" packageFullPath
        let dll = if dll.EndsWith(".dll") then dll else dll + ".dll"
        if File.Exists(dll) = false then
            printfn "starting copy from package %s" packageFullPath
            let packageExtractedPath = combine (Path.GetDirectoryName packageFullPath) (Path.GetFileNameWithoutExtension(packageFullPath))
            if Directory.Exists packageExtractedPath = false then
                extractPackage(packageFullPath)
            let dll = if dll.EndsWith(".dll") then dll else dll + ".dll"
            let fileTarget = combine2 packageExtractedPath dllRelPath dll
            printfn "copying from %s" fileTarget
            printfn "copying to %s" (Path.GetFullPath dll)
            if Directory.Exists (Path.GetDirectoryName fileTarget) = false then
                failwithf "file copy from parentDir does not exist: %s" (Path.GetDirectoryName fileTarget)
            File.Copy(fileTarget,dll)
            printfn "copied package dll to %s" (Path.GetFullPath(dll))
        else
            printfn "dll already exists at %s" (Path.GetFullPath dll)

    let getPackageForReference packageLocation dllRelPath dll =
        printfn "Checking %s and %s for %s" Environment.CurrentDirectory srcDir dll
        if File.Exists dll = false && File.Exists (combine srcDir dll) = false then
            let packageFullPath = getPackage packageLocation
            copyPackageDll packageFullPath dllRelPath dll

    getPackageForReference (PackageLocation.ById("Microsoft.CodeAnalysis.CSharp")) @"lib\net45\" "Microsoft.CodeAnalysis.CSharp"
    getPackageForReference (PackageLocation.ById("Microsoft.CodeAnalysis.Common")) @"lib\net45\" "Microsoft.CodeAnalysis"
#if INTERACTIVE
#if MONO

#I "/usr/lib/mono/4.5/Facades/"
#r "/usr/lib/mono/4.5/Facades/System.Runtime.dll"
#r "./packages/System.Collections.Immutable.1.1.33-beta/lib/portable-net45+win8+wp8+wpa81/System.Collections.Immutable"
#else

#r "System.Runtime"
#r "System.Collections.Immutable"
#r @"Microsoft.CodeAnalysis.dll"
#r @"Microsoft.CodeAnalysis.CSharp.dll"
#endif
#r "System.Text.Encoding"
#r "System.Threading.Tasks"
#endif
