namespace Macros
open System
open System.IO

module SlnProjMacros =
    let private findUnder extension path = 
        Directory.EnumerateFiles(path, sprintf "*.%s" extension, SearchOption.AllDirectories)
    let findSln searchRoot = findUnder "sln" searchRoot
    let findConfigUnder searchRoot = findUnder "config" searchRoot
    type ConfigType = 
        | PackageRef of version:string
        | Config of newVersion:string*oldVersion:string
        with 
            member x.ToDump() =
                sprintf "%A" x
    type Config = {Version:ConfigType; Name:string; Src:string; Raw:string}
open SlnProjMacros

open System.Xml.Linq
open System.Xml.XPath
open BReusable.StringHelpers
open BReusable.Xml

module PackageConfigs =

    let private listFrom2nd (x,y) = x, y |> List.ofSeq

    let getPackageConfigs slnDir = 
        findConfigUnder slnDir
        |> Seq.filter(fun x -> Path.GetFileName x = "packages.config")
        |> List.ofSeq
    //let pConfigs = getPackageConfigs()
    let readPConfig path = 
        XDocument.Load(uri=path)
        |> fun x -> x.XPathSelectElements "//package"
    // Package Config

    let getSortedPackages targetFolder configs =
        configs
        |> Seq.map (fun x -> x |> after targetFolder |> before "packages.config" |> trim1 "\\", readPConfig x)
        |> Seq.map (fun (path, x) ->
            x |> Seq.map(fun x ->
                let text = x |> string
                let pId = x |> getAttrValue "id" |> Option.getOrDefault String.Empty
                {Src=path; Name=pId; Version=getAttrValue "version" x |> Option.getOrDefault String.Empty |> PackageRef;Raw=text}
            )
        )
        |> Seq.concat
        |> Seq.trySortByDesc(fun x -> x.Version)
        |> Seq.groupBy(fun x -> x.Name)
        |> Seq.map listFrom2nd
        |> Seq.filter(fun (_,x) -> x.Length > 1 && x |> Seq.map (fun x -> x.Version) |> Seq.distinct |> Seq.length |> fun x -> x > 1)
        |> Seq.trySortBy (fun (x,_) -> x <> "FSharp.Core")
        
module AppWebConfigs =
    //open System.Linq
    open BReusable

    let getAppConfigs slnDir =
        slnDir
        |> findConfigUnder
        |> Seq.filter(fun x -> Path.GetFileName x |> String.equalsI "app.config" || Path.GetFileName x |> String.equalsI "web.config")
        |> List.ofSeq

    let readConfig path =
        XDocument.Load(uri=path)
        |> fun x -> x.Root.XPathSelectElement "runtime" 
        |> Option.ofObj
        |> Option.map (XElement.GetElements1 "assemblyBinding" >> Seq.collect (XElement.GetElements1 "dependentAssembly") >> List.ofSeq)

    let getSortedRedirects targetFolder configs =
        configs
        // eliminate items without binding redirects
        |> Seq.choose(fun path ->
            match readConfig path with
            | Some [] -> None
            | Some x -> Some(path,x)
            | None -> None
        )
        |> Seq.collect (fun (path,x) -> 
            x
            |> Seq.map(fun x ->
                try
                    let elements = x.Elements() |> Seq.map string |> List.ofSeq |> Seq.ofList
                    let br =  x |> XElement.GetElements1 "bindingRedirect" |> Seq.single
                    let nv = br |> getAttrValue "newVersion" |> Option.getOrDefault String.Empty 
                    let ov = br |> getAttrValue "oldVersion" |> Option.getOrDefault String.Empty 
                    let ct = ConfigType.Config(nv,ov)
                    {   Name= x |> XElement.GetElements1 "assemblyIdentity" |> Seq.single |> getAttrValue "name" |> Option.getOrDefault String.Empty
                        Version= ct
                        Raw= elements |> delimit "\r\n"
                        Src=path |> after targetFolder |> before ".config" |> trim1 "\\"
                    }
                with ex ->
                    ex.Data.Add("Path",path)
                    ex.Data.Add("Element",x)
                    reraise()
            )
        )
        |> Seq.groupBy(fun x -> x.Name)
        |> Seq.filter(fun (_, items) -> items |> Seq.map (fun x -> x.Version) |> Seq.distinct |> Seq.length |> fun x -> x > 1)

