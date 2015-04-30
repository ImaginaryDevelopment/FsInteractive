#r "EnvDTE"
#r "EnvDTE80"
#r "System.Management"
#r "System.Xml.Linq"
#r "VsLangProj"
//type ProjFileItem = {ProjName:string;FullPath:string}
//let projFileItemFromProj (p:EnvDTE.Project) = {ProjFileItem.ProjName = p.Name; FullPath= p.FullPath}

#load "DteMacros.fs"
open Macros.VsMacros
let dte = getDte()
let sln,dteProjs = getSP dte
let projs= dteProjs |> Seq.map getReferences 
open Macros.ProjFiles
let projFiles = getProjFiles dteProjs

 // run in interactive needs this to ensure the following line actually prints out in full

let showProjs () = projs |> Seq.iter (fun p -> printfn "%A" (string p))
let showProjFiles () = projFiles |> mapReferences |> Seq.iter (fun p -> printfn "%A" p)

//Seq.map joinRefsToProject dteProjs |> Seq.iter (fun p -> printfn "%A" p)

