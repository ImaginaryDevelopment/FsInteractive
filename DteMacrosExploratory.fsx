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
let projs = getReferences dteProjs
open Macros.ProjFiles
let projFiles = getProjFiles <| Seq.map snd dteProjs

;; // run in interactive needs this to ensure the following line actually prints out in full

projs |> Seq.iter (fun p -> printfn "%A" (string p))
projFiles |> mapReferences |> Seq.iter (fun p -> printfn "%A" p)
