#r "EnvDTE"
#r "EnvDTE80"
#r "System.Management"
#load "DteMacros.fs"
#r "VsLangProj"
open Macros.VsMacros
let dte = getDte()
let sln,dteProjs = getSP dte
let projs = getReferences dteProjs;; // run in interactive needs this to ensure the following line actually prints out in full
projs |> Seq.iter (fun p -> printfn "%A" (string p))
