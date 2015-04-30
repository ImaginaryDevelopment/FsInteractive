#r "EnvDTE"
#r "DteMacros"
#r "VsLangProj"
open Macros.VsMacros
let dte = getDte()
let sln,dteProjs = getSP dte
let projs = getReferences dteProjs
projs |> Seq.map (fun p -> printfn "%A" (string p))
