module pathmacros
// vs path macros
// compilation : 
(*
fsc --out:vspathmacros.dll --reference:vslangproj.dll --target:library vspathmacros.fs

copy *.dll "C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\PublicAssemblies"
  *)

(* Tfs section *)
// module tfsfsmacros =
  let vscomntools = System.Environment.GetEnvironmentVariable("VS120COMNTOOLS")
  let idePath = lazy(
      vscomntools
      |> System.IO.Path.GetDirectoryName
      |> System.IO.Path.GetDirectoryName
      |> fun d-> System.IO.Path.Combine(d,"IDE")
    )
  let tfPath = lazy(
      System.IO.Path.Combine(idePath.Value,"tf.exe")
    )
  let getTfRefs () = 
    let formatRefPath (dllRef:string) = 
      sprintf "%s @\"%s\"" "#r" 
      <| System.IO.Path.Combine(idePath.Value,"ReferenceAssemblies","v2.0",dllRef)
    printfn "%s" <| formatRefPath("Microsoft.TeamFoundation.Client.dll")
    printfn "%s" <| formatRefPath "Microsoft.TeamFoundation.VersionControl.Client.dll"
    printfn "%s" <| formatRefPath "Microsoft.TeamFoundation.VersionControl.Common.dll"