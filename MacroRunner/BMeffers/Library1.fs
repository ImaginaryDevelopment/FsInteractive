namespace BMeffers

open System.ComponentModel.Composition
open MacroRunner.Schema
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module VolumeDownEffects =
    ()

[<Export(typeof<ISideEffect>)>]
type VolumeDownEffect() = 
    interface ISideEffect with
        member x.Description = "Turn the Volume Down when run"
        member x.Execution _ = printfn "Turned volume down, maybe"
        member x.Name = "VolumeDown"