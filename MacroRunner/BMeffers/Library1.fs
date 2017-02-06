namespace BMeffers

open System.ComponentModel.Composition
open MacroRunner.Schema
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module VolumeEffects=
    open System
    open System.Runtime.InteropServices

//    let private VolumeMute = 0x80000
//    let private VolumeUp = 0xA00000
//    let private VolumeDown = 0x900000
//    let appCommand = 0x319
//
////  [<DllImport("ole32.dll")>] 
////  extern int internal GetRunningObjectTable(uint32 reserved, IRunningObjectTable& pprot) 
//    [<DllImport("user32.dll")>]
//    extern IntPtr private SendMessageW(IntPtr hWnd, int Msg, IntPtr wParam)
    [<DllImport("user32.dll")>]
    extern void private keybd_event(byte bVk, byte bScan, UInt32 dwFlags, int dwExtraInfo);
    let private keyboardEvent key = 
        keybd_event(key, 0uy, 0u, 0)
    let mute() = 
        keyboardEvent 173uy
    let volumeDown() = 
        keyboardEvent 174uy
    let volumeUp() = 
        keyboardEvent 175uy
    ()

// https://blogs.msdn.microsoft.com/jomo_fisher/2010/03/09/neat-samples-extend-your-f-program-with-mef/
[<Export(typeof<ISideEffect>)>]
// http://stackoverflow.com/questions/13139181/how-to-programmatically-set-the-system-volume
type VolumeDownEffect() = 
    interface ISideEffect with
        member __.Description = "Turn the Volume Down when run"
        member __.Execution _ = 
            VolumeEffects.volumeDown()
            printfn "Turned volume down, maybe"
        member __.Name = "VolumeDown"
[<Export(typeof<ISideEffect>)>]
type VolumeMuteEffect() = 
    interface ISideEffect with
        member __.Description = "Mute Volume when run"
        member __.Execution _ = 
            VolumeEffects.mute()
            printfn "muted, maybe"
        member __.Name = "Mute"