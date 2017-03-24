open System
open System.Diagnostics
open System.Runtime.InteropServices
module PInvoke = 
    open System.Text

    //type Message =
    //    |GetText
    //    |Close
    let private WM_GETTEXT = 0x000D
    let private WM_CLOSE = 0x0010

    type EnumThreadDelegate= delegate of IntPtr * IntPtr -> bool

    [<DllImport("user32.dll")>]
    extern [<return: MarshalAs(UnmanagedType.Bool)>] bool private EnumThreadWindows(int dwThreadId, EnumThreadDelegate lpfn, IntPtr lParam);
    [<DllImport("user32.dll")>]
    extern [<return: MarshalAs(UnmanagedType.Bool)>] bool private IsWindowVisible(IntPtr hWnd);

    [<DllImport("user32.dll", CharSet = CharSet.Auto)>]
    extern IntPtr private SendMessage(IntPtr hWnd, int msg, int wParam, StringBuilder lParam);

    [<DllImport("user32.dll", CharSet = CharSet.Unicode, SetLastError=true)>]
    extern [<return: MarshalAs(UnmanagedType.Bool)>] bool private DestroyWindow(IntPtr hwnd);
    [<DllImport("user32.dll")>]
    extern [<return: MarshalAs(UnmanagedType.Bool)>] bool private IsWindowEnabled(IntPtr hWnd);
    let destroyWindow hWnd = DestroyWindow hWnd

    let getWindowName hWnd = 
        let sb = StringBuilder(1000)
        SendMessage(hWnd, WM_GETTEXT, sb.Capacity, sb) |> ignore<nativeint>
        sb.ToString()

    let closeWindow hWnd = SendMessage(hWnd, WM_CLOSE, 0, null)
    //let getIsWindowVisible
    let getThreadWindows (threadId:int) : _ list = 
        let items = ResizeArray()
        let withData (hWnd:IntPtr) (lParam:IntPtr) = 
            let _ = items.Add(hWnd,lParam)
            true
        let f = EnumThreadDelegate withData

        EnumThreadWindows (threadId, f, IntPtr.Zero) |> ignore<bool>
        items
        |> Seq.cast<IntPtr*IntPtr>
        |> List.ofSeq
    let getIsVisible hWnd = 
        IsWindowVisible(hWnd)

open PInvoke


let lp = Process.GetProcesses() |> Seq.filter(fun p -> p.ProcessName.StartsWith("L")) |> Seq.minBy(fun p -> p.StartTime) 

let tryAsForm hWnd = 
    try
        match System.Windows.Forms.Form.FromHandle(hWnd) with
        | null -> ()
        | :? System.Windows.Forms.Form as form -> 
            printfn "Found a windows form! %A" hWnd
            form.Close()
        | x -> printfn "Returned something that wasn't a form"
    with ex -> 
        printfn "Error forming handle: %s" ex.Message

let windows = 
    lp.Threads
    |> Seq.cast<ProcessThread> 
    |> Seq.map (fun t -> PInvoke.getThreadWindows t.Id )
    |> Seq.concat
    |> Seq.map fst
    |> Seq.map (fun hWnd -> hWnd,  PInvoke.getWindowName hWnd, PInvoke.getIsVisible hWnd)
    |> List.ofSeq
printfn "Found %i windows" windows.Length

windows
|> Seq.map (fun ((hWnd, name, isVisible) as x) ->
    tryAsForm hWnd
    printfn "closingWindow result %A" (closeWindow hWnd)
    let result = (PInvoke.destroyWindow hWnd)
    //if name ="Can't Snoop" then 
    if not result then
        printfn "error destroying is %i" (Marshal.GetLastWin32Error())
        ()
    x
)
|> List.ofSeq

