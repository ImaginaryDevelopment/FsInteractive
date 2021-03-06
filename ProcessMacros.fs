namespace Macros
#if INTERACTIVE
#load "BReusable.fs"
#endif
open System
open BReusable
module ProcessMacros =
    open System.Collections.ObjectModel
    open System.IO
    open System.Diagnostics

    [<NoComparison>]
    type RunProcResult = {Outputs:string ObservableCollection; Errors: string ObservableCollection; }

    let displayText lines titling =
        let tp,tf =
            Path.GetTempFileName()
            |> fun tf -> Path.GetDirectoryName tf, Path.GetFileName tf
        let targetFile = Path.Combine(tp, titling + "."+  tf + ".txt")
        File.WriteAllLines(targetFile, lines)
        let p = Process.Start(targetFile)
        printfn "Started textfile %s with id %i @ %s" titling p.Id targetFile

    // consider switching this to use F#'s event Observables
    let private setupRunProc filename args startDir fBothOpt outF errorF =
        let timer = System.Diagnostics.Stopwatch.StartNew()
        let procStartInfo =
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = filename,
                Arguments = args
            )
        match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()
        let outputHandler isError f (_sender:obj) (args:DataReceivedEventArgs) =
            let result = f args.Data
            match fBothOpt with
            | Some fBoth -> fBoth isError args.Data
            | None -> ()

            result

        let p = new Process(StartInfo = procStartInfo)
        let filterS f s = if not (String.IsNullOrEmpty s) then f s
//        p.OutputDataReceived
//        |> Observable.map(fun d-> d.Data)
//        |> Observable.add (outputHandler false >> filterS outF)

        p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler false (filterS outF)))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler true (filterS errorF)))

        let started =
            try
                p.Start()
            with ex ->
                ex.Data.Add("filename", filename)
                reraise()
        if not started then
            failwithf "Failed to start process %s" filename
        printfn "Started %s with pid %i" p.ProcessName p.Id
#if LINQPAD
        let setupKillLinq () = // TODO: try to get the liveKillLink:observable<HyperLinq> deal working
            let killIt () =
                p.Id.Dump("killing process")
                p.Kill()
            let h = Hyperlinq(Action(killIt),sprintf "Kill Process:%i" p.Id, runOnNewThread = true)
            h.Dump()
#endif

        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
#if LINQPAD
        setupKillLinq()
#endif

        let onFinish =
            async{
                try
                    p.WaitForExit()
                    timer.Stop()
                    printfn "Finished %s after %A milliseconds" filename timer.ElapsedMilliseconds
                    return Happy(p,timer)
                with ex ->
                    return Unhappy ex
            }
        onFinish

    let runProcAsync filename args startDir fBothOpt fOutput fError=
        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let tree f1 s =
            f1 s
            s
        let onFinish = setupRunProc filename args startDir fBothOpt (tree fOutput>>outputs.Add) (tree fError >> errors.Add)
        let resultTask =
            async {
                let! result = onFinish
                match result with
                | Happy t ->
                    let p,timer = t
                    return Happy(outputs,errors)
                | Unhappy ex ->
                    return Unhappy ex
            }
        resultTask

    let runProcPrint filename args startDir outputIsErrorF =
        let errorHandler errS=
            let color = System.Console.ForegroundColor
            Console.ForegroundColor <- ConsoleColor.Red
            Console.WriteLine (sprintf "err:%s" errS)
            Console.ForegroundColor <- color

        let outputHandler s =
            match outputIsErrorF with
            |Some f when f s -> errorHandler s
            | _ -> printfn "%s" s

        let resultTask =
            runProcAsync filename args startDir None outputHandler errorHandler

        let result = Async.RunSynchronously resultTask
        result

    let runProcSync filename args startDir fBothOpt fOutputOpt fErrorOpt =
        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let fOutput txt =
            outputs.Add txt
            match fOutputOpt with
            |Some f ->
                f txt
            | None ->
                ()
        let fError txt =
            errors.Add txt
            match fErrorOpt with
            |Some f ->
                f txt
            | None -> ()

        let r = Async.RunSynchronously (setupRunProc filename args startDir fBothOpt fOutput fError)
        outputs,errors
    // translation from LinqPad
    module LP =
        open System.ComponentModel
        open System.Text

        let getCmdProcess (cmdText:string) args useCmdExec =
            let cmdText = cmdText.Trim()
            let processStartInfo = ProcessStartInfo(RedirectStandardOutput=true, RedirectStandardError=true, RedirectStandardInput=true, UseShellExecute=false, CreateNoWindow=true)
            if not useCmdExec then
                processStartInfo.FileName <- cmdText
                processStartInfo.Arguments <- args
                if not <| String.IsNullOrEmpty args || (if cmdText.Contains(" ") then false else not <| File.Exists cmdText) then
                    let strArrays = cmdText.Split(" ".ToCharArray(), 2, StringSplitOptions.RemoveEmptyEntries)
                    if File.Exists strArrays.[0] then
                        processStartInfo.FileName <- strArrays.[0]
                        processStartInfo.Arguments <- strArrays.[1]
                try
                    if Path.IsPathRooted processStartInfo.FileName then
                        let directoryName = Path.GetDirectoryName processStartInfo.FileName
                        if Directory.Exists directoryName then
                            processStartInfo.WorkingDirectory <- directoryName
                with | :? ArgumentException -> ()
            else
                processStartInfo.FileName <- "cmd.exe"
                let str = if cmdText.Contains " " then String.Concat("\"", cmdText, "\"") else cmdText
                processStartInfo.Arguments <- String.Concat("/c ", str)
                if not <| String.IsNullOrEmpty args then
                    processStartInfo.Arguments <- String.Concat(processStartInfo.Arguments, " ", args)
            Process.Start processStartInfo

        let Cmd commandText args quiet =
            use cmdProcess =
                try
                    getCmdProcess commandText args false
                with | :? Win32Exception ->
                    getCmdProcess commandText args true
            let strs = ResizeArray()
            try
                let stringBuilder = new StringBuilder()
                let appendLine = stringBuilder.AppendLine >> ignore<StringBuilder>
                cmdProcess.ErrorDataReceived.AddHandler (DataReceivedEventHandler(fun _sender errorArgs -> appendLine errorArgs.Data))
                cmdProcess.BeginErrorReadLine()
                let mutable lastLine = String.Empty
                while not <| isNull lastLine do
                    lastLine <- cmdProcess.StandardOutput.ReadLine()
                    match lastLine with
                    | null -> ()
                    | txt ->
                        if not quiet then
                            printfn "%s" txt
                        strs.Add txt
                    ()
                let exitCode = cmdProcess.ExitCode
                if exitCode > 0 then
                    invalidOp (sprintf "exitCode:%i, %s" exitCode (stringBuilder.ToString()))
            finally
                // try kill process if still running
                try
                    cmdProcess.Kill()
                with _ -> ()
            strs.ToArray()


module MsBuild =
    open System.IO
    open BReusable.StringHelpers

    let msbuild targetProject buildArgs fBothOpt fOutputOpt fErrorOpt =
        let targetFolder = Path.GetDirectoryName targetProject
        let msbuildPath = @"C:\Program Files (x86)\MSBuild\14.0\Bin\MSBuild.exe"
        let errorCount outputs errors =
            let regex = System.Text.RegularExpressions.Regex(@"^\s*([1-9][0-9]*)\s+Error\(s\)$|Build FAILED.")
            [ outputs;errors] |> Seq.concat  |> Seq.map regex.Match |> Seq.tryFind(fun m -> m.Success)

        let args = targetProject::buildArgs |> delimit " "

        //liveMessageStream.OnNext args.Data
        //liveMessageStream.OnCompleted()

        let output,errors = ProcessMacros.runProcSync msbuildPath args (Some targetFolder) fBothOpt fOutputOpt fErrorOpt

        match errorCount output errors with
        | Some errorMatch ->
            let regex = System.Text.RegularExpressions.Regex("Build error", Text.RegularExpressions.RegexOptions.IgnoreCase)

            printfn "%A" (output |> Seq.filter regex.IsMatch |> List.ofSeq)
            let errorText =
                let text = errorMatch.Groups.[1].Value
                if String.IsNullOrWhiteSpace(text) then errorMatch.Groups.[0].Value else text
            failwithf "ErrorsFound : %s" errorText
        | None -> ()
        if output |> Seq.contains ("Build FAILED.") then failwithf "Build failed"
        output,errors


    let buildSqlProj loggerPath targetFolder =
        let sqlProjFile = Directory.GetFiles(targetFolder, "*.sqlproj") |> Seq.head
        let logger =
            match loggerPath with
            | Some lp -> sprintf "/logger:XMLLogger,%s" lp
            | None -> ""
        let args =
            logger::["/target:Build"]
            |> List.filter (String.IsNullOrWhiteSpace >> not)
        printfn "%A" args
        let outputs,errors = msbuild sqlProjFile args None None None
        let outputs = Array.ofSeq outputs
        ProcessMacros.displayText outputs "MsBuild"
        //printfn "%A" outputs
        //printfn "error length: %i" (Seq.length errors)
        if Seq.exists (fun _ -> true) errors then
            printfn "errors %A" (Array.ofSeq errors)
            failwithf "Build failed %A" errors
        let last = outputs |> Seq.last
        targetFolder,last
