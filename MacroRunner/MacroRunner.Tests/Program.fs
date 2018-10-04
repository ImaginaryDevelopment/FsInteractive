module Program

open Expecto
open System

[<EntryPoint>]
let main argv =
    // force load of code gen/MacroRunner
    //let _t = typeof<MacroRunner.AdoHelper.Connector>
    //let _t = typeof<CodeGeneration.DataModelToF.CodeGenSettingMap>
    let config = defaultConfig.appendSummaryHandler(fun trs ->
        if trs.successful then
            trs.passed
            |> List.iter(fun (ft,summary) ->
                match summary.result with
                | Impl.TestResult.Passed ->
                    printfn "%s:%i" ft.name summary.count
                | x -> eprintfn "Fail:%A" x
            )
    )
    let result = Tests.runTestsInAssembly config argv
    Console.ReadLine() |> ignore
    result