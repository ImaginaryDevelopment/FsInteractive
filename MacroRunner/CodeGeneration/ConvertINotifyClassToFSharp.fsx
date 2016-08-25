// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
#I __SOURCE_DIRECTORY__
open System
open System.IO
printfn "SourceDir is %s" __SOURCE_DIRECTORY__
printfn "CurrentDir is %s" Environment.CurrentDirectory
#r "./bin/Debug/MacroRunner.exe"
#r "./bin/Debug/CodeGeneration.dll"
#r "System.Collections.Immutable"
#r "../packages/Microsoft.CodeAnalysis.Common.2.0.0-beta3/lib/net45/Microsoft.CodeAnalysis.dll"
//#r "../packages/Microsoft.CodeAnalysis.CSharp.2.0.0-beta3/lib/net45/Microsoft.CodeAnalysis.CSharp.dll"
//#r "System.Threading.Tasks"
//#r "System.Text.Encoding"
open CodeGeneration

// Define your library scripting code here

module ScriptOptions = 
    open CodeGeneration.PureCodeGeneration

    let searchLinqPadQueriesForSample() =
        let myDocs = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
        Path.Combine(myDocs,"LINQPad Queries","Roslyn")

    #if INTERACTIVE

    let srcPath = 
        let workTarget = @"C:\TFS\PracticeManagement\dev\PracticeManagement\PracticeManagement.Foundation\DataModels\PcpDataModel.cs"
        if File.Exists(workTarget) then 
            workTarget (*  Environment.ExpandEnvironmentVariables("%devroot%"); *) 
        else
            let envTarget = Environment.ExpandEnvironmentVariables("%devroot%")
            if Directory.Exists envTarget then 
                envTarget
            else
            searchLinqPadQueriesForSample()
        |> PathF
        |> CodeSource.File
        |> CodeSources.CodeSource
    #else
    let srcPath = 
        //searchLinqPadQueriesForSample()
        let target = @"C:\TFS\PracticeManagement\dev\PracticeManagement\XpteReportLibrary\ReportInputsViewModel.cs"
        match Directory.Exists target, File.Exists target with
        | false,false -> failwith "Invalid target directory or file"
        | _, true -> CodeSources.CodeSource(CodeSource.File (PathF target))
        | true, _ -> CodeSources.Directory(PathF target,Some "*datamodels.cs")
        
    #endif

    printfn "searching path %A" srcPath
    let getDefaultRunOptions () = 
        {   Target = CodeGenTarget.Class (PropertyOptions.KeepNotify, PromoteUnitializedStructsToNullables.Flag(true))
            TypeAttributes = ["AllowNullLiteral"]
            Source = srcPath (*  Environment.ExpandEnvironmentVariables("%devroot%"); *) }
//    let promoteUninitializedStructsToNullable = true
//    let includeOriginalInComments = true
//    let includeMatchTypeInComments = true
//    let selfIdentifier = "x"
//    let spacing = Enumerable.Repeat(' ',4) |> Array.ofSeq |> String
//
//    let isDebugFieldPred name = if name ="_AppointmentEndTime" then Promote else Demote
//    let isDebugPropPred name = if name = "LOS" then Promote else Demote
    let isDebugCode code = match code with | Code(_p,_s) -> Abstain 
//    let isDebugNode (_node: #SyntaxNode) = Abstain

open CodeGeneration.PureCodeGeneration
open BReusable

type System.String with
    static member optionToStringOrEmpty (s:string option) = match s with Some text -> text | None -> String.Empty
let convertFile translateOptions typeAttrs target (cls:FileInfoB) = 
    let _debugOpt,getDebugOpt = translateOptions.GetStartDebugState None (ScriptOptions.isDebugCode cls.File)
    let convertProperties () = PropConversion.convertProperties (ScriptOptions.isDebugCode) translateOptions target translateOptions.Spacing cls
    
    match target with 
    //| RecordBasedClass (propertyPrefs,promote)  -> String.Empty
    | Record -> 
        let props = convertProperties()
        sprintf "type %sRecord={%s}" cls.ClassName (delimit ";" props)
    | CodeGenTarget.Interface ->
        let props = convertProperties()
        sprintf "type I%s =\r\n%s" cls.ClassName (props |> Seq.map (sprintf "abstract %s") |> delimit ";")
    | CodeGenTarget.Class (propertyPrefs,promote) ->
        let spacing = translateOptions.Spacing
        let classD = {
            ClassAttributes = typeAttrs
            Name=cls.ClassName
            BaseClass= 
                match propertyPrefs with 
                | PropertyOptions.InheritFsharpViewModule -> Some "  inherit FSharp.ViewModule.ViewModelBase()" 
                |_ -> if cls.Bases |> Seq.any then cls.Bases |> Seq.head |> sprintf "inherit %s()" |> Some else None
            Fields=  List.empty
            Members = List.empty
            Interfaces = List.empty
            }
        let classD = 
            cls.Bases |> dumpt "bases" |> ignore
            let buildInterface() : ClassMember list = 
                [
                    ClassMember.Interface <| sprintf " interface %s\r\n" "System.ComponentModel.INotifyPropertyChanged"
                    ClassMember.Method "RaisePropertyChanged"
                ]
            match propertyPrefs with 
            | PropertyOptions.KeepNotify -> 
                {classD with Members = buildInterface() @ classD.Members} 
            |_ -> classD

        let classD ={classD with Fields = FieldConversion.convertFileFields translateOptions promote spacing  cls getDebugOpt}
        let props = convertProperties() |> Seq.map (indent spacing)
        let filename = match cls.File with 
                        |Code (Some path,_) -> match path with |PathF p -> p
                        | _ -> "unknown"
        let text = sprintf "%s\r\ntype %s() = // translated from %s\r\n%s\r\n\r\n" (classD.AttributeText()) cls.ClassName filename (translateOptions.Spacing + String.optionToStringOrEmpty classD.BaseClass)
        let text = new System.Text.StringBuilder(text)
        text
            .AppendLine(classD.FieldText spacing)
            .AppendLine(String.Empty)
            .AppendLine(delimit "\r\n" props).ToString()

let ``convertToF#`` translateOptions (runOptions:ConversionRunOptions option) limit =
    let runOptions = match runOptions with |Some ro -> ro | None -> ScriptOptions.getDefaultRunOptions()
    printfn "runOptions.Source = %A" runOptions.Source
    runOptions.Source |> dumpt "runOptions.Source" |> ignore
    let lines = ref 0
    let classes = ref 0
    let converted ()= 
        seq{
        let items = getFileInfoFrom runOptions.Source
        let items = if Option.isSome limit then items |> Seq.take limit.Value else items
        //printfn "converting items %i" (Seq.length items)
        
        for cls in items do
            //printfn "Starting conversion %s" cls.Class'
            let text = convertFile translateOptions runOptions.TypeAttributes runOptions.Target cls
            let split = text.Split([| "\r\n" |], StringSplitOptions.RemoveEmptyEntries)
            lines := !lines +  (split |> Seq.length)
            classes:= !classes + 1
            yield text
        } |> Array.ofSeq
    printfn "converted %i classes, %i lines" !classes !lines
    converted()

let convertUsingDefaults translateOptions fileInfos target= 
    let options = ScriptOptions.getDefaultRunOptions()
    let target = findModel target fileInfos
    let target' = target |> Option.map(convertFile translateOptions options.TypeAttributes options.Target)
    target'

#if INTERACTIVE
#r "System.Windows.Forms"
#endif
module ScriptSample =
    open CodeGeneration.PureCodeGeneration
    open BReusable

    let pdm' fileInfos = CodeGeneration.PoorlyNamed.convertUsingDefaults ScriptOptions.isDebugCode (TranslateOptions.getDefault()) fileInfos "PatientDataModel" 
    let apm' fileInfos = CodeGeneration.PoorlyNamed.convertUsingDefaults ScriptOptions.isDebugCode (TranslateOptions.getDefault()) fileInfos "AppointmentDataModel"
    let getClip () = System.Windows.Forms.Clipboard.GetText()
    let setClip text = if not <| isNull text then System.Windows.Forms.Clipboard.SetText text

    let getClassToClip f= 
        let sources = ScriptOptions.getDefaultRunOptions()
        f(sources.Source) |> Option.iter setClip

    let pdm () = 
        printfn "starting on patient data model"
        getClassToClip (fun s -> pdm' (getFileInfoFrom s))

    let apm () = getClassToClip (fun s -> apm' (getFileInfoFrom s))

    let chargeDataModelToInterface() = 
        let options = {
            ConversionRunOptions.Source= PathF @"C:\TFS\Pm-Rewrite\Source-dev-rewrite\PracticeManagement\Pm.ViewModelsC\DataModels\ChargeDataModel.cs" |> CodeSource.File |> CodeSources.CodeSource 
            Target = CodeGenTarget.Interface
            ConversionRunOptions.TypeAttributes = List.empty
        }
        let fileInfo = getFileInfoFrom options.Source |> Seq.head
        convertFile ScriptOptions.isDebugCode (TranslateOptions.getDefault()) options.TypeAttributes options.Target fileInfo
    let clipAll runOptions= 

        let converted = ``convertToF#`` ScriptOptions.isDebugCode (TranslateOptions.getDefault()) runOptions None 
        if Seq.isEmpty converted then
            printfn "nothing converted"
        else
            converted |> delimit "\r\n\r\n" |> setClip
    clipAll None
