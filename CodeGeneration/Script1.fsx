//#if INTERACTIVE
//#r "System.Core"
//#r "FSharp.Core"
//#r "EnvDTE"
//#r "EnvDTE80"
//#I @"C:\Program Files (x86)\Microsoft Visual Studio 14.0\VSSDK\VisualStudioIntegration\Common\Assemblies\v4.0"
//#r "Microsoft.VisualStudio.TextTemplating.Interfaces.10.0.dll"
//#I @"C:\projects\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\"
//#r @"bin\debug\CodeGeneration.dll"
//#r "MacroRunner.exe"
////#r "Microsoft.VisualStudio.TextTemplating.Interfaces.10.0.dll"
//#endif

#if INTERACTIVE
#r "System.Core"
#r "System.Data.Entity.Design"
#r "FSharp.Core"
#r "EnvDTE"
#r "EnvDTE80"
//#I @"C:\Program Files (x86)\Microsoft Visual Studio 14.0\VSSDK\VisualStudioIntegration\Common\Assemblies\v4.0"
#I @"C:\projects\FsInteractive\MacroRunner\CodeGeneration\bin\Debug\"
#r "Microsoft.VisualStudio.TextTemplating.Interfaces.10.0.dll"
#r @"CodeGeneration.dll"
#r @"MacroRunner.exe"
#endif
// write the .tt that calls the F# and generates into EnvDTE?
open System
open System.Collections.Generic
open System.Diagnostics
open Microsoft.VisualStudio.TextTemplating
open CodeGeneration
open CodeGeneration.SqlMeta
open MacroRunner
open MacroRunner.DteWrap
open MacroRunner.MultipleOutputHelper.Managers
open Macros.SqlMacros

let failing s= 
    if Debugger.IsAttached then
        Debugger.Log(1,"fail", s)
        Debugger.Break()
    failwith s
let dte =
    Macros.VsMacros.getWindowNames()
    |> Seq.find(fun wn -> wn.Contains("PracticeManagement"))
    |> Macros.VsMacros.getDteByWindowName
let manager =
    let tHost =
        {
        new ITextTemplatingEngineHost with
            member __.GetHostOption(optionName: string): obj = failing "GetHostOption(%s):Not implemented yet" optionName
            member __.LoadIncludeText(requestFileName: string, content: byref<string>, location: byref<string>): bool = 
                failing "LoadIncludeText(%s, %s, %s):Not implemented yet" requestFileName (content) (location)
            member __.LogErrors(errors: System.CodeDom.Compiler.CompilerErrorCollection): unit = 
                failing "LogErrors(%A):Not implemented yet" errors
            member __.ProvideTemplatingAppDomain(content: string): System.AppDomain = 
                failing "ProvideTemplatingAppDomain(%s): Not implemented yet" content
            member __.ResolveAssemblyReference(assemblyReference: string): string = 
                failing "ResolveAssemblyReference(%s): Not implemented yet" assemblyReference
            member __.ResolveDirectiveProcessor(processorName: string): System.Type = 
                failing "ResolveDirectiveProcessor(%s): Not implemented yet" processorName
            member __.ResolveParameterValue(directiveId: string, processorName: string, parameterName: string): string = 
                failing "ResolveParameterValue(%s, %s, %s): Not implemented yet" directiveId processorName parameterName
            member __.ResolvePath(path: string): string = 
                failing "ResolvePath(%s): Not implemented yet" path
            member __.SetFileExtension(extension: string): unit = 
                failing "SetFileExtension(%s): Not implemented yet" extension
            member __.SetOutputEncoding(encoding: System.Text.Encoding, fromOutputDirective: bool): unit = 
                failing "SetOutputEncoding(%A, %A): Not implemented yet" encoding fromOutputDirective
            member __.StandardAssemblyReferences: System.Collections.Generic.IList<string> = 
                failwith "StandardAssemblyReferences: Not implemented yet"
            member __.StandardImports: System.Collections.Generic.IList<string> = 
                failwith "StandardImports: Not implemented yet"
            member __.TemplateFile: string = "HelloTesting.fake.tt"
        }
    // if this script is in the solution it is modifying, we need the EnvDTE.ProjectItem representing it, otherwise where does the main (non sub-file) output go?
    let scriptFullPath = Path.Combine(__SOURCE_DIRECTORY__,__SOURCE_FILE__)
    let templateProjectItem:EnvDTE.ProjectItem option = dte.Solution.FindProjectItem scriptFullPath |> Option.ofObj
    printfn "Script is at %s" scriptFullPath
    templateProjectItem
    |> Option.iter (fun templateProjectItem  ->
        printfn "ProjectItem= %A" (templateProjectItem.FileNames(0s))
    )
    let dteWrapper = DteWrap.wrapDte dte
    let sb = System.Text.StringBuilder()
    MultipleOutputHelper.Managers.VsManager(Some scriptFullPath, dteWrapper, sb, templateProjectItem)


    //System.Runtime.InteropServices.Marshal.GetActiveObject("VisualStudio.DTE") :?> EnvDTE.DTE
let activeDocumentFullName = dte.ActiveDocument.FullName
printfn "activeDocument is %s" activeDocumentFullName
printfn "Got dte for solution %s" dte.Solution.FileName
let doMultiFile = true
let targetSqlProjectName = "ApplicationDatabase"
let targetCodeProjectName = "Pm.Schema"
let targetInsertRelativePath = @"Scripts\Post-Deployment\TableInserts\Accounting1.5\AccountingInserts.sql"
let refData : ReferenceData list = [
            //type ReferenceData = {FKeyId:FKeyIdentifier; GenerateReferenceTable:bool; ValuesWithComment: IDictionary<string,string>}
            {   ReferenceData.FKeyId={Table={Schema="dbo";Name="GuarantorTypes"};Column="GuarantorTypeId"}
                GenerateReferenceTable = false
                ValuesWithComment= dict[
                                        "SELF",null
                                        "THIRD PARTY", null
                                        "Insurance & Self", null ]
            }
]

//type ColumnInput = {
//        Name:string
//        Type:Type
//        Length: int option
//        Precision:int option
//        Scale: int option
//        UseMax: bool
//        AllowNull: Nullability
//        Attributes:string list
//        FKey:FKeyInfo option
//        Comments: string list
//        GenerateReferenceTable: bool
//        ReferenceValuesWithComment: IDictionary<string,string>
//        IsUnique: bool
//    } with 
//        static member create name columnType = 
//            {Name=name; Type=columnType; Length= None; Precision=None;Scale=None;UseMax=false; AllowNull=Nullability.NotNull; Attributes=list.Empty; FKey= None; Comments = List.empty; GenerateReferenceTable=false; ReferenceValuesWithComment = null; IsUnique=false}
//        static member createFKeyedColumn<'T> name fkeyInfo = 
//            ColumnInput.create name typeof<'T>
//            |> fun x -> {x with FKey = Some fkeyInfo }
//        static member createFKeyedNColumn<'T> name fkeyInfo =
//            ColumnInput.createFKeyedColumn name fkeyInfo
//            |> fun x -> { x with AllowNull = Nullability.AllowNull}
//        static member createPatientIdColumn prefix allowNull comments = 
//            ColumnInput.createFKeyedColumn<int> (prefix + "PatientID") {Schema="dbo"; Table="Patients"; Column ="PatientID"}
//            |> fun x -> {x with Comments = comments; AllowNull = allowNull}
//        static member createUserIdColumn prefix allowNull comment = 
//            ColumnInput.createFKeyedColumn<int> (prefix + "UserID") {Schema="dbo"; Table="Users";Column="UserID" }
//            |> fun x -> {x with Comments = comment; AllowNull= allowNull}
//        static member makeNullable50 = ()

type TableInput() =
     member val Name:string = null with get,set
     member val Schema:string = null with get,set
     member val Columns:ColumnInput seq = Unchecked.defaultof<_> with get,set

// StringBuilder ge = GenerationEnvironment;
let sb = System.Text.StringBuilder()
let appendLine text (sb:System.Text.StringBuilder) =
    sb.AppendLine text

sb
|> appendLine "Main File Output"
|> appendLine (sprintf "Started at %O" DateTime.Now)
|> ignore


type ColumnInfo = Macros.SqlMacros.ColumnInfo
let runGeneration (toGen:TableInput list) =
    let projects = snd <| Macros.VsMacros.getSP dte // RecurseSolutionProjects(Dte)
    sb 
    |> appendLine ("Projects:")
    |> ignore

    projects
    |> Seq.iter (fun proj -> sb.AppendLine (sprintf "    %s" proj.Name) |> ignore)

    let targetSqlProject =
        projects
        |> Seq.tryFind (fun p -> p.Name = targetSqlProjectName) 
        |> function 
            | Some p -> p 
            | None -> failwithf "did not find project, names were %A" (projects |> Seq.map (fun p -> p.Name) |> List.ofSeq)

    let targetSqlProjectFolder = Path.GetDirectoryName targetSqlProject.FullName
    printfn "Going to generate into project %s via folder %s" targetSqlProject.Name targetSqlProjectFolder
    
    let genMapped : TableInfo list =
        //let toColumnType t l p s u = SqlMeta.toColumnType t (Option.toNullable l) (Option.toNullable p) (Option.toNullable s) u
        toGen
        |> List.map(fun g -> 
            GenerateAllTheThings.TableInput(Schema=g.Schema, Name=g.Name, Columns= g.Columns)
        )
    printfn "%i tables to generate" genMapped.Length

    let codeGenAsm= typeof<CodeGeneration.SqlScriptGeneration.SqlObj>.Assembly
    let info = BReusable.Assemblies.getAssemblyFullPath(codeGenAsm)
    let fileInfo = new System.IO.FileInfo(info)
    sb |> appendLine (sprintf "Using CodeGeneration.dll from %O" fileInfo.LastWriteTime) |> ignore
    SqlMeta.generateTablesAndReferenceTables(manager, sb, Some targetSqlProjectFolder, genMapped)
    // TODO: convert SqlGeneration.ttinclude -> GenerateAccountingInserts
    
    manager.Process(doMultiFile)
