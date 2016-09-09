module SqlGeneratorTests
open System
open global.Xunit
open global.Xunit.Abstractions
open Microsoft.VisualStudio.TextTemplating
open System.Text
open CodeGeneration.SqlMeta
open System.Diagnostics

let tHost = 
    {
    new ITextTemplatingEngineHost with
        member __.GetHostOption(optionName: string): obj = failwithf "GetHostOption(%s):Not implemented yet" optionName
        member __.LoadIncludeText(requestFileName: string, content: byref<string>, location: byref<string>): bool = 
            failwithf "LoadIncludeText(%s, %s, %s):Not implemented yet" requestFileName (content) (location)
        member __.LogErrors(errors: System.CodeDom.Compiler.CompilerErrorCollection): unit = 
            failwithf "LogErrors(%A):Not implemented yet" errors
        member __.ProvideTemplatingAppDomain(content: string): System.AppDomain = 
            failwithf "ProvideTemplatingAppDomain(%s): Not implemented yet" content
        member __.ResolveAssemblyReference(assemblyReference: string): string = 
            failwithf "ResolveAssemblyReference(%s): Not implemented yet" assemblyReference
        member __.ResolveDirectiveProcessor(processorName: string): System.Type = 
            failwithf "ResolveDirectiveProcessor(%s): Not implemented yet" processorName
        member __.ResolveParameterValue(directiveId: string, processorName: string, parameterName: string): string = 
            failwithf "ResolveParameterValue(%s, %s, %s): Not implemented yet" directiveId processorName parameterName
        member __.ResolvePath(path: string): string = 
            failwithf "ResolvePath(%s): Not implemented yet" path
        member __.SetFileExtension(extension: string): unit = 
            failwithf "SetFileExtension(%s): Not implemented yet" extension
        member __.SetOutputEncoding(encoding: System.Text.Encoding, fromOutputDirective: bool): unit = 
            failwithf "SetOutputEncoding(%A, %A): Not implemented yet" encoding fromOutputDirective
        member __.StandardAssemblyReferences: System.Collections.Generic.IList<string> = 
            failwith "StandardAssemblyReferences: Not implemented yet"
        member __.StandardImports: System.Collections.Generic.IList<string> = 
            failwith "StandardImports: Not implemented yet"
        member __.TemplateFile: string = "HelloTesting.fake.tt"
    }

let projects = [
        "PracticeManagement.Foundation"
        "PracticeManagement.UI.Localization"
        "PracticeManagement.Reports"
        "PracticeManagement"
        "XpteReports"
        "Pm.Dal"
        "Pm.Schema"
        "XpteReportLibrary"
        "Pm.Domain"
        "Pm.UI"
        "Pm.Tests"
        "ApplicationDatabase"
        "CodedUITestProject1"
        "Miscellaneous Files"
    ]
module SqlGeneratorReferenceData = 
    let refData= [
            {ReferenceData.Schema="dbo";Table="GuarantorTypes";Column="GuarantorTypeId"; ValuesWithComments = 
                dict [
                "SELF",null
                "THIRD PARTY", null
                "Insurance & Self", null
                ]
            }]
    let pkeyIdent =[ "identity"; "primary key" ] 
    let vOrnonValueStringToList s = if String.IsNullOrEmpty s then List.empty else [s]

    let makeStrFkey50 name fkey = {Name=name; Type=VarChar (Length 50); Attributes = List.empty; AllowNull = NotNull; FKey = Some fkey; Comments = List.empty; GenerateReferenceTable=false; ReferenceValuesWithComment=null}
    let makeIntFkey name fkey = {Name=name; Type=Other typeof<int>; Attributes = List.empty; AllowNull = NotNull; FKey=Some fkey; Comments = List.empty; GenerateReferenceTable=false; ReferenceValuesWithComment=null}
    let makeUserIdColumn prefix allowNull comment = 
        { makeIntFkey (prefix + "UserID") {FKeyInfo.Schema="dbo"; Table="Users"; Column="UserID"} with AllowNull = allowNull; Comments= vOrnonValueStringToList comment}
    let makePatientIdColumn prefix allowNull comment = 
        {makeIntFkey (prefix+"PatientID") {FKeyInfo.Schema="dbo";Table="Patients"; Column="PatientID"} with AllowNull = allowNull; Comments=vOrnonValueStringToList comment}
    let makeNullable50 name = 
        {Name=name; Type = VarChar (Length 50); AllowNull = AllowNull; Attributes = List.empty; FKey = None; Comments = List.empty; GenerateReferenceTable = false; ReferenceValuesWithComment = null}
    let makeNonFKeyColumn name columnType allowNull = 
        {Name=name; Type=columnType; AllowNull=allowNull; Comments = List.empty; GenerateReferenceTable = false; ReferenceValuesWithComment=null;FKey=None; Attributes = List.empty}

    let toGen = [
        {Schema="dbo";Name="Payment"; 
            Columns=
                [
                    {Name="PaymentID"; Type=Other typeof<int>; Attributes = pkeyIdent; AllowNull=NotNull; FKey=None ; Comments = List.empty;GenerateReferenceTable=false; ReferenceValuesWithComment=null}
                    {makeIntFkey "AppointmentId" {FKeyInfo.Schema="dbo"; Table="PaymentItemStatus"; Column="AppointmentId"} with AllowNull = AllowNull }
//                    createFKeyedColumn typeof<int> "AppointmentId" {FKeyInfo.Schema="dbo"; Table="PaymentItemStatus"; Column="AppointmentId"} true null
                    // from line 47
                    {makeStrFkey50 "PaymentTypeId" {Schema="Accounts";Table="PaymentType";Column="PaymentTypeId"} with 
                        GenerateReferenceTable = true
                        ReferenceValuesWithComment = [ "Patient";"ThirdParty";"Era"] |> Seq.map (fun n -> n,null) |> dict
                        Comments = [
                                    "|Patient of PatientIdentifier * PatientPayment |ThirdParty of PayerIdentifier * ThirdPartyPayment |Era of PayerIdentifier * EraPaymentMethod"
                        ] }
                    // from line 60
                    {makeStrFkey50 "PaymentMethodId" {Schema="Accounts"; Table="PaymentType"; Column="PaymentMethodId"} with
                        GenerateReferenceTable = true
                        ReferenceValuesWithComment = 
                            dict[
                                "Cash",null;"CC",null;"Check",null;"Ach",null;"Fsa",null;"Other","for when Era amount is 0 or a catch-all"
                        ] }
                    // from line 69
                    {makeStrFkey50 "PaymentStatusId" {Schema="Accounts"; Table="PaymentStatus"; Column= "PaymentStatusId"} with
                        GenerateReferenceTable = true
                        ReferenceValuesWithComment = ["New";"Partial";"Complete"] |> Seq.map (fun n -> n,null) |> dict }
                    {   Name="TotalAmount"; 
                        Type=Decimal (Some {Precision=12; Scale=2});Attributes = List.empty; AllowNull = NotNull; FKey=None; 
                        GenerateReferenceTable=false
                        ReferenceValuesWithComment=null
                        Comments = ["was amount (18,2)"] }
                    makeUserIdColumn null AllowNull "null to allow system inserts/adjustments that aren't done by a user"
                    {makeIntFkey "PayerID" {Schema="dbo";Table="Payers";Column="PayerID"} with AllowNull=AllowNull}
                    makePatientIdColumn null AllowNull null
                    // change comment after testing to 'name was timestamp'
                    { makeNonFKeyColumn "Created" (Other typeof<DateTime>) AllowNull with Comments = ["was timestamp"]}
//                    {Name="Created"; Type=Other typeof<DateTime>; AllowNull=AllowNull; Comments = ["was timestamp"]; GenerateReferenceTable = false; ReferenceValuesWithComment=null;FKey=None; Attributes = List.empty}
                    { makeNonFKeyColumn "TransactionNumber" (VarChar (Length 30)) AllowNull with Comments = ["was checkNumber now will store check number or ACH number (when applicable)"]}
                    { makeNonFKeyColumn "Rcd" (Other typeof<DateTime>) AllowNull with Comments = ["Payment Recvd"]}
                    makeNonFKeyColumn "IsElectronic" (Other typeof<bool>) NotNull
                    { makeIntFkey "CCItemID" {Schema="dbo";Table="Accounts";Column="CCItem"} with AllowNull=AllowNull}
                    makeNonFKeyColumn "Comments" (VarChar Max) AllowNull
            ]
        }
    ]

[<Fact>]
let testSqlGenerator () = 

    // translate SqlGenerator.tt to call into SqlMeta
    let sb = StringBuilder()
//    let manager = MacroRunner.MultipleOutputHelper.Managers.Manager.Create(tHost, sb)
    let manager = MacroRunner.MultipleOutputHelper.Managers.Manager(tHost,sb)
//    let targetProjectName = "ApplicationDatabase"
//    let targetInsertRelativePath = @"Scripts\Post-Deployment\TableInserts\Accounting1.5\AccountingInserts.sql"
    generateTablesAndReferenceTables(manager,sb, None, SqlGeneratorReferenceData.toGen |> Seq.take 1)
    let output = sb |> string
    Console.WriteLine "Hello world"
    Trace.WriteLine output
    Debug.WriteLine output
    Debugger.Log(0,"1", output)
    output.Length > 0
    |> Assert.True
    output
    |> (=) (System.IO.File.ReadAllText(@"C:\TFS\PracticeManagement\dev\PracticeManagement\Db\Schema Objects\Schemas\dbo\Tables\Payment.table.sql"))
    |> Assert.True

