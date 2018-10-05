module SqlGeneratorTests
open System
open global.Expecto
open Microsoft.VisualStudio.TextTemplating
open System.Text
open Macros.SqlMacros
open CodeGeneration.SqlMeta
open System.Diagnostics
open System.IO

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
    open CodeGeneration.SqlMeta.ColumnTyping

    let refData = [
            {ReferenceData.FKeyId = {Table={Schema="dbo";Name="GuarantorTypes"};Column="GuarantorTypeId"}; GenerateReferenceTable = false; ValuesWithComment =
                dict [
                "SELF",null
                "THIRD PARTY", null
                "Insurance & Self", null
                ]
            }]
    let pkeyIdent =[ "identity"; "primary key" ]
    let vOrnonValueStringToList s = if String.IsNullOrEmpty s then List.empty else [s]

    let makeUserIdColumn prefix nullability comment =
        let fkey = FKey.FKeyIdentifier {Table={Schema="dbo"; Name="Users"}; Column="UserID"}
        { makeIntFkey (prefix + "UserID") fkey with Nullability = nullability; Comments= vOrnonValueStringToList comment}
    let makePatientIdColumn prefix nullability comment =
        let fkey = FKey.FKeyIdentifier {Table={Schema="dbo";Name="Patients"; };Column="PatientID"}
        {makeIntFkey (prefix+"PatientID") fkey with Nullability = nullability; Comments=vOrnonValueStringToList comment}

    let toGen =
        let makeNonFKeyColumn name columnType nullability = 
            {Name=name; ColumnType = columnType; IsUnique = NotUnique; Nullability = nullability; FKey = None; Comments = List.Empty; DefaultValue = null}
        [
        {   TableGenerationInfo.Id = {Schema="dbo";Name="Payment"}
            Columns=
                [
                    {ColumnInput.Name="PaymentID"; ColumnType=ColumnType.IdentityColumn; IsUnique=NotUnique; Nullability=Nullability.NotNull; FKey=None; Comments = List.empty; DefaultValue=null}
                    {makeIntFkey "AppointmentId" (FKey.FKeyIdentifier {Table ={Schema="dbo"; Name="PaymentItemStatus"}; Column="AppointmentId"}) with Nullability = AllowNull }
//                    createFKeyedColumn typeof<int> "AppointmentId" {FKeyInfo.Schema="dbo"; Table="PaymentItemStatus"; Column="AppointmentId"} true null
                    // from line 47

                    {makeStrRefFkey50 "PaymentTypeId"
                        {   ReferenceData.FKeyId = {Table = {Schema = "Accounts"; Name = "PaymentType"}; Column = "PaymentTypeId"}
                            GenerateReferenceTable = true
                            ValuesWithComment = dict ["Patient", null; "ThirdParty", null; "Era", null]}
                        with
                            Comments = [ "|Patient of PatientIdentifier * PatientPayment |ThirdParty of PayerIdentifier * ThirdPartyPayment |Era of PayerIdentifier * EraPaymentMethod"
                        ] }
                    // from line 60
                    makeStrRefFkey50 "PaymentMethodId"
                        {   ReferenceData.FKeyId = {Table = {Schema="Accounts"; Name="PaymentType"}; Column="PaymentMethodId"}
                            GenerateReferenceTable = true
                            ValuesWithComment =
                                dict[
                                    "Cash",null;"CC",null;"Check",null;"Ach",null;"Fsa",null;"Other","for when Era amount is 0 or a catch-all"
                            ] }
                    // from line 69
                    makeStrRefFkey50 "PaymentStatusId"
                        {   ReferenceData.FKeyId = {Table ={Schema="Accounts"; Name="PaymentStatus"}; Column= "PaymentStatusId"}
                            GenerateReferenceTable = true
                            ValuesWithComment = ["New";"Partial";"Complete"] |> Seq.map (fun n -> n,null) |> dict }
                    {   Name="TotalAmount";
                        ColumnType=DecimalColumn (Some {Precision=Precision.Create(12uy).Value; Scale=Scale.Create(2uy).Value}); IsUnique=NotUnique; Nullability = NotNull; FKey=None;
                        Comments = ["was amount (18,2)"]; DefaultValue=null}
                    makeUserIdColumn null AllowNull "null to allow system inserts/adjustments that aren't done by a user"
                    {makeIntFkey "PayerID" (FKeyIdentifier {Table={Schema="dbo";Name="Payers"};Column="PayerID"}) with Nullability=AllowNull}
                    makePatientIdColumn null AllowNull null
                    // change comment after testing to 'name was timestamp'
                    { ColumnInput.Name="Created"; ColumnType=ColumnType.DateTimeColumn; Nullability=NotNull; IsUnique=NotUnique; Comments = ["was timestamp"]; FKey=None;DefaultValue=null}
//                    {Name="Created"; Type=Other typeof<DateTime>; AllowNull=AllowNull; Comments = ["was timestamp"]; GenerateReferenceTable = false; ReferenceValuesWithComment=null;FKey=None; Attributes = List.empty}
                    { makeNonFKeyColumn "TransactionNumber" (ColumnType.StringColumn 30) AllowNull with Comments = ["was checkNumber now will store check number or ACH number (when applicable)"]}
                    { makeNonFKeyColumn "Rcd" ColumnType.DateTimeColumn AllowNull with Comments = ["Payment Recvd"]}
                    makeNonFKeyColumn "IsElectronic" ColumnType.Bit NotNull
                    { makeIntFkey "CCItemID" (FKeyIdentifier {Table={Schema="dbo";Name="Accounts"};Column="CCItem"}) with Nullability=AllowNull}
                    makeNonFKeyColumn "Comments" ColumnType.StringMax AllowNull
            ]
        }
    ]


[<Tests>]
let testSqlGenerator =
    let inline generate allowDebugOutput =
            // translate SqlGenerator.tt to call into SqlMeta
            let sb = StringBuilder()
            //    let manager = MacroRunner.MultipleOutputHelper.Managers.Manager.Create(tHost, sb)
            let manager = MacroRunner.MultipleOutputHelper.Managers.Manager(Some "HelloTesting.fake.tt",sb,allowDebugOutput)
            //    let targetProjectName = "ApplicationDatabase"
            //    let targetInsertRelativePath = @"Scripts\Post-Deployment\TableInserts\Accounting1.5\AccountingInserts.sql"
            generateTablesAndReferenceTables(manager,sb, None, SqlGeneratorReferenceData.toGen |> Seq.take 1, allowDebugOutput)
            let output = sb |> string
            output

    testSequenced <| testList "generateTablesAndRefrenceTables" [
        testCase "testSqlGenerator" <|
            fun () ->
                let output = generate true
                Console.WriteLine "Hello world"
                Trace.WriteLine <| sprintf "Tracing output:%s" output
                Debug.WriteLine <| sprintf "Debug output:%s" output
                printfn "Console output:%s" output
                Debugger.Log(0,"1", output)
                let expectations =
                    [   "[Payment]"
                        "[Accounts].[PaymentType]"
                        "-- Generated file, DO NOT edit directly"
                    ]
                Expect.isTrue (output.Length > 0) "no text generated"
                Expect.all expectations (fun e -> output.Contains(e) = true) "Missing something in sql gen"
         // we want to provide an option for console output from the generator is silent, write a test that fails when it writes to output, then add the feature
        testCase "no console out" <|
            fun () ->
                let oldOut = Console.Out
                use fakeConsole = new ConsoleWrapper(oldOut,fun () -> invalidOp "it is writing!")
                Console.SetOut fakeConsole
                try
                    generate false |> ignore
                finally
                    Console.SetOut oldOut


    ]

