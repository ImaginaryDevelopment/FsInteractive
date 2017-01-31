module SqlGeneratorTests
open System
open global.Xunit
open global.Xunit.Abstractions
open Microsoft.VisualStudio.TextTemplating
open System.Text
open Macros.SqlMacros
open CodeGeneration.SqlMeta
open System.Diagnostics

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
            {ReferenceData.FKeyId = {Table={Schema="dbo";Name="GuarantorTypes"};Column="GuarantorTypeId"}; GenerateReferenceTable = false; ValuesWithComment = 
                dict [
                "SELF",null
                "THIRD PARTY", null
                "Insurance & Self", null
                ]
            }]
    let pkeyIdent =[ "identity"; "primary key" ] 
    let vOrnonValueStringToList s = if String.IsNullOrEmpty s then List.empty else [s]

    let makeUserIdColumn prefix allowNull comment = 
        let fkey = FKey.FKeyIdentifier {Table={Schema="dbo"; Name="Users"}; Column="UserID"}
        { makeIntFkey (prefix + "UserID") fkey with AllowNull = allowNull; Comments= vOrnonValueStringToList comment}
    let makePatientIdColumn prefix allowNull comment = 
        let fkey = FKey.FKeyIdentifier {Table={Schema="dbo";Name="Patients"; };Column="PatientID"}
        {makeIntFkey (prefix+"PatientID") fkey with AllowNull = allowNull; Comments=vOrnonValueStringToList comment}

    let toGen = [
        {   TableGenerationInfo.Id = {Schema="dbo";Name="Payment"}
            Columns=
                [
                    {ColumnGenerationInfo.Name="PaymentID"; Type=ColumnType.Other typeof<int>; Attributes = pkeyIdent; IsUnique=false; AllowNull=Nullability.NotNull; FKey=None; Comments = List.empty}
                    {makeIntFkey "AppointmentId" (FKey.FKeyIdentifier {Table ={Schema="dbo"; Name="PaymentItemStatus"}; Column="AppointmentId"}) with AllowNull = AllowNull }
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
                        Type=Decimal (Some {Precision=12; Scale=2}); IsUnique=false; Attributes = List.empty; AllowNull = NotNull; FKey=None;
                        Comments = ["was amount (18,2)"] }
                    makeUserIdColumn null AllowNull "null to allow system inserts/adjustments that aren't done by a user"
                    {makeIntFkey "PayerID" (FKeyIdentifier {Table={Schema="dbo";Name="Payers"};Column="PayerID"}) with AllowNull=AllowNull}
                    makePatientIdColumn null AllowNull null
                    // change comment after testing to 'name was timestamp'
                    { makeNonFKeyColumn "Created" (Other typeof<DateTime>) AllowNull with Comments = ["was timestamp"]}
//                    {Name="Created"; Type=Other typeof<DateTime>; AllowNull=AllowNull; Comments = ["was timestamp"]; GenerateReferenceTable = false; ReferenceValuesWithComment=null;FKey=None; Attributes = List.empty}
                    { makeNonFKeyColumn "TransactionNumber" (VarChar (Length 30)) AllowNull with Comments = ["was checkNumber now will store check number or ACH number (when applicable)"]}
                    { makeNonFKeyColumn "Rcd" (Other typeof<DateTime>) AllowNull with Comments = ["Payment Recvd"]}
                    makeNonFKeyColumn "IsElectronic" (Other typeof<bool>) NotNull
                    { makeIntFkey "CCItemID" (FKeyIdentifier {Table={Schema="dbo";Name="Accounts"};Column="CCItem"}) with AllowNull=AllowNull}
                    makeNonFKeyColumn "Comments" (VarChar Max) AllowNull
            ]
        }
    ]


[<Fact>]
let testSqlGenerator () = 

    // translate SqlGenerator.tt to call into SqlMeta
    let sb = StringBuilder()
//    let manager = MacroRunner.MultipleOutputHelper.Managers.Manager.Create(tHost, sb)
    let manager = MacroRunner.MultipleOutputHelper.Managers.Manager(Some "HelloTesting.fake.tt",sb)
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
//    output
//    |> (=) (System.IO.File.ReadAllText(@"Payment.table.sql"))
//    |> Assert.True

