module DataModelToFTests

open System
open System.Text
open global.Expecto
open CodeGeneration
open BReusable
open BReusable.StringHelpers
open DataModelToF
open System.Diagnostics
open CodeGeneration.SqlScriptGeneration

[<Tests>]
let ``generateINotifyClass Foo generates a FooN type``=
    testCase "generateINotifyClass Foo generates a FooN type" <|
        fun () ->
            let columns =
                SqlTableColumnChoice.SqlTableColumnMeta [
                    {ColumnDescription.ColumnName="Bar";
                     Type = "string"
                     Length = 1
                     Nullable = false
                     IsPrimaryKey = false
                     IsComputed = false
                     IsIdentity = false}
                ]
            let sb = StringBuilder()
            let appendIndented indentLevel text =
                List.replicate (4*indentLevel) " " |> delimit String.Empty
                |> flip (sprintf "%s%s") text
                |> sb.AppendLine
                |> ignore<StringBuilder>

            DataModelToF.generateINotifyClassSql (fun _ -> None) ({SettersCheckInequality=false;AllowPropertyChangeOverride=false},"Foo", columns, appendIndented)
            let generatedClass = sb |> string
            Debug.WriteLine(sb.ToString())
            Expect.stringContains generatedClass "type FooN" "Didn't find expected INotify class name"
