namespace PureCodeGenerationTests
open System
open System.Text
open global.Expecto
open CodeGeneration
open BReusable
open BReusable.StringHelpers


module PureCodeGeneration =
    open CodeGeneration.PureCodeGeneration
    let zero() = {ClassDeclaration.Attributes=List.empty;Name=null;BaseClass=None;Fields=List.empty;Members=List.empty}

    [<Tests>]
    let cg = testList "PureCodeGeneration" [
            testList "ClassDeclaration" [
                //testCase "pcg" <|
                //    fun () ->
                //        let x = {zero() with Name="MyProperty"}
                //        let actual = x.GetAttributeText()
                //        Expect.equal actual "" "Should be empty"
                testCase "AttributeText gen" <|
                    fun () ->
                        let x ={zero() with Name="MyProperty"; Attributes=["MyProperty2"]}
                        let actual = x.GetAttributeText()
                        Expect.all x.Attributes (fun v -> actual.Contains v && (actual.Contains <| sprintf "[<%s>]" v)) "bad attribute gen"
                testCase "FieldText gen" <|
                    fun () ->
                        let x ={zero() with Fields=["Toy";"Boy"]}
                        let actual = x.FieldText " "
                        Expect.all x.Fields (fun v -> actual.Contains v) "bad field gen"
            ]
            testList "PureMeasure" [
                testCase "IsInValidMeasure" <|
                    fun () ->
                        let actual = PureMeasure.IsValidMeasureOpt ""
                        Expect.isFalse actual "an empty string is not a measure"
            ]
            testList "PureColumnTypeName" [
                testCase "Accepts string" <|
                    fun () -> Expect.isSome (PureColumnTypeName.Create "string") "String failed"
                testCase "Rejects Nullable" <|
                    fun () -> Expect.isNone (PureColumnTypeName.Create "int Nullable") "Nullable failed"
                testCase "Rejects ? Nullable" <|
                    fun () -> Expect.isNone (PureColumnTypeName.Create "int?") "?Nullable failed"
                testCase "Rejects option" <|
                    fun () -> Expect.isNone (PureColumnTypeName.Create "int option") "option failed"
                testCase "Rejects sqlTypes" <|
                    fun () ->
                        ["bit"; "varchar"; "char"; "nvarchar"; "nchar";"datetime";"xml";"datetime2"]
                        |> List.iter(fun x ->
                            Expect.isNone (PureColumnTypeName.Create x) <| sprintf "%s is a sql type, not a column type name" x
                        )
                testCase "ReflectedCreation" <|
                    fun () ->
                        let asm = typeof<System.Int16>.Assembly
                        asm.ExportedTypes
                        |> Seq.iter(fun x ->
                            Expect.isSome (PureColumnTypeName.Create x.Name) <| sprintf "%s failed" x.Name
                        )

            ]
            testList "getDefaultValue" [
                testCase "Option is None" <|
                    fun () ->
                        let expected = "None"
                        let actual = getDefaultValue None "System.String option"
                        Expect.equal actual expected "Any option should return None"
                testCase "measures are included" <|
                    fun () ->
                        let m = PureMeasure.create "Gold"
                        let expected = "0<Gold>"
                        let actual = getDefaultValue m "int"
                        Expect.equal actual expected "bad default with measure"
                testCase "Samples" <|
                    fun () ->
                        let items =
                            [   "int","0"
                                "int64", "0L"
                                "int Nullable", "Nullable()"
                                "int option", "None"
                            ]
                        items |> List.iter(fun (item,expected) ->
                            Expect.equal (getDefaultValue None item) expected <| sprintf "Sample fail for type %s" item
                        )
            ]
        ]
