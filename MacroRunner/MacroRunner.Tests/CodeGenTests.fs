﻿namespace MacroRunner.Tests
open System
open System.Text
open global.Expecto
open Macros.SqlMacros
open CodeGeneration
open BReusable
open BReusable.StringHelpers

module HelloWorldTests =
    [<Tests>]
    let myFirstTests =
        testList "myFirstTests" [
            testCase "MyFirstTest" <|
                fun () -> Expect.isTrue(1 = 1) "1 doesn't equal 1?"
            testList "inlineData conversion" <|
                List.ofSeq (testParam [3;5] [
                        "inlineData",
                            fun (values:int list) () ->
                                Expect.all values (fun v -> v % 2 = 1) "odd wasn't odd"
                    ])
        ]

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
                testCase "Primitives" <|
                    fun () ->
                        let items =
                            [   "int","0"
                                "int64", "0L"
                            ]
                        Expect.all items (fun (t,expected) -> getDefaultValue None t = expected) "Primitive fail"
            ]
        ]
module CodeGeneration =
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

//
///// 1 properties
//type FooN (model:FooRecord) =
//
//    let propertyChanged = new Event<_, _>()
//
//
//    let mutable bar = model.Bar
//
//    interface IFoo with
//        /// string (1) not null
//        member x.Bar with get () = x.Bar
//    interface IFooRW with
//        /// string (1) not null
//        member x.Bar with get () = x.Bar and set v = x.Bar <- v
//
//    member x.MakeRecord () =
//        {
//        Bar = x.Bar
//        }
//
//    interface INotifyPropertyChanged with
//        [<CLIEvent>]
//        member x.PropertyChanged = propertyChanged.Publish
//    abstract member RaisePropertyChanged : string -> unit
//    default x.RaisePropertyChanged(propertyName : string) = propertyChanged.Trigger(x, PropertyChangedEventArgs(propertyName))
//
//    abstract member SetAndNotify<'t> : string * 't byref * 't -> bool
//    default x.SetAndNotify<'t> (propertyName, field: 't byref, value:'t) =
//        if obj.ReferenceEquals(box field,box value) then false
//        else
//            field <- value
//            x.RaisePropertyChanged(propertyNam)
//            true
//
//    abstract member SetAndNotify<'t,'b> : string * 'b * 't Action * 't -> bool
//    default x.SetAndNotify<'t,'b> (propertyName, baseValue:'b, baseSetter: 't Action, value:'t) =
//        if obj.ReferenceEquals(box baseValue,box value) then false
//        else
//            baseSetter.Invoke value
//            x.RaisePropertyChanged(propertyName)
//            true
//
//    /// string (1) not null
//    member x.Bar
//        with get() = bar
//        and set v =
//            bar <- v
//            x.RaisePropertyChanged "Bar"
//
//