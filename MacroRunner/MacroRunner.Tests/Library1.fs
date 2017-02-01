namespace MacroRunner.Tests
open System
open System.Text
open global.Xunit
open global.Xunit.Abstractions
open Macros.SqlMacros
open CodeGeneration
open BReusable


type MyFirstTests() =
    [<Fact>] // fact methods are not allowed to have parameters
    member __.MyFirstFact () = Assert.True(1 = 1)
    [<Theory>]// theory methods are required to have input attributes
    [<InlineData(3)>]
    [<InlineData(5)>]
    member __.MyFirstTheory v = Assert.True(v % 2 = 1)

module CodeGeneration =
    open DataModelToF
    open System.Diagnostics

    [<Fact>]
    let ``generateINotifyClass Foo generates a FooN type``() =
        let columns =
            [
                {ColumnDescription.ColumnName="Bar";
                 Type = "string"
                 Length = 1
                 Nullable = false
                 IsPrimaryKey = false
                 IsIdentity = false}
            ]
        let sb = StringBuilder()
        let appendIndented indentLevel text =
            List.replicate (4*indentLevel) " " |> delimit String.Empty
            |> flip (sprintf "%s%s") text
            |> sb.AppendLine
            |> ignore<StringBuilder>
        DataModelToF.generateINotifyClass("Foo", columns, appendIndented, false)
        let generatedClass = sb |> string
        Debug.WriteLine(sb.ToString())
        Assert.Contains("type FooN", generatedClass)

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
