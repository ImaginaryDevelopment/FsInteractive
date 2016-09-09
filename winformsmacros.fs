namespace Macros.UI
#if INTERACTIVE
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "System.Windows.dll"
#r "System.Xaml"
#r "WindowsBase"
#endif
open System.Collections.ObjectModel
open System



module WinformsMacros =
// winformfsmacros.fs
    open System.Windows.Forms
    type ItemSource<'t> =
        | Items of 't seq
        | Obs of 't ObservableCollection

    type private ItemSource = 
        |Dummy
        with
            static member internal GetBox<'t> (itemSource:ItemSource<'t>) =
                match itemSource with
                | Items source -> source |> box
                | Obs source -> source |> box

    let displayModal itemSource = 
      use frm = new Form()
      use grd = new DataGridView()
      grd.DataSource <- ItemSource.GetBox itemSource
      grd.Dock <- DockStyle.Fill
      frm.Controls.Add(grd)
      frm.Activate()
      frm.ShowDialog() 

    let display itemSource : IDisposable = 
        let frm = new Form()
        let grd = new DataGridView()
        grd.DataSource <- ItemSource.GetBox itemSource
        grd.Dock <- DockStyle.Fill
        frm.Controls.Add grd
        frm.Activate()
        frm.Show() 
        {
            new IDisposable with
                member __.Dispose () = 
                    grd.Dispose()
                    frm.Dispose()
            }


module WpfMacros = 
    open System.Windows
    open System.Windows.Controls
    open System.Windows.Threading
    open System.Threading
    open System.Windows.Data
    open System.Collections.Generic
    open System.Collections.Specialized
    open System.Windows.Input
    open BReusable

    let inline addCommand cmd x = 
        (^t:(member set_Command:ICommand -> unit)(x,cmd))
        x

    let copyFrom (source: _ seq) (toPopulate:IList<_>)  =
        if not <| isNull source && not <| isNull toPopulate then
            use enumerator = source.GetEnumerator()
            while enumerator.MoveNext() do
                toPopulate.Add(enumerator.Current)
    type FunCommand (fExecute, ?fCanExecuteOpt) =
        let canExecuteChanged = new Event<_,_>()
        let fCanExecute = defaultArg fCanExecuteOpt (fun _ -> true)
//        [<CLIEvent>]
//        member __.CanExecuteChanged = canExecuteChanged.Publish
        member x.CanExecuteChanged = canExecuteChanged.Publish

        interface ICommand with
            member __.CanExecute o = fCanExecute o
            member __.Execute o = fExecute o
            [<CLIEvent>]
            member x.CanExecuteChanged = x.CanExecuteChanged


    type BindableObservableCollection<'t> (items) =
        inherit ObservableCollection<'t>(items)

        new(collection : 't seq) as self =
            BindableObservableCollection(items=List<'t>())
            then
                self.Items |> copyFrom collection 
//                let items = self.Items
//                if not <| isNull collection && not <| isNull items then
//                    use enumerator = collection.GetEnumerator()
//                    while enumerator.MoveNext() do
//                        items.Add(enumerator.Current)

        member private __.Occ (e:obj) = base.OnCollectionChanged (e :?> NotifyCollectionChangedEventArgs)
        member val Dispatcher = Dispatcher.CurrentDispatcher with get,set

        override x.OnCollectionChanged e = 
            printfn "Yay collection changed override"
            x.Dispatcher.Invoke(fun () ->
                x.Occ e
            )
            printfn "OnCollection changed finished"

    type SuppressibleBindableObservableCollection<'t>(items) = 
        inherit BindableObservableCollection<'t>(items)

        let mutable suppressCc = false 
        let mutable changeQueued = false

        new(collection: 't seq) as self = 
            SuppressibleBindableObservableCollection(items= List<'t>()) 
            then 
                self.Items |> copyFrom collection 

        override x.OnCollectionChanged e = 
            printfn "inside Suppressible onCollectionChanged"
            if not x.SuppressRaiseCollectionChanged then

                base.OnCollectionChanged e
            else
                changeQueued <- true
            printfn "Suppressible onCollectionChanged finished"

        member x.SuppressRaiseCollectionChanged
            with get() = suppressCc
            and set v =
                match v, changeQueued with
                | true, _ -> suppressCc <- true
                | false, false -> suppressCc <- false
                | false, true -> 
                    suppressCc <- false
                    // hopefully reset will cover all the bases
                    x.OnCollectionChanged (NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset))

    type ItemSource<'t> = 
        | Obs of BindableObservableCollection<'t>
        | Items of 't seq

    // not sure how well this generalizes or if it works outside this one use case
    let private (|IsUsableAs|_|) (_:'a) (t:Type) = 
        let t2 = typeof<'a>
        printfn "IsAssignableFrom: Checking %s is assignable from %s" t.Name t2.Name
        if t.IsAssignableFrom(t2) then
            Some()
        elif t.IsGenericType && t2.IsGenericType && t.GenericTypeArguments.Length = t2.GenericTypeArguments.Length && t.GenericTypeArguments |> Seq.zip t2.GenericTypeArguments |> Seq.forall (fun (ta1,ta2) -> ta1.IsAssignableFrom(ta2) || ta2.IsAssignableFrom(ta1)) then
            printfn "generics lined up, kinda"
            Some()
            //None
        else
            printfn "typeof %A :?> %A -> fail" t t2
            None


    // threaded wpf ui - http://reedcopsey.com/2011/11/28/launching-a-wpf-window-in-a-separate-thread-part-1/
    
    // assumes you don't depend on a synchronization context
    let display fSetWindow itemSource= 
        let window = Window()
        window.Closed.Add (fun _ -> Dispatcher.CurrentDispatcher.BeginInvokeShutdown DispatcherPriority.Background)
        let sp = StackPanel()
        // simple way to display items based on https://social.msdn.microsoft.com/Forums/vstudio/en-US/3b09b049-9622-4d17-be75-210c99b44dba/simplest-way-to-display-a-list-of-strings-and-floats-listbox-listview-textbox-textblock-so?forum=wpf
        let listDisplayer =
            let source = 
                match itemSource with
                | Items source -> source :> _ seq
                | Obs source -> 
                    source.Dispatcher <- Dispatcher.CurrentDispatcher
                    upcast source
            ItemsControl(ItemsSource= source) 
        // list display template based on http://stackoverflow.com/a/23080314/57883
        let makeTextBlockTemplate() = 
            let textBlockFactory = new FrameworkElementFactory(typeof<TextBlock>)
            textBlockFactory.SetValue(TextBlock.TextProperty, Binding("."))
            let t = DataTemplate()
            t.VisualTree <- textBlockFactory
            t

        listDisplayer.ItemTemplate <- makeTextBlockTemplate()

        listDisplayer |> sp.Children.Add |> ignore<int>
        //let btnClear = 
        let clearCommand = 
            // not planning on accounting for the possibility the itemsSource is changed.
            let src = listDisplayer.ItemsSource
            let tSrc = src.GetType()
            let fClearOpt = 
                match tSrc with
                | IsUsableAs (Reflection.isType:IList<_>) -> 
                    let m = tSrc.GetMethod("Clear")
                    if not <| isNull m then 
                        Some (fun () -> m.Invoke(src,null) |> ignore<obj>)
                    else
                        printfn "Could not locate clear method on type %s" tSrc.Name
                        None
                | _ -> None
                |> Option.map (fun f -> fun _ -> f())
            let fClear = match fClearOpt with | Some f -> f | None -> fun _ -> ()
            let fCanClear _ = fClearOpt |> Option.isSome
            FunCommand(fClear, fCanClear)
            
        Button(Content="Clear")
        |> addCommand clearCommand
        |> sp.Children.Add |> ignore<int>
        window.Content <- sp
        window.Show()
        fSetWindow window
        System.Windows.Threading.Dispatcher.Run()

    let displayAsThread itemSource = 
        let dispatcherSyncContext = new DispatcherSynchronizationContext(Dispatcher.CurrentDispatcher)
        let window: Window ref = ref null // f# 4.3 compatability
        let fOnStart () = 
            SynchronizationContext.SetSynchronizationContext(dispatcherSyncContext)
            display (fun w -> window := w) itemSource

        let t = Thread(fOnStart)
        t.SetApartmentState ApartmentState.STA
        t.IsBackground <- true
        t.Start()
        // let the thread finish starting up so it can populate window
        Thread.Sleep(1000)
        t,window

    let testCollection (items:'t seq) = items |> ItemSource.Items |> displayAsThread
    let testBindable (items:'t seq) : BindableObservableCollection<'t> * _ * _ = 
        let src = BindableObservableCollection(items)
        let t,w = src |> ItemSource.Obs |> displayAsThread
        src,t,w

    let testSuppressible items = 
        let src = SuppressibleBindableObservableCollection<_>(collection= items)
        let t,w = src :> BindableObservableCollection<_> |> ItemSource.Obs |> displayAsThread
        src,t,w

    let testSuppression() = 
        let items = SuppressibleBindableObservableCollection(["Hello World"])
        items.SuppressRaiseCollectionChanged <- true
        let wpfThread,window = items :> BindableObservableCollection<_> |> ItemSource<string>.Obs |> displayAsThread
        items,wpfThread,window
    type SampleObject = {Name:string; Identifier:int}
        with override x.ToString() = sprintf "%A" x
    let testDisplayObjects() =
        [ {Name="HelloObjects"; Identifier=0}]
        |> testCollection
        

