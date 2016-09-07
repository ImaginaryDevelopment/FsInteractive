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

    let copyFrom (source: _ seq) (toPopulate:IList<_>)  =
        if not <| isNull source && not <| isNull toPopulate then
            use enumerator = source.GetEnumerator()
            while enumerator.MoveNext() do
                toPopulate.Add(enumerator.Current)


    type BindingObservableCollection<'t> (items) =
        inherit ObservableCollection<'t>(items)

        new(collection : 't seq) as self =
            BindingObservableCollection(items=List<'t>())
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

    // WIP: not working, or untested
    [<Obsolete("WIP: not working or not tested")>]
    type SuppressableBindingObservableCollection<'t>(items) = 
        inherit BindingObservableCollection<'t>(items)

        let mutable suppressCc = false 
        let mutable changeQueued = false

        new(collection: 't seq) as self = 
            SuppressableBindingObservableCollection(items= List<'t>()) 
            then 
                self.Items |> copyFrom collection 
//                
//                let items = self.Items
//                if not <| isNull collection && not <| isNull items then
//                    use enumerator = collection.GetEnumerator()
//                    while enumerator.MoveNext() do
//                        items.Add(enumerator.Current)

        override x.OnCollectionChanged e = 
            printfn "inside Suppressable onCollectionChanged"
            if not x.SuppressRaiseCollectionChanged then

                base.OnCollectionChanged e
            else
                changeQueued <- true

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
        | Obs of BindingObservableCollection<'t>
        | Items of 't seq


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
//                    source.SyncContext <- SynchronizationContext.Current
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
        Button(Content="Hello World") |> sp.Children.Add |> ignore<int>
        window.Content <- sp
        window.Show()
        fSetWindow window
        System.Windows.Threading.Dispatcher.Run()

    let displayAsThread itemSource = 
        let dispatcherSyncContext = new DispatcherSynchronizationContext(Dispatcher.CurrentDispatcher)
        let mutable window: Window = null
        let fOnStart () = 
            SynchronizationContext.SetSynchronizationContext(dispatcherSyncContext)
            display (fun w -> window <- w) itemSource

        let t = Thread(fOnStart)
        t.SetApartmentState ApartmentState.STA
        t.IsBackground <- true
        t.Start()
        // let the thread finish starting up so it can populate window
        Thread.Sleep(1000)
        t,window

    let testBindingCollection() = 
        let items = BindingObservableCollection(["Hello World"])
        let wpfThread,window = items |> ItemSource<string>.Obs |> displayAsThread
        items,wpfThread,window

    let testIt() = 
        let items = SuppressableBindingObservableCollection([ "Hello World"])
        items.SuppressRaiseCollectionChanged <- true
        let wpfThread,window = items :> BindingObservableCollection<_> |> ItemSource<string>.Obs |> displayAsThread
        items,wpfThread,window

