namespace wpf
// http://stackoverflow.com/questions/5723823/fsi-wpf-event-loop
module WpfEventLoop = 
  open System    
  open System.Windows    
  open System.Windows.Threading    
  open Microsoft.FSharp.Compiler.Interactive    
  open Microsoft.FSharp.Compiler.Interactive.Settings    

  type RunDelegate<'b> = delegate of unit -> 'b     
  let Create() =         
      let app  =             
          try                 
              // Ensure the current application exists. This may fail, if it already does.                
              let app = new Application() in                 
              // Create a dummy window to act as the main window for the application.                
              // Because we're in FSI we never want to clean this up.                
              new Window() |> ignore;                 
              app              
          with :? InvalidOperationException -> Application.Current        
      let disp = app.Dispatcher        
      let restart = ref false        
      { new IEventLoop with             
          member x.Run() =                    
              app.Run() |> ignore                 
              !restart             

          member x.Invoke(f) =                  
              try 
                  disp.Invoke(DispatcherPriority.Send,new RunDelegate<_>(fun () -> box(f ()))) |> unbox                 
              with e -> eprintf "\n\n ERROR: %O\n" e; reraise()             

          member x.ScheduleRestart() =   ()                 
          //restart := true;                 
          //app.Shutdown()        
       }     

  let Install() = fsi.EventLoop <-  Create()

[<AutoOpen>]
module wpfmacros =
  open System
  open System.IO
  open System.Linq
  open System.Xml
  open System.Windows
  open System.Windows.Controls
  open System.Windows.Data
  open System.Windows.Markup
  WpfEventLoop.Install()

  let HelloWpfWindow () =
    let window = new Window(Title="Simple Test",Width = 800., Height = 600.)
    window.Show()
  (* let createDataTemplate value = // http://stackoverflow.com/a/17106767/57883
    let xmlFormat = sprintf """<DataTemplate DataType="DataGrid"><TextBlock Text="Hello"/></DataTemplate>"""
    let xml = xmlFormat // typeof<TextBlock>.Name // value
    let context = ParserContext()
    context.XamlTypeMapper <- new XamlTypeMapper(Array.empty)
    // context.XamlTypeMapper.AddMappingProcessingInstruction(System.String.empty, "http://schemas.microsoft.com/winfx/2006/xaml/presentation")
    // context.XamlTypeMapper.AddMappingProcessingInstruction("x", "http://schemas.microsoft.com/winfx/2006/xaml")
    context.XmlnsDictionary.Add(System.String.Empty,"http://schemas.microsoft.com/winfx/2006/xaml/presentation")
    context.XmlnsDictionary.Add("x","http://schemas.microsoft.com/winfx/2006/xaml")
    context.XmlnsDictionary.Add("System","clr-namespace:System;assembly=mscorlib")
    printfn "xml: %s" xml
    let xaml = XamlReader.Parse(xml,context)
    printfn "%A" xaml
    let template = DataTemplate(xaml)
    printfn "%A" template
    template
  *)
  (* let createTemplate = 
    let templ = """<DataTemplate xmlns='http://schemas.microsoft.com/winfx/2006/xaml/presentation' xmlns:x='http://schemas.microsoft.com/winfx/2006/xaml'><Grid><TextBlock Text="Hello"/></Grid></DataTemplate>"""
    printfn "templ: %s" templ
    use xmlReader = XmlReader.Create(new StringReader(templ))
    let xaml = XamlReader.Load(xmlReader)
    printfn "Xamlreader finished %A" xaml
    DataGridTemplateColumn(xaml) 
  *)
  let createTemplate = // http://stackoverflow.com/questions/8779893/create-datagridtemplatecolumn-through-c-sharp-code
    let binding = Binding("Changes", Mode=BindingMode.OneWay)
    // let textFactory = new FrameworkElementFactory(typeof<TextBlock>)
    let listFactory = new FrameworkElementFactory(typeof<ListView>)
    listFactory.SetBinding(ListView.ItemsSourceProperty,binding)
    // panelFactory.AppendChild(textFactory)
    let textTemplate = DataTemplate(VisualTree=listFactory)
    textTemplate

  let display<'a> (source:'a seq) =
    let window = new Window(Title="Data display", Width = 800., Height = 600.)
    let grid = new DataGrid()
    // interesting: (DataTemplate)System.Windows.Markup.XamlReader.Parse(dt);
    let changesColumn = 
      let col = DataGridTemplateColumn()
      col.Header<-"File Changes"
      let template = 
        // createTemplate typeof<string> typeof<TextBlock>
        createTemplate
      // template.DataType <- typeof<DataGridTemplateColumn>
      printfn "about to set dataDataGridTemplateColumn's cell template %A" template
      col.CellTemplate <- template
      col
    // http://msdn.microsoft.com/en-us/library/cc903950(v=vs.95).aspx
    grid.AutoGeneratingColumn.Add (fun e-> if e.PropertyName ="Changes" then e.Column <- changesColumn)
    grid.ItemsSource <- source
    window.Content <- grid 
    window.Show()
