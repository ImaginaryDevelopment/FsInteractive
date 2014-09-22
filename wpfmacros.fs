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
  open System.Windows
  WpfEventLoop.Install()
  let simpleWindow () = 
    let window = new Window(Title="Simple Test",Width = 800., Height = 600.)
    window.Show()
