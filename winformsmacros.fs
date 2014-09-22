[<AutoOpen>]
module winformsmacros
// winformfsmacros.fs
open System.Windows.Forms

let display<'a> (source:'a seq) = 
  use frm = new Form()
  use grd = new DataGridView()
  grd.DataSource <- System.Collections.Generic.List(source)
  grd.Dock <- DockStyle.Fill
  frm.Controls.Add(grd)
  frm.Activate()
  frm.ShowDialog() |> ignore