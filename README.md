FsInteractive
=============

my reusable F# interactive code
Includes
 - Wpf window display of a sequence of data
 - <a href="https://github.com/ImaginaryDevelopment/FsInteractive/blob/master/winformsmacros.fs">Win Forms</a> macros
  - display of a sequence of data
 - <a href="https://github.com/ImaginaryDevelopment/FsInteractive/blob/master/tfsmacros.fs">Tfs</a> macros - querying via the Tfs Dlls
  - Get a user's changes
  - Get changes checked in with no associated work item
 - <a href="https://github.com/ImaginaryDevelopment/FsInteractive/blob/master/dtemacros.fs">EnvDte</a> macros
  - Change the startup project
 - a <a href="https://github.com/ImaginaryDevelopment/FsInteractive/blob/master/installmacros.bat">batch file</a> to install them all into Visual Studio's Public assemblies folder for easy referencing
  - with `#r "tfsmacros";open tfsmacros;; let tfs=getTfs();;`
  - without 
    -  compile the files individually or as desired (referencing the dlls needed to build them)
    -  then in fsi reference them `#r "(yourdirectory)\tfsmacros";open tfsmacros;; let tfs=getTfs();;`