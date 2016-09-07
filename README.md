FsInteractive [![Build status](https://ci.appveyor.com/api/projects/status/diqrgmxv00bhpeux?svg=true)](https://ci.appveyor.com/project/ImaginaryDevelopment/fsinteractive)
=============

The purpose of this repository was originally for all my F# interactive mode code, (and handy FSharp scripts) but it has grown.
Now it is broken into 

1. A script that compiles and installs a large portion of the code into somewhere you can easily call it from F# interactive inside Visual Studio (this directory)
2. Individually useful F# source code
3. A solution that includes code gen translated from my T4 C# into F# from https://github.com/ImaginaryDevelopment/LinqPad/tree/master/T4
4.  Including the following previously available NuGet packages
5.   https://www.nuget.org/packages/T4EnvDte/
6.   https://www.nuget.org/packages/T4MultiFile/

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

