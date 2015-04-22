rem installmacros.bat
cd \projects\fsi
setlocal
call "C:\Program Files (x86)\Microsoft Visual Studio 12.0\VC\bin\vcvars32.bat"
fsc --out:PathMacros.dll --reference:vslangproj.dll --target:library PathMacros.fs

fsc --out:DteMacros.dll --reference:envdte.dll --reference:envdte80.dll --reference:System.Management.dll --reference:vslangproj.dll --target:library DteMacros.fs
fsc --out:WinFormsMacros.dll --reference:System.Windows.Forms.dll --reference:vslangproj.dll --target:library WinFormsMacros.fs
fsc --out:WpfMacros.dll -r:FSharp.Compiler.Interactive.Settings.dll -r:PresentationCore.dll -r:PresentationFramework.dll -r:WindowsBase.dll --target:library WpfMacros.fs
fsc --out:LambdaOps.dll -r:FSharp.Compiler.Interactive.Settings.dll --target:library LambdaOps.fs
fsc --out:LdapMacros.dll -r:System.DirectoryServices -r:FSharp.Compiler.Interactive.Settings.dll --target:library  LdapMacros.fs
fsc --out:WmiMacros.dll -r:System.Management -r:System.DirectoryServices -r:FSharp.Compiler.Interactive.Settings.dll --target:library MacroRunner/MacroRunner/Extensions.fs WmiMacros.fs
set vsIde=C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\
set refAssemblies=%vsIde%ReferenceAssemblies\v2.0\

rem #r @"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll"
rem #r @"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll"
rem #r @"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Common.dll"
fsc --out:TfsMacros.dll --reference:vslangproj.dll -r:"%refAssemblies%Microsoft.TeamFoundation.Build.Client.dll" -r:"%refAssemblies%Microsoft.TeamFoundation.Client.dll" -r:"%refAssemblies%Microsoft.TeamFoundation.VersionControl.Client.dll" -r:"%refAssemblies%Microsoft.TeamFoundation.VersionControl.Common.dll" --target:library TfsMacros.fs
IF %ERRORLEVEL% NEQ 0 pause
copy *.dll "%vsIde%PublicAssemblies\"
