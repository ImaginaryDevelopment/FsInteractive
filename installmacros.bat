rem installmacros.bat
cd \projects\fsi
setlocal
call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\Tools\vsvars32.bat"
fsc --out:BReusable.dll --target:library BReusable.fs
fsc --out:PathMacros.dll -r:vslangproj.dll --target:library PathMacros.fs
fsc --out:ProcessMacros.dll -r:BReusable.dll --target:library ProcessMacros.fs
fsc --out:SqlMacros.dll -r:BReusable.dll --target:library SqlMacros.fs
fsc --out:ComMacros.dll --target:library ComMacros.fs
fsc --out:DteMacros.dll -r:envdte.dll -r:envdte80.dll -r:System.Management.dll -r:vslangproj.dll -r:System.Xml.Linq.dll -r:System.Data.Entity.Design.dll --target:library dtemacros.fs
fsc --out:WinFormsMacros.dll -r:System.Windows.Forms.dll -r:PresentationCore -r:PresentationFramework -r:vslangproj.dll -r:WindowsBase -r:Breusable --target:library WinFormsMacros.fs
fsc --out:WpfMacros.dll -r:FSharp.Compiler.Interactive.Settings.dll -r:PresentationCore.dll -r:PresentationFramework.dll -r:WindowsBase.dll --target:library WpfMacros.fs
fsc --out:LambdaOps.dll -r:FSharp.Compiler.Interactive.Settings.dll --target:library LambdaOps.fs
fsc --out:LdapMacros.dll -r:System.DirectoryServices -r:FSharp.Compiler.Interactive.Settings.dll --target:library  LdapMacros.fs
fsc --out:WmiMacros.dll -r:BReusable.dll -r:System.Management -r:System.DirectoryServices -r:FSharp.Compiler.Interactive.Settings.dll --target:library MacroRunner/MacroRunner/Extensions.fs WmiMacros.fs
set vsIde=C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\
set teamExplorer=%vsIde%CommonExtensions\Microsoft\TeamFoundation\Team Explorer
rem C:\Program Files (x86)\Microsoft Visual Studio 14.0\Common7\IDE\Extensions\jzcjey4t.o05\Microsoft.TeamFoundation.VersionControl.Client.dll
rem #r @"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll"
rem #r @"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll"
rem #r @"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Common.dll"
echo %refAssemblies%
fsc --out:TfsMacros.dll -r:vslangproj.dll -r:"%teamExplorer%\Microsoft.TeamFoundation.Build.Client.dll" -r:"%teamExplorer%\Microsoft.TeamFoundation.Client.dll" -r:"%teamExplorer%\Microsoft.TeamFoundation.VersionControl.Client.dll" -r:"%teamExplorer%\Microsoft.TeamFoundation.VersionControl.Common.dll" --target:library TfsMacros.fs
IF %ERRORLEVEL% NEQ 0 pause
copy *.dll "%vsIde%PublicAssemblies\"
