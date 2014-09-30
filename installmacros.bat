rem installmacros.bat
cd \projects\fsi
setlocal

fsc --out:PathMacros.dll --reference:vslangproj.dll --target:library pathmacros.fs

fsc --out:DteMacros.dll --reference:envdte.dll --reference:System.Management.dll --reference:vslangproj.dll --target:library dtemacros.fs
fsc --out:WinFormsMacros.dll --reference:System.Windows.Forms.dll --reference:vslangproj.dll --target:library winformsmacros.fs
fsc --out:WpfMacros.dll -r:FSharp.Compiler.Interactive.Settings.dll -r:PresentationCore.dll -r:PresentationFramework.dll -r:WindowsBase.dll --target:library wpfmacros.fs
fsc --out:LambdaOps.dll -r:FSharp.Compiler.Interactive.Settings.dll --target:library LambdaOps.fs
fsc --out:LdapMacros.dll -r:System.DirectoryServices -r:FSharp.Compiler.Interactive.Settings.dll --target:library  LdapMacros.fs
fsc --out:WmiMacros.dll -r:System.Management -r:System.DirectoryServices -r:FSharp.Compiler.Interactive.Settings.dll --target:library MacroRunner/MacroRunner/Extensions.fs WmiMacros.fs
set vsIde=C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\
set refAssemblies=%vsIde%ReferenceAssemblies\v2.0\

rem #r @"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll"
rem #r @"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll"
rem #r @"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Common.dll"
fsc --out:TfsMacros.dll --reference:vslangproj.dll -r:"%refAssemblies%Microsoft.TeamFoundation.Build.Client.dll" -r:"%refAssemblies%Microsoft.TeamFoundation.Client.dll" -r:"%refAssemblies%Microsoft.TeamFoundation.VersionControl.Client.dll" -r:"%refAssemblies%Microsoft.TeamFoundation.VersionControl.Common.dll" --target:library tfsmacros.fs
copy *.dll "%vsIde%PublicAssemblies\"