rem installmacros.bat
cd \projects\fsi
setlocal

fsc --out:vspathmacros.dll --reference:vslangproj.dll --target:library vspathmacros.fs

fsc --out:fsmacros.dll --reference:envdte.dll --reference:System.Management.dll --reference:vslangproj.dll --target:library vsfsmacros.fs

set vsIde=C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\
set refAssemblies=%vsIde%ReferenceAssemblies\v2.0\

rem #r @"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.Client.dll"
rem #r @"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Client.dll"
rem #r @"C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\ReferenceAssemblies\v2.0\Microsoft.TeamFoundation.VersionControl.Common.dll"

fsc --out:tfsmacros.dll -r:"%refAssemblies%Microsoft.TeamFoundation.Client.dll" -r:"%refAssemblies%Microsoft.TeamFoundation.VersionControl.Client.dll" -r:"%refAssemblies%Microsoft.TeamFoundation.VersionControl.Common.dll" --target:library tfsfsmacros.fs
copy *.dll "%vsIde%PublicAssemblies\"