namespace Macros

open System
open System.Runtime.InteropServices

/// <summary> 
/// Exposes objects, methods and properties to programming tools and other 
/// applications that support Automation. 
/// </summary> 
[<ComImport()>] 
[<Guid("00020400-0000-0000-C000-000000000046")>] 
[<InterfaceType(ComInterfaceType.InterfaceIsIUnknown)>] 
[<AllowNullLiteral>]
type IDispatch =

    [<PreserveSig>] 
    abstract member GetTypeInfoCount : [<Out>] Count: byref<int> -> int 

    [<PreserveSig>] 
    abstract member GetTypeInfo : 
        [<MarshalAs(UnmanagedType.U4)>] iTInfo:int * 
        [<MarshalAs(UnmanagedType.U4)>] lcid:int * 
        [<Out>] typeInfo:byref<System.Runtime.InteropServices.ComTypes.ITypeInfo> -> int

    [<PreserveSig>] 
    abstract member GetIDsOfNames :
        riid : byref<Guid> * 
        [<MarshalAs(UnmanagedType.LPArray, ArraySubType = UnmanagedType.LPWStr)>]
        rgsNames:string[] * 
        cNames:int * 
        lcid:int * 
        [<MarshalAs(UnmanagedType.LPArray)>] rgDispId:int[] -> int

    [<PreserveSig>] 
    abstract member Invoke :
        dispIdMember:int *
        riid:byref<Guid>  *
        lcid:uint32  *
        wFlags:uint16  *
        pDispParams: byref<System.Runtime.InteropServices.ComTypes.DISPPARAMS> *
        [<Out>] pVarResult:byref<obj>  *
        pExcepInfo: byref<System.Runtime.InteropServices.ComTypes.EXCEPINFO>  *
        pArgErr : IntPtr[]  -> int

module Com = 
        /// <summary> 
        /// Returns a string value representing the type name of the specified COM object. 
        /// </summary> 
        /// <param name="comObj">A COM object the type name of which to return.</param> 
        /// <returns>A string containing the type name.</returns> 
        let GetTypeName(comObj:obj):string =
            if comObj = null || 
                    //The specified object is not a COM object 
                    Marshal.IsComObject(comObj) = false then
                        String.Empty
            else
                let mutable dispatch = comObj :?> IDispatch
                match dispatch with 
                | null -> String.Empty //The specified COM object doesn't support getting type information 
                | _ ->
                    let typeInfoRef = ref null
                    try
                        try 
                            let typeInfo = 
                                try 

                                    // obtain the ITypeInfo interface from the object 
                                    let success = dispatch.GetTypeInfo(0, 0, typeInfoRef)
                                    Some !typeInfoRef
                                with |ex -> (* ex.Dump(); *) 
                                //Cannot get the ITypeInfo interface for the specified COM object 
                                    None
                            if typeInfo.IsNone then String.Empty
                            else
                                try 
                                    let typeName = ref null
                                    let documentation = ref null
                                    let helpContext = ref 0
                                    let helpFile = ref null
                                    //retrieves the documentation string for the specified type description 
                                    let returnVal = typeInfo.Value.GetDocumentation(-1,typeName,documentation,helpContext,helpFile)
                                    !typeName
                                with |ex -> String.Empty // Cannot extract ITypeInfo information 
                        with | ex -> String.Empty // Unexpected error 
                    finally 
                        if !typeInfoRef <> null then Marshal.ReleaseComObject(!typeInfoRef) |> ignore
         