module System
type System.String with
    member x.After (delimiter:string) = 
        x.Substring(x.IndexOf(delimiter) + delimiter.Length)
    member x.AfterOrSelf(delimiter) = 
        if x.Contains delimiter then x.After delimiter else x
    member x.AfterLast (delimiter:string) = 
        if x.Contains delimiter = false then failwithf "After last called with no match"
        x.Substring(x.LastIndexOf(delimiter) + delimiter.Length)
    member x.Before (delimiter:string) = 
        x.Substring(0, x.IndexOf(delimiter))