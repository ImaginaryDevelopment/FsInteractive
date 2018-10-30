namespace BCore.CHelpers

open System
open System.Runtime.CompilerServices

[<Extension>]
type Delegates =
    [<Extension>]
    static member toFSharpFunc act = Action.invoke act
    [<Extension>]
    static member toFSharpFunc act = Action.invoke1 act
    [<Extension>]
    static member toFSharpFunc act = Action.invoke2 act
    [<Extension>]
    static member toFSharpFunc act = Action.invoke3 act
    [<Extension>]
    static member toFSharpFunc act = Action.invoke4 act
    [<Extension>]
    static member toFSharpFunc act = Action.invoke5 act
    [<Extension>]
    static member toFSharpFunc func = Func.invoke func
    [<Extension>]
    static member toFSharpFunc func = Func.invoke1 func
    [<Extension>]
    static member toFSharpFunc func = Func.invoke2 func
    [<Extension>]
    static member toFSharpFunc func = Func.invoke3 func
    [<Extension>]
    static member toFSharpFunc func = Func.invoke4 func
    [<Extension>]
    static member toFSharpFunc func = Func.invoke5 func

