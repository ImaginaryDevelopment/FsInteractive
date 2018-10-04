namespace LambdaOps
open System

// replacement for the verbose c# in my extensions file
[<AbstractClass;Sealed>]
type LambdaOp private () =
    static let apply2 (f:Func<'a,'b>) (a:'a):'b = f.Invoke(a)
    static let apply3 (f:Func<'a,'b,'c>) (a:'a):Func<'b,'c> = System.Func<_,_>(fun b -> f.Invoke(a,b))
    static let apply4 f a = System.Func<_,_,_>(f a)
    static let apply5 f a = System.Func<_,_,_,_>(f a)

    static member Curry (f:Func<_,_,_>) = Func<_,_>(fun a -> Func<_,_>(fun b -> f.Invoke(a,b)))
    static member Curry (f:Func<_,_,_,_>) = Func<_,_>(fun a-> Func<_,_>(fun b-> Func<_,_>(fun c-> f.Invoke(a,b,c))))

    static member private Application (f,a)= f a
    static member private Application (f,a,b) = f a b

    static member Apply (f:Func<'a,'b>,a:'a):Func<_> = Func<_>(fun () -> f.Invoke(a))
    static member Apply (f:Func<'a,'b,'c>,a:'a):Func<'b,'c> = apply3 f a
    // static member Apply (f:Func<'a,'b,_>,a:'a) = Func<_,_>(fun b -> f.Invoke(a,b))
    // static member Apply (f:Func<'a,'b,'c,_>,a:'a):Func<'b,'c,_> = apply4 a