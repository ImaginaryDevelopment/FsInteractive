[<AutoOpen>]
module Pm.Schema.BReusable
open System
open System.Collections.Generic
open System.Diagnostics
// for statically typed parameters in an active pattern see: http://stackoverflow.com/questions/7292719/active-patterns-and-member-constraint
//consider pulling in useful functions from https://gist.github.com/ruxo/a9244a6dfe5e73337261

module IntPatterns =
    let (|PositiveInt|Zero|NegativeInt|) (x:int<_>) =
        if x > 0<_> then PositiveInt
        elif x = 0<_> then Zero
        else NegativeInt

// things that assist with point-free style
[<AutoOpen>]
module FunctionalHelpersAuto =
    let teeTuple f x = x, f x
    /// take a dead-end function and curry the input
    let tee f x = f x; x
    /// super handy with operators like (*) and (-)
    /// take a function that expects 2 arguments and flips them before applying to the function
    let inline flip f x y = f y x
    /// take a tuple and apply the 2 arguments one at a time (from haskell https://www.haskell.org/hoogle/?hoogle=uncurry)
    let uncurry f (x,y) = f x y
    /// does not work with null x
    let inline getType x = x.GetType()
    // based on http://stackoverflow.com/a/2362114/57883
    // mimic the C# as keyword
    let castAs<'t> (o:obj): 't option =
        match o with
        | :? 't as x -> Some x
        | _ -> None
    // long pipe chains don't allow breakpoints anywhere inside
    // does this need anything to prevent the method from being inlined/optimized away?
    let breakpoint x =
        let result = x
        result
    let breakpointf f x =
        let result = f x
        result

module Tuple2 = // idea and most code taken from https://gist.github.com/ploeh/6d8050e121a5175fabb1d08ef5266cd7
    let replicate x = x,x
    // useful for Seq.mapi
    let fromCurry x y = (x,y)
    let curry f x y = f (x, y)
    // calling already defined function from outer namespace, instead of duplicating the functionality for consistency with gist
    let uncurry f (x, y) = uncurry f (x, y)
    let swap (x, y) = (y, x)
    let mapFst f (x, y) = f x, y
    let mapSnd f (x, y) = x, f y
    let extendFst f (x,y) = f (x,y), y
    let extendSnd f (x,y) = x, f(x,y)
    let optionOfFst f (x,y) =
        match f x with
        | Some x -> Some (x, y)
        | None -> None
    let optionOfSnd f (x,y) =
        match f y with
        | Some y -> Some (x,y)
        | None -> None
    // start Brandon additions
    let mapBoth f (x,y) = f x, f y

let private failNullOrEmpty paramName x = if String.IsNullOrEmpty x then raise <| ArgumentOutOfRangeException paramName else x
type System.String with
//        // no idea how to call this thing with a comparer
//        static member indexOf (delimiter,?c:StringComparison) (x:string) =
//            match failNullOrEmpty "delimiter" delimiter,c with
//            | d, Some c -> x.IndexOf(d,comparisonType=c)
//            | d, None -> x.IndexOf d
    static member indexOf delimiter (x:string) =
        failNullOrEmpty "delimiter" delimiter
        |> x.IndexOf
    static member indexOfC delimiter c (x:string) =
        x.IndexOf(failNullOrEmpty "delimiter" delimiter ,comparisonType=c)
// couldn't get this guy to call the other guy, so... leaving him out too
//        static member contains (delimiter, ?c:StringComparison) (x:string) =
//            match failNullOrEmpty "delimiter" delimiter, c with
//            | d, Some c -> x.IndexOf(d, comparisonType=c) |> flip (>=) 0
//            | d, None -> x.Contains d
    static member contains delimiter (x:string) =
        failNullOrEmpty "delimiter" delimiter
        |> x.Contains
    static member containsC delimiter c (x:string) =
        x
        |> String.indexOfC (failNullOrEmpty "delimiter" delimiter) c
        |> flip (>=) 0
    static member substring i (x:string) = x.Substring i
    static member substring2 i e (x:string)= x.Substring(i,e)
    // the default insensitive comparison
    static member defaultIComparison = StringComparison.InvariantCultureIgnoreCase
    static member Null:string = null
    static member trim (s:string) = match s with | null -> null | s -> s.Trim()
    static member split (delims:string seq) (x:string) = x.Split(delims |> Array.ofSeq, StringSplitOptions.None)
    static member splitO (items:string seq) options (x:string) = x.Split(items |> Array.ofSeq, options)
    static member emptyToNull (x:string) = if String.IsNullOrEmpty x then null else x
    static member equalsI (x:string) (x2:string) = not <| isNull x && not <| isNull x2 && x.Equals(x2, String.defaultIComparison)
    static member startsWithI (toMatch:string) (x:string) = not <| isNull x && not <| isNull toMatch && toMatch.Length > 0 && x.StartsWith(toMatch, String.defaultIComparison)
    static member isNumeric (x:string) = not <| isNull x && x.Length > 0 && x |> String.forall Char.IsNumber
    static member splitLines(x:string) = x.Split([| "\r\n";"\n"|], StringSplitOptions.None)
    static member beforeAnyOf (delimiters:string list) (x:string) =
        let index, _ =
            delimiters
            |> Seq.map (fun delimiter -> x.IndexOf(delimiter),delimiter)
            |> Seq.filter(fun (index,_) -> index >= 0)
            |> Seq.minBy (fun (index, _) -> index)
        x.Substring(0,index)
    static member replace (target:string) (replacement) (str:string) = if String.IsNullOrEmpty target then invalidOp "bad target" else str.Replace(target,replacement)
// comment/concern/critique auto-opening string functions may pollute (as there are so many string functions)
// not having to type `String.` on at least the used constantly is a huge reduction in typing
// also helps with point-free style
module StringHelpers =

    // I've been fighting/struggling with where to namespace/how to architect string functions, they are so commonly used, static members make it easier to find them
    // since typing `String.` with this module open makes them all easy to find
    // favor non attached methods for commonly used methods

//    let before (delimiter:string) (x:string) = x.Substring(0, x.IndexOf delimiter)

    let contains (delimiter:string) (x:string) = String.contains delimiter x
    let containsI (delimiter:string) (x:string) = x |> String.containsC delimiter String.defaultIComparison
    let substring i x = x |> String.substring i
    let substring2 i length (x:string)  = x |> String.substring2 i length //x.Substring(i, length)
    let before (delimiter:string) s = s |> String.substring2 0 (s.IndexOf delimiter)
    let beforeOrSelf delimiter x = if x|> String.contains delimiter then x |> before delimiter else x
    let after (delimiter:string) (x:string) =
        failNullOrEmpty "x" x
        |> tee (fun _ -> failNullOrEmpty "delimiter" delimiter |> ignore)
        |> fun x ->
            match x.IndexOf delimiter with
            | i when i < 0 -> failwithf "after called without matching substring in '%s'(%s)" x delimiter
            | i -> x |> String.substring (i + delimiter.Length)
    let afterI (delimiter:string) (x:string) =
        x
        |> String.indexOfC delimiter String.defaultIComparison
        |> (+) delimiter.Length
        |> flip String.substring x
    let afterOrSelf delimiter x = if x|> String.contains delimiter then x |> after delimiter else x
    let afterOrSelfI (delimiter:string) (x:string) = if x |> String.containsC delimiter String.defaultIComparison then x |> afterI delimiter else x
    let containsAnyOf (delimiters:string seq) (x:string) = delimiters |> Seq.exists(flip contains x)
    let containsIAnyOf (delimiters:string seq) (x:string) = delimiters |> Seq.exists(flip containsI x)
    let delimit (delimiter:string) (items:#seq<string>) = String.Join(delimiter,items)

    let endsWith (delimiter:string) (x:string) = x.EndsWith delimiter
    let isNumeric (s:string)= not <| isNull s && s.Length > 0 && s |> String.forall Char.IsNumber
    let replace (target:string) (replacement) (str:string) = if String.IsNullOrEmpty target then invalidOp "bad target" else str.Replace(target,replacement)
    let splitLines(x:string) = x.Split([| "\r\n";"\n"|], StringSplitOptions.None)
    let startsWith (delimiter:string) (s:string) = s.StartsWith delimiter
    let startsWithI (delimiter:string) (s:string) = s.StartsWith(delimiter,String.defaultIComparison)
    let trim = String.trim
//    let after (delimiter:string) (x:string) =
//        match x.IndexOf delimiter with
//        | i when i < 0 -> failwithf "after called without matching substring in '%s'(%s)" x delimiter
//        | i -> x.Substring(i + delimiter.Length)

    let afterLast delimiter x =
        if x |> String.contains delimiter then failwithf "After last called with no match"
        x |> String.substring (x.LastIndexOf delimiter + delimiter.Length)
    let stringEqualsI s1 (toMatch:string)= not <| isNull toMatch && toMatch.Equals(s1, StringComparison.InvariantCultureIgnoreCase)
    let inline isNullOrEmptyToOpt s =
        if String.IsNullOrEmpty s then None else Some s

    // was toFormatString
    // with help from http://www.readcopyupdate.com/blog/2014/09/26/type-constraints-by-example-part1.html
    let inline toFormatString (f:string) (a:^a) = ( ^a : (member ToString:string -> string) (a,f))

    //if more is needed consider humanizer or inflector
    let toPascalCase s =
        s
        |> Seq.mapi (fun i l -> if i=0 && Char.IsLower l then Char.ToUpper l else l)
        |> String.Concat

    let humanize camel :string =
        seq {
            let pascalCased = toPascalCase camel
            yield pascalCased.[0]
            for l in  pascalCased |> Seq.skip 1 do
                if System.Char.IsUpper l then
                    yield ' '
                    yield l
                else
                    yield l
        }
        |> String.Concat


// I've also been struggling with the idea that Active patterns are frequently useful as just methods, so sometimes methods are duplicated as patterns
[<AutoOpen>]
module StringPatterns =
    open StringHelpers

    let (|NullString|Empty|WhiteSpace|ValueString|) (s:string) =
        match s with
        | null -> NullString
        | "" -> Empty
        | _ when String.IsNullOrWhiteSpace s -> WhiteSpace
        | _ -> ValueString
    let (|StartsWith|_|) (str:string) arg = if str.StartsWith(arg) then Some() else None
    let (|StartsWithI|_|) s1 (toMatch:string) = if not <| isNull toMatch && toMatch.StartsWith(s1, StringComparison.InvariantCultureIgnoreCase) then Some () else None
    let (|StringEqualsI|_|) s1 (toMatch:string) = if String.equalsI toMatch s1 then Some() else None
    let (|InvariantEqualI|_|) (str:string) arg =
       if String.Compare(str, arg, StringComparison.InvariantCultureIgnoreCase) = 0
       then Some() else None
    let (|IsNumeric|_|) (s:string) = if not <| isNull s && s.Length > 0 && s |> String.forall Char.IsNumber then Some() else None
    let (|ContainsI|_|) s1 (toMatch:string) = if toMatch |> containsI s1 then Some () else None

    let (|OrdinalEqualI|_|) (str:string) arg =
       if String.Compare(str, arg, StringComparison.OrdinalIgnoreCase) = 0
       then Some() else None

    let inline (|IsTOrTryParse|_|) (t,parser) (x:obj): 't option =
        match x with
        | v when v.GetType() = t -> Some (v :?> 't)
        | :? string as p ->
            match parser p with
            | true, v -> Some v
            | _, _ -> None
        | _ -> None

    let (|Int|_|) (x:obj) =
        match x with
        | :? string as p ->
            let success,value = System.Int32.TryParse(p)
            if success then
                Some value
            else None
        | _ -> None

    type System.String with
        static member IsValueString =
            function
            | ValueString _ -> true
            | _ -> false

module Xml =
    open System.Xml.Linq
    let nsNone = XNamespace.None
    let toXName (ns:XNamespace) name =
        ns + name

    let getElement1 n (e:XElement) =
        e.Element n
        |> Option.ofObj
    // leaving out a 'getElement' as it will likely be (samples in code comments below):
    //    let getElement = toXName nsNone >> getElement1
    //    let getElement = toXName doc.Root.Name.Namespace >> getElement1
    let getElements1 n (e:XElement) = e.Elements n
    // point-free Seq.filter argument
    let isNamed n (e:XElement) = e.Name = n
    let getElementsAfter n (e:XElement) =
        e
        |> getElements1 n
        |> Seq.skipWhile (isNamed n >> not)
        |> Seq.skip 1

    let getAttribVal name (e:XElement) =
        nsNone + name
        |> e.Attribute
        |> Option.ofObj
        |> Option.map (fun a -> a.Value)
    let setAttribVal name value (e:XElement) =
        e.SetAttributeValue(nsNone + name, value)

    let getDescendants n (e:XElement) = e.Descendants n

    let attribValueIs name value e =
        e
        |> getAttribVal name
        |> Option.toObj
        |> (=) value
    let isElement (e:XNode) =
        match e with
        | :? XElement -> true
        | _ -> false

    // when you |> string an XElement, normally it writes appends the namespace as an attribute, but this is normally covered by the root element
    let stripNamespaces (e:XElement):XElement=
        // if the node is not XElement, pass through
        let rec stripNamespaces (n:XNode): XNode =
            match n with
            | :? XElement as x ->
                let contents =
                    x.Attributes()
                    // strips default namespace, but not other declared namespaces
                    |> Seq.filter(fun xa -> xa.Name.LocalName <> "xmlns")
                    |> Seq.cast<obj>
                    |> List.ofSeq
                    |> (@) (
                        x.Nodes()
                        |> Seq.map stripNamespaces
                        |> Seq.cast<obj>
                        |> List.ofSeq
                    )
                XElement(nsNone + x.Name.LocalName, contents |> Array.ofList) :> XNode
            | x -> x
        stripNamespaces e :?> XElement


//        e.nodes
//        XElement(nsNone + e.Name.LocalName, content = e.Nodes)
    ()

module Debug =
    open System.Collections.ObjectModel

    type FListener(fWrite: _ -> unit,fWriteLn:_ -> unit, name) =
        inherit TraceListener(name)
        override __.Write (msg:string)  = fWrite msg
        override __.WriteLine (msg:string)  = fWriteLn msg
        new(fWrite,fWriteLn) = new FListener(fWrite,fWriteLn, null)

    type FLineListener(source:string ObservableCollection, fLineMap) =
        inherit TraceListener()
        let mutable lastWasWriteNotWriteLine = false
        let fLineMap = defaultArg fLineMap id
        let addText msg isLineFinished =
            if lastWasWriteNotWriteLine then
                let lastLine = source.[source.Count - 1]
                assert (source.Remove lastLine)
                lastLine + msg
            else msg
            |> fun x -> if isLineFinished then fLineMap x else x
            |> source.Add
        new(source, lineMap:Func<_, _>) = new FLineListener(source,fLineMap = if isNull lineMap then None else Some lineMap.Invoke)

        override __.Write (msg:string) =
            addText msg false
            lastWasWriteNotWriteLine <- true

        override __.WriteLine (msg:string) =
            addText msg true
            lastWasWriteNotWriteLine <- false


    type DebugTraceListener(?breakOnAll) =
        inherit TraceListener()
        let mutable breakOnAll:bool = defaultArg breakOnAll false
        override __.Write (_msg:string) = ()
        override __.WriteLine (msg:string) =
            let toIgnorePatterns = [
                @"BindingExpression path error: 'Title' property not found on 'object' ''String' \(HashCode=-[0-9]+\)'. BindingExpression:Path=Title; DataItem='String' \(HashCode=-[0-9]+\); target element is 'ContentPresenter' \(Name='Content'\); target property is 'ResourceKey' \(type 'String'\)"
            ]
            let regMatch p =
                let m = Text.RegularExpressions.Regex.Match(msg,p)
                if m.Success then
                    Some p
                else
                    None

            let matchedIgnorePattern = toIgnorePatterns |> Seq.choose regMatch |> Seq.tryHead
            match matchedIgnorePattern with
            | Some _ -> ()
            | None ->
                if breakOnAll && Debugger.IsAttached then
                    Debugger.Break()
                else ()

    type Listener(created:DateTime, name) =
        inherit TraceListener(name)

        new(created) = new Listener(created, null)

        override __.Write (msg:string) = printf "%s" msg
        override __.WriteLine (msg:string) =
            printfn "%s" msg
        member __.Created= created


    let inline assertIfDebugger b =
        if not b then
            printfn "Assertion failed"
            if Diagnostics.Debugger.IsAttached then
                Debugger.Break()
            // this option may be different from Debug.Assert somehow
            // https://docs.microsoft.com/en-us/dotnet/articles/fsharp/language-reference/assertions
//            else
//                assert b


type System.Action with
    static member invoke (x:System.Action) () = x.Invoke()
    static member invoke1 (x:System.Action<_>) y = x.Invoke(y)
    static member invoke2 (x:System.Action<_,_>) y z = x.Invoke(y,z)
    static member invoke3 (x:System.Action<_,_,_>) a b c = x.Invoke(a,b,c)

type System.Func<'tResult> with
    static member invoke (x:System.Func<'tResult>) () = x.Invoke()
    static member invoke1<'t> (x:System.Func<'t,'tResult>) y = x.Invoke y
    static member invoke2<'t1,'t2> (x:System.Func<'t1,'t2,'tResult>) y z = x.Invoke(y, z)
    static member invoke3 (x:System.Func<'t1,'t2,'t3,'tResult>) a b c = x.Invoke(a,b,c)
    static member invoke4 (x:System.Func<'t1, 't2, 't3, 't4, 'tResult>) a b c d = x.Invoke(a,b,c,d)

//module Array =
//    let ofOne x = [| x |]
module Seq =
    /// Seq.take throws if there are no items
    let takeLimit limit =
        let mutable count = 0
        Seq.takeWhile(fun _ ->
            let result = count < limit
            count <- count + 1
            result)
    let any items = items |> Seq.exists(fun _ -> true)
    let copyFrom (source: _ seq) (toPopulate:IList<_>)  =
        if not <| isNull source && not <| isNull toPopulate then
            use enumerator = source.GetEnumerator()
            while enumerator.MoveNext() do
                toPopulate.Add(enumerator.Current)
    let ofType<'t> items =
        items |> Seq.cast<obj> |> Seq.choose (fun x -> match x with | :? 't as x -> Some x | _ -> None )


module List =
//    is this worth having/keeping?
//    let except toScrape items =
//        let toScrape = Set.ofList toScrape
//        let items = Set.ofList items
//        items - toScrape

    // return a Tuple where (A, B) (both present if they have a match)
    let forceJoin b a =
        let b = Set.ofList b
        let a = Set.ofList a
        let x = Set.intersect a b
        let diffa = a - b
        let diffb = b - a
        diffa - x
        |> Seq.map (fun a' -> Some a', None)
        |> Seq.append (x |> Seq.map (fun x' -> (Some x', Some x')))
        |> Seq.append (diffb - x |> Seq.map (fun b' -> None, Some b'))
        |> List.ofSeq

module Observables =
    open System.Collections.ObjectModel
    open System.Collections.Specialized
    let bindObsTToObsObjDispatched (obsCollection:ObservableCollection<'t>) fDispatch =
        let obsObj = ObservableCollection<obj>()
        obsCollection.CollectionChanged.Add (fun e ->
            match e.Action with
            |NotifyCollectionChangedAction.Add ->
                fDispatch (fun () ->
                    e.NewItems
                    |> Seq.cast<obj>
                    |> Seq.iter obsObj.Add
                )
            |NotifyCollectionChangedAction.Move ->
                fDispatch (fun () ->
                    let oldIndex = e.OldStartingIndex
                    let newIndex = e.NewStartingIndex
                    obsObj.Move(oldIndex,newIndex)
                )

            |NotifyCollectionChangedAction.Remove ->
                fDispatch (fun () ->
                    e.OldItems
                    |> Seq.cast<obj>
                    |> Seq.iter (obsObj.Remove>> ignore<bool>)
                )
            |NotifyCollectionChangedAction.Replace ->
                fDispatch (fun () ->
                    e.NewItems
                    |> Seq.cast<obj>
                    |> Seq.zip (e.OldItems |> Seq.cast<obj>)
                    |> Seq.iteri(fun i (oldItem,newItem) ->
                        assert (obsObj.[e.OldStartingIndex + i] = oldItem)
                        obsObj.[e.OldStartingIndex + i] <- newItem
                    )
                )
            | NotifyCollectionChangedAction.Reset ->
                fDispatch (fun () ->
                    obsObj.Clear()
                    if not <| isNull e.NewItems then
                        e.NewItems
                        |> Seq.cast<obj>
                        |> Seq.iter obsObj.Add
                )
            | x -> failwithf "Case %A is unimplemented" x

        )
        obsObj
    let bindObsTToObsObj (obsCollection:ObservableCollection<'t>) =
        bindObsTToObsObjDispatched obsCollection (fun f -> f())
// |Null|Value| already in use by Nullable active pattern

type System.Convert with
    static member ToGuid(o:obj) = o :?> Guid
    static member ToBinaryData(o:obj) = o :?> byte[] // http://stackoverflow.com/a/5371281/57883


// based on http://stackoverflow.com/questions/15115050/f-type-constraints-on-enums
type System.Enum with // I think enum<int> is the only allowed enum-ish constraint allowed in all of .net
    static member parse<'t when 't : enum<int>> x = Enum.Parse(typeof<'t>,x)
    static member parseT t x = Enum.Parse(t, x)
    static member fromString<'t when 't:enum<int>> x = Enum.parse<'t> x :?> 't
    static member getName<'t when 't : enum<int>> x = Enum.GetName(typeof<'t>,x)
    static member getAll<'t when 't : enum<int>>() =
        Enum.GetValues typeof<'t>
        |> Seq.cast<int>
        |> Seq.map (fun x -> Enum.getName<'t> x)
        |> Seq.map (fun x -> Enum.parse<'t> x :?> 't)
    static member fromInt<'t when 't :enum<int>>(i:int) =
        Enum.getName<'t> i
        |> fun x -> Enum.parse<'t> x :?> 't


type System.DateTime with
    static member addDays dy (dt:DateTime) = dt.AddDays dy
    static member addHours h (dt:DateTime) = dt.AddHours h
    static member addMinutes min (dt:DateTime) = dt.AddMinutes min
    static member toShortDateString (dt:DateTime) = dt.ToShortDateString()
    static member getYear (dt:DateTime) = dt.Year
    static member getHour (dt:DateTime) = dt.Hour
    static member getMinute (dt:DateTime) = dt.Minute
    static member roundTo useRoundUp (ts:TimeSpan) (dt:DateTime) =
        if useRoundUp then
            ts.Ticks - 1L
        else
            ts.Ticks / 2L
        |> flip (+) dt.Ticks
        |> flip (/) ts.Ticks
        |> (*) ts.Ticks
        |> DateTime
    // taken from SO http://stackoverflow.com/a/1595311/57883
    static member getAge  (now:DateTime) (dt:DateTime) =
        let age = now.Year - dt.Year
        if (now.Month < dt.Month || (now.Month = dt.Month && now.Day < dt.Day)) then
            age - 1
        else
            age
    static member toString (format:string) (dt:DateTime) = dt.ToString(format)

    //public static string CalculateAge(DateTime birthDate, DateTime now)
    static member getAgeDisplay (now:DateTime) (dob:DateTime) =
        let years = DateTime.getAge now dob
        let _days,now =
            let days = now.Day - dob.Day
            if days < 0 then
                let newNow = now.AddMonths(-1)
                let totalDays = now - newNow
                let totalDays = int totalDays.TotalDays
                days + totalDays,newNow
            else days,now

        let months = ((now.Year - dob.Year) * 12) + now.Month - dob.Month

        if (years <= 2) then
            months.ToString() + "m"
        else
            years.ToString() + "y"

    member x.GetAge (now:DateTime) = DateTime.getAge now x

module Time =
    [<CustomComparison>]
    [<CustomEquality>]
    // shadowed constructor/private implementation
    type ValidatedTime = private {_Hour:int; _Minute:int;} with
            static member op_LessThan (x:ValidatedTime,y:ValidatedTime) = x.Hour < y.Hour || (x.Hour = y.Hour && x.Minute < y.Minute)
            static member op_GreaterThan (x:ValidatedTime, y:ValidatedTime) = x.Hour > y.Hour || (x.Hour = y.Hour && x.Minute > y.Minute)
            static member op_GreaterThanOrEqual (x:ValidatedTime, y:ValidatedTime) = x.Hour > y.Hour || (x.Hour = y.Hour && x.Minute >= y.Minute)
            static member op_LessThanOrEqual (x:ValidatedTime,y:ValidatedTime) = x.Hour < y.Hour || (x.Hour = y.Hour && x.Minute <= y.Minute)
            static member CanCreate hour minute = hour < 24 && hour >= 0 && minute >=0 && minute < 60
            static member Create hour minute = if ValidatedTime.CanCreate hour minute then {_Hour=hour; _Minute = minute} |> Some else None
            // exposing any members is a questionable decision for a Pure ADT, but... maybe this is ok for the way I'm using it
            member x.Hour = x._Hour
            member x.Minute = x._Minute
            override x.ToString() =
                DateTime.Today
                |> DateTime.addHours (float x.Hour)
                |> DateTime.addMinutes (float x.Minute)
                |> DateTime.toString "hh:mmtt"
            override x.GetHashCode() = (x.Hour,x.Minute).GetHashCode()
            override x.Equals obj =
                match obj with
                | :? ValidatedTime as y ->
                    x.Hour = y.Hour && x.Minute = y.Minute
                | _ -> false
            interface IComparable with
                member x.CompareTo (obj:obj)=
                    match obj with
                    | :? ValidatedTime as y ->
                       if ValidatedTime.op_LessThan (x, y) then
                           -1
                        elif ValidatedTime.op_GreaterThan (x, y) then
                            1
                        else
                            0
                    | _ -> raise <| InvalidOperationException("Type must be ValidatedTime")

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module ValidatedTime = //| ValidatedTime of hour:int * minute:int
            let create hour minute = if ValidatedTime.CanCreate hour minute then {_Hour = hour; _Minute=minute} |> Some else None
            let ofDateTime (dt:DateTime) = ValidatedTime.Create dt.Hour dt.Minute
            let ofTimeSpan (ts:TimeSpan) = ValidatedTime.Create ts.Hours ts.Minutes
            let getHour (vt: ValidatedTime) = vt.Hour
            let getMinute (vt:ValidatedTime) = vt.Minute
//    // shadow constructor
//    let ValidatedTime hour minute = ValidatedTime.Create hour minute

    // where only the hour component and below are relevant
    // a timespan of
    type IntraDayTimeSpan = |IntraDayTimeSpan of start:ValidatedTime*stop:ValidatedTime with
        member x.Start = x |> function |IntraDayTimeSpan(start,_) -> start
        member x.Stop = x |> function |IntraDayTimeSpan(_,stop) -> stop
        member x.Contains (t:ValidatedTime) =
            x.Start < t && t < x.Stop
        member x.Overlaps (y:IntraDayTimeSpan) =
            x.Contains y.Start || x.Contains y.Stop || y.Contains x.Start || y.Contains x.Stop

    let IntraDayTimeSpan start stop =
        if start < stop then
            IntraDayTimeSpan(start,stop) |> Some
        else None

type System.TimeSpan with
    static member getTicks (x:TimeSpan) = x.Ticks
    static member toString (s:string) (x:TimeSpan) = x.ToString(s)


[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>] // for C# access
module DateTime =
    let getAgeDisplay now dob = DateTime.getAgeDisplay now dob

// Railway Oriented Programming
type Rail<'tSuccess,'tFailure> =
    |Happy of 'tSuccess
    |Unhappy of 'tFailure

[<RequireQualifiedAccess>]
module Railway =

    /// apply either a success function or a failure function
    let inline either happyFunc unhappyFunc twoTrackInput =
        match twoTrackInput with
        |Happy s -> happyFunc s
        |Unhappy u -> unhappyFunc u

    /// convert a one-track function into a switch
    let inline switch f = f >> Happy

    /// convert a switch function into a two-track function
    let inline bind f = either f Unhappy

    // convert a one-track function into a two-track function
    let inline map f = //why isn't this simply "bind (f >> Happy)" ?
        either (f >> Happy) Unhappy

    /// bind a function to the failure track
    /// primary design purpose: adding data to the failure track
    let inline bind' f = either (Happy) f

    let toHappyOption =
        function
        | Happy s -> s |> Some
        | _ -> None

    /// An adapter that takes a normal one-track function and turns it into a switch function, and also catches exceptions
    /// could use id instead of a full exnHandler function for cases you just want the exception
    let inline tryCatch f exnHandler x =
        try
            f x |> Happy
        with ex -> exnHandler ex |> Unhappy

module Rop =

    type Error = {Property : string; Message : string}
    type Result<'a> =
        | Success of 'a
        | Fail of Error

    let bind f x =
        match x with Success x -> f x |Fail err -> Fail err

    let bind' f1 f2 x =
        match f1 x with
        | Success x -> f2 x
        | Fail err -> Fail err

    // let inline (>>=) f1 f2 = bind' f1 f2
    let overrideFail default' r = match r with |Success x -> x | Fail(_) -> default'
    let overrideFail' f r = match r with |Success x -> x | Fail(_) -> f()

//http://stackoverflow.com/a/8227943/57883
let lock (lockobj:obj) f =
  System.Threading.Monitor.Enter lockobj
  try
    f()
  finally
    System.Threading.Monitor.Exit lockobj

let buildCmdString fs arg i :string*string*obj =
    let applied = sprintf fs arg
    let replacement = (sprintf"{%i}" i)
    let replace target = StringHelpers.replace target replacement
    let replaced =
        fs.Value
        |> replace "'%s'"
        |> replace "'%i'"
        |> replace "'%d'"
    applied,replaced, upcast arg


let inline SetAndNotify eq setter notifier=
    if eq() then false
    else
        setter()
        notifier()
        true
let inline SetAndNotifyEquality field value setter notifier =
    let eq () = EqualityComparer<'T>.Default.Equals(field, value)
    SetAndNotify eq setter notifier

let SetAndNotifyEqualityC (field:'a byref, value:'a, notifier:System.Action) =
    if EqualityComparer<'a>.Default.Equals(field,value) then
        false
    else
        field <- value
        notifier.Invoke()
        true

module Diagnostics =
    open StringHelpers
    open System.Diagnostics
    let tryAsyncCatch f =
        f
        |> Async.Catch
        |> Async.Ignore
        |> Async.Start

    let makeDatedFilename (dt:DateTime) =
        let dt = dt.ToString("yyyyMMdd")
        sprintf "DebugLog_%s.txt" dt

    let logToFile filename (dt:DateTime) topic attrs s =
        let pid,sessionId =
            try
                let proc = System.Diagnostics.Process.GetCurrentProcess()
                proc.Id, proc.SessionId

            with _ -> 0,0

        let attrs = [sprintf "dt=\"%A\"" dt;sprintf "pid=\"%i\"" pid; sprintf "sId=\"%i\"" sessionId]@attrs |> StringHelpers.delimit " "
        let topic = match topic with |Some t -> t |_ -> "Message"
        let msg = sprintf "<%s %s>%s</%s>%s" topic attrs s topic Environment.NewLine
        System.IO.File.AppendAllText(filename,msg)

    let logS topic attrs s =
        if not <| String.IsNullOrEmpty s then
            printfn "%s" s
            Debug.WriteLine s

        let dt = DateTime.Now
        let filename = makeDatedFilename(dt)
        let fileLog= logToFile filename dt topic attrs
        try
            fileLog s
        with |ex ->
            printfn "Exception attemping to log:%A" ex

    let LogS topic s =
        logS (isNullOrEmptyToOpt topic) [] s

    let inline addDataIfPresent (ex:#exn) (s:string) =
        if not <| isNull ex.Data && ex.Data.Keys.Count > 0 then
            let extractString (v:obj) =
                match v with 
                | :? string as s -> s
                | x -> sprintf "%A" x
                |> replace "\"" "\\\""
                |> sprintf "\"%s\""
            ex.Data.Keys
            |> Seq.cast<obj>
            |> Seq.map (fun k -> k, ex.Data.[k])
            |> Seq.map (Tuple2.mapBoth extractString)
            |> Seq.map (fun (k,v) -> sprintf "%s:%s" k v)
            |> delimit ","
            // escape double quotes
            |> sprintf "%s\r\n{data:{%s}}" s
        else
            s

    // this needs to account for ex.Data information
    let logObj topic sOpt (x:obj) = 
        match sOpt with 
        | Some s -> sprintf "%s:%A" s x
        | None -> sprintf "%A" x
        |> fun s ->
            match x with 
            | :? exn as x -> s |> addDataIfPresent x
            | _ -> s
        |> logS topic []

    // if desired, only add data if the key isn't already present
    let addDataMaybe k v (ex:#exn) =
        if not <| ex.Data.Contains k then
            ex.Data.Add (k, v)

    // purpose: in a catch clause, try to do f, swallow exceptions so the outer exception is still handled the way it was intended to
    // not using addDataMaybe in case the behavior is not desired
    let inline tryDataAdd (x:#exn) f =
        try
            f x.Data.Add
        with ex ->
            logObj (Some "error adding exception data") None ex

    let logExS topic s ex = logObj topic s ex 

    let BeginLogScope scopeName=
        let pid = Process.GetCurrentProcess().Id
        logS (Some scopeName) [ sprintf "pid=\"%i\"" pid ] ""
        { new System.IDisposable
            with member __.Dispose() = logS (Some scopeName) [] (sprintf "<%s/>" scopeName)
        }

    let BeginTimedLogScope scopeName=
        let pid = Process.GetCurrentProcess().Id
        let sw = Stopwatch.StartNew()
        logS (Some scopeName) [ sprintf "pid=\"%i\"" pid ] ""
        {
            new System.IDisposable
                with member __.Dispose() =
                        sw.Stop()
                        logS (Some scopeName) [] (sprintf "  <Elapsed>%A</Elapsed>" sw.ElapsedMilliseconds)
                        logS (Some scopeName) [] (sprintf "<%s/>" scopeName)
        }


module Option = // https://github.com/fsharp/fsharp/blob/master/src/fsharp/FSharp.Core/option.fs
    let bindf f = Option.bind f

    /// unsafe (Unchecked.defaultof<_>)
    let getValueOrDefault (n: 'a option) = match n with | Some x -> x | None -> Unchecked.defaultof<_>

    // the built-in exists, but the order here is more natural
    let getOrDefault (default': 'a) (n: 'a option) = match n with| Some x -> x | None -> default'
    let getOrDefault' (default': 'a Lazy) (n: 'a option) = match n with| Some x -> x | None -> default'.Force()

    // for types the compiler insists aren't nullable, but maybe C# is calling
    let ofUnsafeNonNullable x =
        match box x with
        | null -> None
        | _ -> Some x

    // primarily for C# / wpf where the framework/ui are the only ones not accounting for this
    let toUnsafeObj x =
        match x with
        | Some x -> box x
        | None -> null


module Reflection =
    open System.Reflection
    open Microsoft.FSharp.Reflection
    let rec compareProps goDeep asTypeOpt blacklist nameOpt expected actual =
        let doNotDescendTypes = [typeof<string>; typeof<DateTime>; typeof<Type>;]
        // for types we don't want to take a reference to, but should not be descended
        let doNotDescendTypeNames = ["System.Windows.Threading.Dispatcher";"System.Windows.DependencyObjectType";"System.Windows.Media.Transform"]
        let t = asTypeOpt |> Option.getOrDefault' (Lazy(Func<_>(fun () -> expected.GetType())))
        let props = t.GetProperties()
        let toWalk = props |> Seq.filter(fun p -> blacklist |> Seq.contains p.Name |> not && p.GetMethod.GetParameters().Length = 0)
        let blacklist = blacklist |> Seq.except (props |> Seq.map (fun p -> p.Name)) |> List.ofSeq
        seq{
            for p in toWalk do
                printfn "Getting property %s via type %s(%s)" p.Name t.Name t.FullName
                let valuesOpt =
                    try
                        Some (p.GetValue expected , p.GetValue actual)
                    with ex ->
                        ex.Data.Add("Type", t.Name)
                        ex.Data.Add("TypeFullName", t.FullName)
                        None
                match valuesOpt with
                | Some (expected,actual) ->
                    if goDeep
                            && not <| isNull expected
                            && not <| isNull actual
                            && doNotDescendTypes |> Seq.contains p.PropertyType |> not
                            && doNotDescendTypeNames |> Seq.contains p.PropertyType.FullName |> not
                        then
                        printfn "Going deep into %s via type %s(%s)" p.Name t.Name t.FullName
                        let bindingName = match nameOpt with | Some n -> sprintf "%s.%s" n p.Name | None -> p.Name
                        yield! compareProps true (Some p.PropertyType) blacklist (Some bindingName) expected actual
                    let bindingName = match nameOpt with | Some n -> sprintf "%s.%s" n p.Name | None -> p.Name
                    yield ((expected = actual), bindingName, p.PropertyType, sprintf "%A: expected %A, actual was %A" p.Name expected actual)
                | None -> ()
            }

    let rec typeMatch t (g:Type) =
        if t = typeof<obj> then
            None
        elif g.IsInterface then
            let ints = if t.IsInterface then [| t |] else t.GetInterfaces()
            ints |> Seq.tryPick (fun t -> if t.GetGenericTypeDefinition() = g then Some(t.GetGenericArguments()) else None)
        elif t.IsGenericType && t.GetGenericTypeDefinition() = g then
            t.GetGenericArguments() |> Some
        else typeMatch (t.BaseType) g

    /// for when you need to see if something matches and expected Generic Type Definition ( you don't know "'t" but don't care)
    /// Sample (tested good) usage:
    /// match list with
    /// | TypeDefOf (isType:List<_>) typeArgs -> sprintf "Yay matched1 : %A" typeArgs \r\n
    /// | _ -> "boo"
    /// Also works for some types:
    /// | TypeDefOf (null:List<_>) typeArgs -> sprintf "Yay matched: %A" typeArgs
    let (|TypeDef|_|) (_:'a) (value:obj) =
        let typeDef = typedefof<'a>
        if obj.ReferenceEquals(value, null) then
            None
        else
            let typ = value.GetType()
            if typ.Name = "RuntimeType" then failwithf "Invalid use of |TypeDef|"
//            let gtd = if typ.IsGenericType then typ.GetGenericTypeDefinition() |> Some else None
            if typ.IsGenericType && typ.GetGenericTypeDefinition() = typeDef then
                Some(typ.GetGenericArguments())
            else
                let typeArgs = typeMatch typ typeDef
                typeArgs

    // for when you don't have a value or you want a switch on an instance of Type
    // or you want to unbox as one of a number of possible types
    // do not use where `| :?` is appropriate
    let (|TypeOf|_|) (_:'a) t =
        if t = typeof<'a> then Some ()
        else
            //printfn "did not match %A to %A" typeof<'a> t
            None

    // instead of null in TypeOf or TypeDef matches for types that don't allow null
    let isType<'a> = Unchecked.defaultof<'a>

    let rec getMethod recurse name (t:Type) =
        seq {
            let m = t.GetMethod(name)
            if not <| isNull m then
                yield t,m
            if recurse then
                yield! t.GetInterfaces() |> Seq.collect (getMethod recurse name)
        }
    let rec getMethods recurse (t:Type) =
        seq {
            yield (t,t.GetMethods())
            if recurse then
                yield! t.GetInterfaces() |> Seq.collect (getMethods recurse)
        }

    // primarily for use hand-in-hand with the 'Nullish' active pattern
    //unhandled: _ Nullable option
    /// for boxed objects that may be 'Valueable`
    let rec getReflectionValueOpt (genTypeOpt:Type option) (typeOpt:Type option)  (o:obj) =
        match o,genTypeOpt, typeOpt with
        | null, _, _ -> None
        | _ , Some gt ,_  ->
            // based on http://stackoverflow.com/a/13367848/57883
            match gt.GetProperty "Value" with
            | null -> None
            | prop ->
                let v = prop.GetValue(o,null)
                Some v
        | _, _,Some t ->
            match t.IsGenericType with
            | true -> getReflectionValueOpt typeOpt (t.GetGenericTypeDefinition() |> Some) o
            | false -> Some o
        | _, _, None ->
            getReflectionValueOpt None (o.GetType() |> Some) o
    //method taken from http://stackoverflow.com/q/4604139/57883
    let methodSourceName (mi:MemberInfo) =
        mi.GetCustomAttributes(true)
        |> Array.tryPick
                (function
                    | :? CompilationSourceNameAttribute as csna -> Some(csna)
                    | _ -> None)
        |> (function | Some(csna) -> csna.SourceName | None -> mi.Name)

    module Assemblies =
        // http://stackoverflow.com/a/28319367/57883
        let getAssemblyFullPath (assembly:Assembly) =
            let codeBaseFailedAssert () = Debug.Assert(false, "CodeBase evaluation failed! - Using Location as fallback.")
            let fullPath =
                match assembly.CodeBase with
                | null -> codeBaseFailedAssert () ;assembly.Location
                | codeBasePseudoUrl ->
                    let filePrefix3 = @"file:///"
                    if codeBasePseudoUrl.StartsWith filePrefix3 then
                        let sPath = codeBasePseudoUrl.Substring filePrefix3.Length
                        let bsPath = sPath.Replace('/', '\\')
                        bsPath
                    else codeBaseFailedAssert () ;assembly.Location
            fullPath

open System.Linq.Expressions
open Microsoft.FSharp.Quotations.Patterns
// until we get the `nameof()` operator
module QuotationHelpers =
    open Reflection

    let rec getQuoteMemberName expr =
        match expr with
        |Call (_,mi,_) -> methodSourceName mi
        |Lambda (_,expr) -> getQuoteMemberName expr
        |Coerce(expr,_) -> getQuoteMemberName expr
        |PropertyGet(_,p,_) -> p.Name
        |FieldGet(_,fi) -> fi.Name
        |ValueWithName(_,_,n) -> n
        |_ -> failwithf "Method is not a call expression"

    let getQuoteMemberNameT<'t> (expr:Quotations.Expr<'t -> _>) =
        let expr = expr :> Quotations.Expr
        getQuoteMemberName expr

    let getTypeName<'t> =
        match <@ fun (_:'t) -> () @> with
        | Lambda(x,_expr) -> x.Type.Name
        | x -> failwithf "getTypeName failed for %A" x

// this is unused, and it's value is questionable
type Microsoft.FSharp.Core.Option<'t> with
    static member OfT (targetOptionType:Type) value =
        let someMethod = targetOptionType.GetMethod("Some")
        let wrappedValue = someMethod.Invoke(null, [| value |])
        wrappedValue

// can't believe there's nothing built-in for this
let (|NullableNull|NullableValue|) (x: _ Nullable) =
    if x.HasValue then NullableValue x.Value else NullableNull

// Nullish covers actual null, NullableNull, and None
let (|Nullish|NullableObj|SomeObj|GenericObj|NonNullObj|) (o:obj) =
    // consider including empty string in nullish?
    Debug.Assert(Nullable<int>() |> box |> isNull)
    Debug.Assert(None |> box |> isNull)
    match isNull o with
    | true -> Nullish
    | false ->
        let t = o |> getType
        // a more direct translation would have been t |> Nullable.GetUnderlyingType|> isNull |> not
        match t.IsGenericType with
        | false -> NonNullObj
        | true ->
            let genericType = t.GetGenericTypeDefinition()
            if genericType = typedefof<Nullable<_>> then
                NullableObj genericType
            elif genericType = typedefof<Option<_>> then
                SomeObj genericType
            else GenericObj genericType

// this may not be even remotely useful, you can just |> Option.ofNullable
module Nullable = //http://bugsquash.blogspot.com/2010/09/nullable-in-f.html also https://gist.github.com/mausch/571158
//    [<AutoOpen>]
//    module BReusable =
    let getValueOrDefault n = match n with NullableValue x -> x | NullableNull -> n.GetValueOrDefault()

    //let create x = System.Nullable x (* just use Nullable in and of itself, create is unnecessary. perhaps this is because of F# 4? *)
    let getOrDefault v n = match n with NullableValue x -> x | _ -> v
    let getOrElse (v: 'a Lazy) (n: 'a Nullable) = match n with NullableValue x -> x | _ -> match v with | Lazy v -> v

    let get (x: _ Nullable) = x.Value
    let fromOption = Option.toNullable
    let toOption = Option.ofNullable
    let bind f x =
        match x with
        | NullableNull -> Nullable()
        | NullableValue v -> f v
    let hasValue (x: _ Nullable) = x.HasValue
    let isNull (x: _ Nullable) = not x.HasValue
    let count (x: _ Nullable) = if x.HasValue then 1 else 0
    let fold f state x =
        match x with
        | NullableNull -> state
        | NullableValue v -> f state v
    let foldBack f x state =
        match x with
        | NullableNull -> state
        | NullableValue _ -> f x state
    let exists p x =
        match x with
        | NullableNull -> false
        | NullableValue _ -> p x
    let forall p x =
        match x with
        | NullableNull -> true
        | NullableValue _ -> p x
    let iter f x =
        match x with
        | NullableNull -> ()
        | NullableValue v -> f v
    let map f x =
        match x with
        | NullableNull -> Nullable()
        | NullableValue v -> Nullable(f v)
    let toArray x =
        match x with
        | NullableNull -> [||]
        | NullableValue v -> [| v |]
    let toList x =
        match x with
        | NullableNull -> []
        | NullableValue v -> [v]

    let liftNullable op (a: _ Nullable) (b: _ Nullable) =
        if a.HasValue && b.HasValue
            then Nullable(op a.Value b.Value)
            else Nullable()

    let mapBoolOp op a b =
        match a,b with
        | NullableValue x, NullableValue y -> op x y
        | _ -> false

    let bindf (n: _ Nullable) f ``default`` = if n.HasValue then f n.Value else ``default``

// things I'm not sure are a good idea but enable things that otherwise might not be possible
// things that create a buttload of complexity one place, to reduce boilerplate or lessen complexity elsewhere
module Ideas =
    // active pattern, based on http://stackoverflow.com/a/25243799/57883
    let (|As|) (p:'T) : 'U option =
        let p = p :> obj
        if p :? 'U then Some (p :?> 'U) else None

    ()

// encapsulate INPC such that, fields can hold INPC values
module Inpc =
    open System.ComponentModel
    let checkComparerOpt fComparerOpt oldValue newValue = 
        match fComparerOpt with
        | Some f -> 
            if f oldValue newValue then
                printfn "Value was the same, keeping value %A = %A" oldValue newValue
                None
            else 
                printfn "Value was different changing value! %A <> %A" oldValue newValue
                Some newValue
        | None -> Some newValue

            
    let triggerPropChanged (event:Event<PropertyChangedEventHandler,PropertyChangedEventArgs>) name () =
        event.Trigger(null, PropertyChangedEventArgs(name))
    // consider adding a param for comparison, so the inpc won't fire if they are equal
    // or the parent property exposure could do it?
    type InpcWrapper<'T> (fNotifier, defaultValue:'T, comparerOpt) =
        let mutable field = defaultValue
        // consider:
        //member x.UnsafeSet v = field <- v
        member val IsDebug = false with get,set
        member val DisplayName : string = null with get,set
        member x.Value
            with get() = field
            and set v =
                let oldValue = field
                let compareResult = checkComparerOpt comparerOpt field v
                if x.IsDebug then
                    printfn "InpcWrapper%s (hasComparer %b, comparerSaysChangeValue %b) from %A to %A"
                        (if not <| isNull x.DisplayName then " " + x.DisplayName else String.Empty)
                        (Option.isSome comparerOpt)
                        (Option.isSome compareResult)
                        oldValue v
                compareResult
                |> Option.iter(fun v ->
                    field <- v
                    fNotifier()
                    if x.IsDebug then
                        printfn "InpcWrapper%s (hasComparer %b, comparerSaysChangeValue %b) from %A to %A"
                            (if not <| isNull x.DisplayName then " " + x.DisplayName else String.Empty)
                            (Option.isSome comparerOpt)
                            (Option.isSome compareResult)
                            oldValue field
                )
        member __.Notify() = fNotifier()

    // because you can't use this when initializing a C# property to call notifyproperty changed, let the notifier be set later
    type InpcWrapperDeferredNotifier<'T>(defaultValue: 'T, comparerOpt) = 
        let mutable notifier: Action = Action(ignore)
        let mutable field = defaultValue
        member __.Notifier
            with set v = notifier <- v
        // consider:
        //member x.UnsafeSet v = field <- v
        member __.Value
            with get() = field
            and set v =
                checkComparerOpt comparerOpt field v
                |> Option.iter(fun v ->
                    field <- v
                    if not <| isNull notifier then
                        notifier.Invoke()
                )
        member __.Notify() = if not <| isNull notifier then notifier.Invoke()

    // instead of using a parent/base class: use this method!
    let createInpc event name comparer defaultValue = InpcWrapper(triggerPropChanged event name, defaultValue, comparer)
    //let CreateInpc 

    // sample class for the createInpc method above
    type InpcEventWrappedSample () =
        let propertyChanged = new Event<_, _>()
        let encapsulated = createInpc propertyChanged "Encapsulated" None false

        member __.Encapsulated
            with get() = encapsulated.Value
            and set v = if v <> encapsulated.Value then encapsulated.Value <- v

        interface INotifyPropertyChanged with
            [<CLIEvent>]
            member __.PropertyChanged = propertyChanged.Publish
        abstract member RaisePropertyChanged : string -> unit
        default x.RaisePropertyChanged(propertyName : string) = propertyChanged.Trigger(x, PropertyChangedEventArgs(propertyName))
        member __.PropertyChanged = propertyChanged

// is this even remotely useful, when you have quotation helpers above?
module ExpressionHelpers =
    open System.Reflection
    let maybeUnary (exp:Expression<_>) =
        match exp.Body with
        | :? UnaryExpression as uExpr -> uExpr.Operand
        | x -> x

    let inline getMember(expr:Expression<_>) =
        if expr = null then raise <| System.ArgumentNullException("expr")
        //if expr.Body :? MemberExpression = false then raise <| System.ArgumentException("The body must be a member expression")
        let memExpr = maybeUnary expr :?> MemberExpression
        if memExpr = null then raise  <| System.ArgumentException("The body must be a member expression")
        memExpr.Member

    let inline GetMemberName(expr:Expression<_>) =
        (getMember expr).Name

    let inline GetMemberTAct<'t> (expr:Expression<Action<'t>>) =
        getMember expr

    let inline GetMemberTF(expr:Expression<Func<_>>) =
        getMember expr

    let inline GetMemberTF2<'t> (expr:Expression<Func<'t,_>>) =
        getMember expr

    let getMethodOf (expr: Expression<_>) =
        let methExpr = expr.Body :?> MethodCallExpression
        methExpr.Method

    let PropertyInfoOf<'T> (expr : Expression<Func<'T,_>>) =
        let mem= getMember expr
        mem :?> PropertyInfo

    let FieldInfoOf<'T> (expr : Expression<Func<_>>) =
        let mem = getMember expr
        mem :?> FieldInfo