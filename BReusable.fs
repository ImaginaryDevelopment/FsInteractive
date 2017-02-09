[<AutoOpen>]
module BReusable

open System
[<AutoOpen>]
module MatchHelpers =
    // possible purpose: 'when clauses' require binding variable names, and if two cases should have the same result, but one has a condition on the bound variable, it can no longer point to the same exact path
    let (|IsTrue|_|) f x = if f x then Some() else None

[<AutoOpen>]
module FunctionalHelpersAuto =
    let teeTuple f x = x, f x
    let tee f x = f x; x
    let flip f x y = f y x
    let uncurry f (x,y) = f x y
    let inline getType x = x.GetType()

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

module StringHelpers =
    type System.String with
        static member subString i (x:string) = x.Substring(i)
        static member subString2 i e (x:string)= x.Substring(i,e)
        static member contains s (x:string) = x.Contains(s)
        static member defaultComparison = StringComparison.InvariantCultureIgnoreCase
        static member Null:string = null
        static member split1 (delims: string seq) stringSplitOptions (x:string) =  x.Split(delims |> Array.ofSeq, options= stringSplitOptions)
        static member split (delims:string seq) (x:string) = x |> String.split1 delims StringSplitOptions.None
        // favor non attached methods for commonly used methods

    let after (delimiter:string) (s:string) = s|> String.subString (s.IndexOf delimiter + delimiter.Length)
    let contains (delimiter:string) (x:string) = if isNull x then false elif isNull delimiter || delimiter = "" then failwithf "bad contains call" else x.IndexOf(delimiter, String.defaultComparison) >= 0
    let containsI (delimiter:string) (x:string) = if isNull x then false elif isNull delimiter || delimiter = "" then failwithf "bad contains call" else x.IndexOf(delimiter, String.defaultComparison) >= 0
    let containsAnyOf (delimiters:string seq) (x:string) = delimiters |> Seq.exists(flip contains x)
    let containsIAnyOf (delimiters:string seq) (x:string) = delimiters |> Seq.exists(flip containsI x)
    let delimit (delimiter:string) (items:#seq<string>) = String.Join(delimiter,items)
    let endsWith (delimiter:string) (x:string) = x.EndsWith delimiter
    let isNumeric (s:string)= not <| isNull s && s.Length > 0 && s |> String.forall Char.IsNumber
    let replace (target:string) (replacement) (str:string) = str.Replace (target,replacement)
    let splitLines(x:string) = x.Split([| "\r\n";"\n"|], StringSplitOptions.None)
    let startsWith (delimiter:string) (s:string) = s.StartsWith delimiter
    let startsWithI (delimiter:string) (s:string) = s.StartsWith(delimiter,String.defaultComparison)
    let trim (s:string) = s.Trim()

    let before (delimiter:string) s = s|> String.subString2 0 (s.IndexOf delimiter)
    let afterOrSelf delimiter x = if x|> String.contains delimiter then x |> after delimiter else x
    let beforeOrSelf delimiter x = if x|> String.contains delimiter then x |> before delimiter else x
    let afterLast delimiter x =
        if x |> String.contains delimiter then failwithf "After last called with no match"
        x |> String.subString (x.LastIndexOf delimiter + delimiter.Length)
    let equalsI s1 (toMatch:string)= not <| isNull toMatch && toMatch.Equals(s1, StringComparison.InvariantCultureIgnoreCase)

    let toCamel s = // https://github.com/ayoung/Newtonsoft.Json/blob/master/Newtonsoft.Json/Utilities/StringUtils.cs
        if String.IsNullOrEmpty s then
            s
        elif not <| Char.IsUpper s.[0] then
            s
        else
            let ci = System.Globalization.CultureInfo.InvariantCulture
            let camelCase = Char.ToLower(s.[0], ci).ToString(ci)
            if (s.Length > 1) then
                camelCase + (s.Substring 1)
            else
                camelCase
    let (|NullString|Empty|WhiteSpace|ValueString|) (s:string) =
        match s with
        | null -> NullString
        | "" -> Empty
        | _ when String.IsNullOrWhiteSpace s -> WhiteSpace s
        | _ -> ValueString s

open StringHelpers

// I've also been struggling with the idea that Active patterns are frequently useful as just methods, so sometimes methods are duplicated as patterns
[<AutoOpen>]
module StringPatterns =
    let (|NullString|Empty|WhiteSpace|ValueString|) (s:string) =
        match s with
        | null -> NullString
        | "" -> Empty
        | _ when String.IsNullOrWhiteSpace s -> WhiteSpace
        | _ -> ValueString
    let (|StartsWith|_|) (str:string) arg = if str.StartsWith(arg) then Some() else None
    let (|StartsWithI|_|) s1 (toMatch:string) = if not <| isNull toMatch && toMatch.StartsWith(s1, StringComparison.InvariantCultureIgnoreCase) then Some () else None
    let (|StringEqualsI|_|) s1 (toMatch:string) = if equalsI toMatch s1 then Some() else None
    let (|InvariantEqualI|_|) (str:string) arg =
       if String.Compare(str, arg, StringComparison.InvariantCultureIgnoreCase) = 0
       then Some() else None
    let (|IsNumeric|_|) (s:string) = if not <| isNull s && s.Length > 0 && s |> String.forall Char.IsNumber then Some() else None

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
            | ValueString -> true
            | _ -> false

//    let (|StartsWithI|_|) (toMatch:string) (x:string) =
//        if not <| isNull x && not <| isNull toMatch && toMatch.Length > 0 && x.StartsWith(toMatch, StringComparison.InvariantCultureIgnoreCase) then
//            Some ()
//        else None
//    let (|StringEqualsI|_|) s1 (toMatch:string) = if stringEqualsI toMatch s1 then Some() else None
//    let (|IsNumeric|_|) (s:string) = if isNumeric s then Some() else None
//    let fooTest() =
//        "xys"
//        |> function
//            | IsTrue (containsI "xy") -> true
//            | _ -> false


#if LINQPAD
    let dumpt (title:string) x = x.Dump(title); x
#else
    let dumpt (title:string) x = printfn "%s:%A" title x; x
#endif
    let indent spacing (text:string) =
        if String.IsNullOrEmpty(text) then
            String.Empty
        else if trim text |> String.contains "\r\n" then
            "\r\n" +  text |> splitLines |> Seq.map (fun s -> spacing + s) |> delimit "\r\n"
        else text

module Map =
    let ofDictionary x =
        x :> _ seq
        |> Seq.map (|KeyValue|)
        |> Map.ofSeq

module PathHelpers=
    open System.IO
    let findNewest path =
        Directory.GetFiles path
        |> Seq.map File.GetLastWriteTime
        |> Seq.max

// Railway Oriented
type Rail<'TSuccess,'TFailure> =
    |Happy of 'TSuccess
    |Unhappy of 'TFailure

[<RequireQualifiedAccess>]
module Railway =

    // legacy name: bind2
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
    let inline map f =
        bind (f >> Happy)

    let isHappy = function | Happy _ -> true | _ -> false
    /// bind a function to the failure track
    /// primary design purpose: adding data to the failure track
    let inline bind' f = either (Happy) f

    let ofOption failure xOpt = match xOpt with |Some x -> Happy x |None -> Unhappy failure

    /// An adapter that takes a normal one-track function and turns it into a switch function, and also catches exceptions
    /// could use id instead of a full exn function for cases you just want the exception
    let inline tryCatch f fEx x =
        try
            f x |> Happy
        with ex -> fEx ex |> Unhappy
//    let inline toHappyOption x = match x with | Happy x -> Some x | Unhappy _ -> None
    let toHappyOption = function | Happy s -> Some s |Unhappy _ -> None
    let toUnhappyOption = function | Happy _ -> None | Unhappy s -> Some s
    let forAllF fAll items =
        let items = items |> List.ofSeq
        if items |> Seq.forall fAll then
            items |> Seq.choose toHappyOption |> Happy
        else items |> Seq.choose toUnhappyOption |> Unhappy

module Railways =
    type Railway<'t,'tError> =
        | Success of 't
        | Failure of 'tError


module Seq =
    open System.Collections.Generic

    let any<'t> (items:'t seq) = items |> Seq.exists (fun _ -> true)
  /// Iterates over elements of the input sequence and groups adjacent elements.
  /// A new group is started when the specified predicate holds about the element
  /// of the sequence (and at the beginning of the iteration).
  ///
  /// For example:
  ///    Seq.groupWhen isOdd [3;3;2;4;1;2] = seq [[3]; [3; 2; 4]; [1; 2]]
    let groupWhen f (input:seq<_>) = seq {
        use en = input.GetEnumerator()
        let running = ref true

        // Generate a group starting with the current element. Stops generating
        // when it founds element such that 'f en.Current' is 'true'
        let rec group() =
          [ yield en.Current
            if en.MoveNext() then
              if not (f en.Current) then yield! group()
            else running := false ]

        if en.MoveNext() then
          // While there are still elements, start a new group
          while running.Value do
            yield group() |> Seq.ofList }

    let copyFrom (source: _ seq) (toPopulate:IList<_>)  =
        if not <| isNull source && not <| isNull toPopulate then
            use enumerator = source.GetEnumerator()
            while enumerator.MoveNext() do
                toPopulate.Add(enumerator.Current)

    /// assumes you will iterate the entire sequence, otherwise not disposed
    /// probably not ok for infinite sequences
    let ofIEnumerator (en:System.Collections.IEnumerator) =
        let unfolder () =
            if en.MoveNext() then
                Some(en.Current, ())
            else
                // sequence iterated, if it is disposable dispose it
                match en with
                | :? IDisposable as d -> d.Dispose()
                | _ -> ()
                None
        Seq.unfold unfolder ()

module Reflection =
    open System
    open System.Reflection
    open Microsoft.FSharp.Reflection
    // some parts of this may be a translation of BMore.linq
    let (|TypeDefOf|_|) (_:'a) t =
        if t = typedefof<'a> then Some() else None
    let (|TypeOf|_|) (_:'a) t =
        if t = typeof<'a> then Some ()
        else
            //printfn "did not match %A to %A" typeof<'a> t ;
            None

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

    // via http://www.fssnip.net/2V author: Tomas Petricek http://stackoverflow.com/users/33518/tomas-petricek
    // let us access public or private properties or methods dynamically
    // Various flags that specify what members can be called
    // NOTE: Remove 'BindingFlags.NonPublic' if you want a version
    // that can call only public methods of classes
    let staticFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Static
    let instanceFlags = BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance
    let private ctorFlags = instanceFlags
    let inline asMethodBase(a:#MethodBase) = a :> MethodBase

    // The operator takes just instance and a name. Depending on how it is used
    // it either calls method (when 'R is function) or accesses a property
    let (?) (o:obj) name : 'R =
      // The return type is a function, which means that we want to invoke a method
      if FSharpType.IsFunction(typeof<'R>) then

        // Get arguments (from a tuple) and their types
        let argType, resType = FSharpType.GetFunctionElements(typeof<'R>)
        // Construct an F# function as the result (and cast it to the
        // expected function type specified by 'R)
        FSharpValue.MakeFunction(typeof<'R>, fun args ->

          // We treat elements of a tuple passed as argument as a list of arguments
          // When the 'o' object is 'System.Type', we call static methods
          let methods, instance, args =
            let args =
              // If argument is unit, we treat it as no arguments,
              // if it is not a tuple, we create singleton array,
              // otherwise we get all elements of the tuple
              if argType = typeof<unit> then [| |]
              elif not(FSharpType.IsTuple(argType)) then [| args |]
              else FSharpValue.GetTupleFields(args)

            // Static member call (on value of type System.Type)?
            if (typeof<System.Type>).IsAssignableFrom(o.GetType()) then
              let methods = (unbox<Type> o).GetMethods(staticFlags) |> Array.map asMethodBase
              let ctors = (unbox<Type> o).GetConstructors(ctorFlags) |> Array.map asMethodBase
              Array.concat [ methods; ctors ], null, args
            else
              o.GetType().GetMethods(instanceFlags) |> Array.map asMethodBase, o, args

          // A simple overload resolution based on the name and the number of parameters only
          // TODO: This doesn't correctly handle multiple overloads with same parameter count
          let methods =
            [ for m in methods do
                if m.Name = name && m.GetParameters().Length = args.Length then yield m ]

          // If we find suitable method or constructor to call, do it!
          match methods with
          | [] -> failwithf "No method '%s' with %d arguments found" name args.Length
          | _::_::_ -> failwithf "Multiple methods '%s' with %d arguments found" name args.Length
          | [:? ConstructorInfo as c] -> c.Invoke(args)
          | [ m ] -> m.Invoke(instance, args) ) |> unbox<'R>

      else
        // The result type is not an F# function, so we're getting a property
        // When the 'o' object is 'System.Type', we access static properties
        let typ, flags, instance =
          if (typeof<System.Type>).IsAssignableFrom(o.GetType())
            then unbox o, staticFlags, null
            else o.GetType(), instanceFlags, o

        // Find a property that we can call and get the value
        let prop = typ.GetProperty(name, flags)
        if isNull prop && isNull instance then
          // The syntax can be also used to access nested types of a type
          let nested = typ.Assembly.GetType(typ.FullName + "+" + name)
          // Return nested type if we found one
          if isNull nested then
            failwithf "Property or nested type '%s' not found in '%s'." name typ.Name
          elif not ((typeof<'R>).IsAssignableFrom(typeof<System.Type>)) then
            let rname = (typeof<'R>.Name)
            failwithf "Cannot return nested type '%s' as a type '%s'." nested.Name rname
          else nested |> box |> unbox<'R>
        else
          // Call property and return result if we found some
          let meth = prop.GetGetMethod(true)
          if isNull prop then failwithf "Property '%s' found, but doesn't have 'get' method." name
          try meth.Invoke(instance, [| |]) |> unbox<'R>
          with _ -> failwithf "Failed to get value of '%s' property (of type '%s')" name typ.Name

module Assemblies =
    // http://stackoverflow.com/a/28319367/57883
    let getAssemblyFullPath (assembly:System.Reflection.Assembly) =
        let codeBaseFailedAssert () = System.Diagnostics.Debug.Assert(false, "CodeBase evaluation failed! - Using Location as fallback.")
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

module Option =
//    [<AutoOpen>]
    // Brandon
//    module BReusable =

    let getValueOrDefault (n: 'a option) = match n with | Some x -> x | None -> Unchecked.defaultof<_>
    let getOrDefault (default': 'a) (n: 'a option) = match n with| Some x -> x | None -> default'
    let getOrDefault' (default': 'a Lazy) (n: 'a option) = match n with| Some x -> x | None -> default'.Force()

    // for types the compiler insists aren't nullable, but maybe C# is calling
    let ofUnsafeNonNullable x =
        match box x with
        | null -> None
        | _ -> Some x
    (* End Brandon *)


let (|NullableNull|NullableValue|) (x: _ Nullable) =
    if x.HasValue then NullableValue x.Value else NullableNull

[<RequireQualifiedAccess>]
module Nullable = //http://bugsquash.blogspot.com/2010/09/nullable-in-f.html also https://gist.github.com/mausch/571158

    let getValueOrDefault n = match n with NullableValue x -> x | NullableNull -> n.GetValueOrDefault()

    //let create x = System.Nullable x (* just use Nullable in and of itself, create is unnecessary. perhaps this is because of F# 4? *)
    let getOrDefault v n = match n with NullableValue x -> x | _ -> v
    let getOrElse (v: 'a Lazy) (n: 'a Nullable) = match n with NullableValue x -> x | _ -> v.Force()

    let get (x: _ Nullable) = x.Value
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

