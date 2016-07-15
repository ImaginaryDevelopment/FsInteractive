namespace BReusable
open System
[<AutoOpen>]
module StringHelpersAuto =
    type System.String with
        static member subString i (x:string) = x.Substring(i)
        static member subString2 i e (x:string)= x.Substring(i,e)
        static member contains s (x:string) = x.Contains(s)
        // favor non attached methods for overfly commonly used methods

//        member x.After (delimiter:string) = 
//            x.Substring(x.IndexOf(delimiter) + delimiter.Length)
//        member x.AfterOrSelf(delimiter) = 
//            if x.Contains delimiter then x.After delimiter else x
//        member x.AfterLast (delimiter:string) = 
//            if x.Contains delimiter = false then failwithf "After last called with no match"
//            x.Substring(x.LastIndexOf(delimiter) + delimiter.Length)
//        member x.Before (delimiter:string) = 
//            x.Substring(0, x.IndexOf(delimiter))
//let dumpt (t:string) x = x.Dump(t); x
    let delimit (delimiter:string) (items:#seq<string>) = String.Join(delimiter,items)
    let contains delimiter (x:string) = x.Contains delimiter
    let replace (target:string) (replacement) (str:string) = str.Replace (target,replacement)
    let after (delimiter:string) (s:string) = s|> String.subString (s.IndexOf delimiter + delimiter.Length)
    let before (delimiter:string) s = s|> String.subString2 0 (s.IndexOf delimiter)
    let afterOrSelf delimiter x = if x|> String.contains delimiter then x |> after delimiter else x
    let beforeOrSelf delimiter x = if x|> String.contains delimiter then x |> before delimiter else x
    let afterLast delimiter x = 
        if x |> String.contains delimiter then failwithf "After last called with no match"
        x |> String.subString (x.LastIndexOf delimiter + delimiter.Length)
    let stringEqualsI s1 (toMatch:string)= toMatch <> null && toMatch.Equals(s1, StringComparison.InvariantCultureIgnoreCase)
    let (|StartsWithI|_|) (toMatch:string) (x:string) = 
        if not <| isNull x && not <| isNull toMatch && toMatch.Length > 0 && x.StartsWith(toMatch, StringComparison.InvariantCultureIgnoreCase) then
            Some () 
        else None


module PathHelpers=
    open System.IO
    let findNewest path = 
        Directory.GetFiles path
        |> Seq.map File.GetLastWriteTime
        |> Seq.max


[<AutoOpen>]
module FunctionalHelpersAuto = 
    let teeTuple f x = x, f x
    let tee f x = f x; x

module Railways =
    type Railway<'t,'tError> =
        | Success of 't
        | Failure of 'tError
    /// rail -> one track function lifted to pretend it is two track
    let map f1to1 rx = 
        match rx with
        | Success s -> f1to1 s |> Success
        | Failure x -> Failure x
    /// rail -> one-in two out function -> outRail 
    let bind f1to2 x =
        match x with
        | Success s -> f1to2 s
        | Failure x -> Failure x
    /// rail-in to two different functions that have the same return type     
    let bind2 fSuccessToRail fFailure rx = 
        match rx with 
        | Success s -> fSuccessToRail s
        | Failure x -> fFailure x

    let ofOption failure xOpt = match xOpt with |Some x -> Success x |None -> Failure failure
//    let tryPick x fItems = 
//        fItems 
//        |> Seq.tryPick (fun fAttempt -> match fAttempt x with |Success result -> Some result | Failure _ -> None)
//        |> function | Some fResult -> Success fResult | None -> Failure [ "Could not find an item to pick"]
    let tryCatch f fEx x =
        try
            f x |> Success
        with ex -> fEx |> Failure
    /// rail-in -> map both the success and failure types to new rail (id on both should work)
    let map2 fSuccess1to1 fFailure1to1 rx =
        match rx with 
        | Success s -> fSuccess1to1 s |> Success
        | Failure x -> fFailure1to1 x |> Failure
        
    // exception paths must use the same types
    
    let switch f1 f2 = f1 >> bind f2
    let isSuccess = function | Success _ -> true | _ -> false
    let toSuccessOption = function | Success s -> Some s |Failure _ -> None
    let toFailureOption = function | Success _ -> None | Failure s -> Some s
    let forAllF fAll items = 
        let items = items |> List.ofSeq
        if items |> Seq.forall fAll then 
            items |> Seq.choose toSuccessOption |> Success
        else items |> Seq.choose toFailureOption |> Failure
    
module Seq =
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