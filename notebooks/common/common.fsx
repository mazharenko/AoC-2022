[<AutoOpen>]
module common

let displayPipe x =
    x |> display |> ignore
    x

let splitToTuple2 (separators : string array) (s : string) =
    let split = s.Split(separators, System.StringSplitOptions.RemoveEmptyEntries)
    split.[0], split.[1]

let (|BetweenInclusive|_|) min max x =
    if min <= x && x <= max then Some () else None

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None
    
module Pattern1 =
    let read (f : string -> 'a) (data : string) = 
        data.Split([|"\n"; "\r"|], System.StringSplitOptions.RemoveEmptyEntries) 
        |> Array.map f

module Pattern2 = 
    let read (f : string -> 'a) (data : string) = 
        data.Split([|"\n\n"; "\r\n\r\n"|], System.StringSplitOptions.RemoveEmptyEntries) 
        |> Array.map f

module Result =
    let failIfError =
        function 
        | Ok x -> x
        | Error error -> failwith (error.ToString())


module Array2D = 
    let toSeq (a:'a[,]) : seq<'a> =
        a |> Seq.cast<'a>
    let indexed (a:'a[,]) : (((int*int)*'a)[,]) = 
        Array2D.mapi (fun i j x -> (i,j),x) a

module Seq =
    open System.Collections.Generic
    let takeUntil predicate (s:seq<_>) = 
        let rec loop (en:IEnumerator<_>) = seq {
            if en.MoveNext() then
                yield en.Current
                if not (predicate en.Current) then
                    yield! loop en }
        seq { use en = s.GetEnumerator()
            yield! loop en }
