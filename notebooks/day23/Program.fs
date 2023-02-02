open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success then
        Some(List.tail [ for g in m.Groups -> g.Value ])
    else
        None


type Point = | Point of (int * int) with
    static member Zero = Point(0, 0)
    static member (+)(Point (x1 : int, y1 : int), Point (x2 : int, y2 : int)) : Point = 
        let x = x1 + x2
        let y = y1 + y2
        Point (x, y)
    static member (-)(Point (x1 : int, y1 : int), Point (x2 : int, y2 : int)) : Point = 
        let x = x1 - x2
        let y = y1 - y2
        Point (x, y)

module Point = 
    let sign (Point (x,  y)) = 
        Point (sign x, sign y)
    let x (Point (x, _)) = x
    let y (Point (_, y)) = y
    let mlen (Point (x1,y1)) (Point (x2, y2)) = 
        abs (x1 - x2) + abs (y1 - y2)
            
module Result =
    let get =
        function 
        | Ok x -> x
        | Error error -> failwith (error.ToString())

module Pattern1 =
    let read (f: string -> 'a) (data: string) =
        data.Split([| "\n"; "\r" |], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map f

module Pattern2 = 
    let read (f : string -> 'a) (data : string) = 
        data.Split([|"\n\n"; "\r\n\r\n"|], System.StringSplitOptions.RemoveEmptyEntries) 
        |> Array.map f

let raw = File.ReadAllText("../../../data")


module Array2D = 
    let toSeq (a:'a[,]) : seq<'a> =
        a |> Seq.cast<'a>
    let indexed (a:'a[,]) : (((int*int)*'a)[,]) = 
        Array2D.mapi (fun i j x -> (i,j),x) a
    let tryGet i j source =
        if (i >= Array2D.base1 source 
            && j >= Array2D.base2 source 
            && i < Array2D.length1 source 
            && j < Array2D.length2 source)
        then Some source.[i,j]
        else None
        
module Seq = 
    
    let group keySelector valueSelector source = 
        source |> Seq.groupBy keySelector 
        |> Seq.map (fun (key, entries) -> key, valueSelector entries)

open Farkle
open Farkle.Builder
open System
open Farkle.Builder.Regex

let parse s =
    Pattern1.read Seq.toArray s
    |> array2D
    |> Array2D.indexed
    |> Array2D.toSeq
    |> Seq.filter (snd >> (=)'#')
    |> Seq.map fst
    |> Seq.map Point
    |> Set.ofSeq
    
let elves = parse raw


let n elves elf =
    if (elves |> Set.contains (elf + Point (-1,-1))
       ||  elves |> Set.contains (elf + Point (-1,0))
       ||  elves |> Set.contains (elf + Point (-1,1)))
    then None
    else
        elf + Point (-1,0) |> Some

let s elves elf =
    if (elves |> Set.contains (elf + Point (1,-1))
       ||  elves |> Set.contains (elf + Point (1,0))
       ||  elves |> Set.contains (elf + Point (1,1)))
    then None
    else
        elf + Point (1,0) |> Some

let w elves elf =
    if (elves |> Set.contains (elf + Point (-1,-1))
       ||  elves |> Set.contains (elf + Point (0,-1))
       ||  elves |> Set.contains (elf + Point (1,-1)))
    then None
    else
        elf + Point (0,-1) |> Some
        
let e elves elf =
    if (elves |> Set.contains (elf + Point (-1,1))
       ||  elves |> Set.contains (elf + Point (0,1))
       ||  elves |> Set.contains (elf + Point (1,1)))
    then None
    else
        elf + Point (0,1) |> Some
        
let adjj a elves elf =
    let g = 
        [
            Point (1,0)
            Point (1,-1)
            Point (1,1)
            Point (0,1)
            Point (0,-1)
            Point (-1,0)
            Point (-1,1)
            Point (-1,-1)
        ] |> List.map ((+)elf)
    if (g|> List.exists (fun p -> Set.contains p  elves))
    then a elves elf
    else None
    
let adj = [adjj n; adjj s; adjj w; adjj e]


let round elves adjj =
    let candidate p =
        adjj |> List.choose (fun f -> f elves p) |> List.tryHead |> Option.defaultValue p
        
    let candidates = 
        elves
        |> Seq.group (candidate) Seq.toArray
        // |> Seq.group (adj[num % Array.length adj] elves) Seq.toArray
    
    let newElves = 
        candidates
        |> Seq.collect (fun (to',from) -> if (Array.length from = 1) then [|to'|] else from)
        |> Set.ofSeq
    newElves
    
    
let rec rounds elves num =
    [0..num - 1]
    |> List.scan (fun (elves1, adj1) n ->
        let newElves = round elves1 adj1
        let x::rest = adj1
        newElves, (rest @ [x])
    ) (elves, adj)
    |> List.map fst
    
let rounds1 elves =
    List.unfold (fun  (elves1, adj1) ->
         let newElves = round elves1 adj1
         if (newElves = elves1) then None
         else
              let x::rest = adj1
              Some(newElves, (newElves, (rest @ [x])))
    ) (elves, adj)
    
let jjj = rounds elves 10

let emptyTiles =
    let h = jjj |> List.last
    let xfrom = h |> Seq.map Point.x |> Seq.min 
    let xto = h |> Seq.map Point.x |> Seq.max 
    let yfrom = h |> Seq.map Point.y |> Seq.min 
    let yto = h |> Seq.map Point.y |> Seq.max
    
    (xto - xfrom + 1) * (yto - yfrom + 1) - h.Count 

let roundsUntil =
    (rounds1 elves |> List.length) + 1

printfn $"{roundsUntil}"
///////////////////
