[<AutoOpen>]
module common

#nowarn "25"

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
    let dir (Point (x,  y)) = 
        Point (sign x, sign y)
    let x (Point (x, _)) = x
    let y (Point (_, y)) = y
    let mlen (Point (x1,y1)) (Point (x2, y2)) = 
        abs (x1 - x2) + abs (y1 - y2)

type Point3 = | Point3 of (int * int * int) with
    static member Zero = Point3(0, 0, 0)
    static member (+)(Point3 (x1 : int, y1 : int, z1 : int), Point3 (x2 : int, y2 : int, z2)) : Point3 = 
        let x = x1 + x2
        let y = y1 + y2
        let z = z1 + z2
        Point3 (x, y, z)
    static member (-)(Point3 (x1 : int, y1 : int, z1 : int), Point3 (x2 : int, y2 : int, z2)) : Point3 = 
        let x = x1 - x2
        let y = y1 - y2
        let z = z1 - z2
        Point3 (x, y, z)

module Point3 = 
    let dir (Point3 (x,  y, z)) = 
        Point3 (sign x, sign y, sign z)
    let x (Point3 (x, _, _)) = x
    let y (Point3 (_, y, _)) = y
    let z (Point3 (_, _, z)) = z
            
module Pattern1 =
    let read (f : string -> 'a) (data : string) = 
        data.Split([|"\n"; "\r"|], System.StringSplitOptions.RemoveEmptyEntries) 
        |> Array.map f

module Pattern2 = 
    let read (f : string -> 'a) (data : string) = 
        data.Split([|"\n\n"; "\r\n\r\n"|], System.StringSplitOptions.RemoveEmptyEntries) 
        |> Array.map f

module Result =
    let get =
        function 
        | Ok x -> x
        | Error error -> failwith (error.ToString())


module Array2D = 
    let toSeq (a:'a[,]) : seq<'a> =
        a |> Seq.cast<'a>
    let indexed (a:'a[,]) : (((int*int)*'a)[,]) = 
        Array2D.mapi (fun i j x -> (i,j),x) a
    let transpose (a:'a[,]) = 
        Array2D.initBased
            (Array2D.base2 a)
            (Array2D.base1 a)
            (Array2D.length2 a)
            (Array2D.length1 a)
            (fun i j -> a[j, i])
    let tryGet i j source =
        if (i >= Array2D.base1 source 
            && j >= Array2D.base2 source 
            && i < Array2D.length1 source + Array2D.base1 source 
            && j < Array2D.length2 source + Array2D.base2 source)
        then Some source.[i,j]
        else None

module Array3D = 
    let toSeq (a:'a[,,]) : seq<'a> =
        a |> Seq.cast<'a>
    let tryGet source i j k  =
        if (i >= 0 && j >= 0 && k >= 0
            && i < Array3D.length1 source
            && j < Array3D.length2 source
            && k < Array3D.length3 source)
        then Some source[i,j,k]
        else None
    let atPoint (Point3(i,j,k)) source = 
        Array3D.get source i j k
    let tryAtPoint (Point3(i,j,k)) source = 
        tryGet source i j k

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
            
    let group keySelector valueSelector source = 
        source |> Seq.groupBy keySelector 
        |> Seq.map (fun (key, entries) -> key, valueSelector entries)


module Range = 

    type T = int * int

    let contains x ((from, to') : T) = from <= x && x <= to'

    let includes ((fromSuper, toSuper) : T) ((fromSub, toSub) : T) =
        fromSuper <= fromSub && toSub <= toSuper
        
    let overlap ((from1, to1) : T) ((from2, to2) : T) =
        (to1 < from2 || to2 < from1)
        |> not
        
    let start ((from, _) : T) = from
    let finish ((_, to') : T) = to'

    let isEmpty ((from, to') : T) =
        to' < from
    
    let len ((from, to') : T) = 
        if isEmpty (from, to') then 0
        else to' - from + 1

    let rec union r1 r2 = 
        if (start r1 > start r2)
        then
            union r2 r1
        else
            if start r2 <= finish r1 + 1
            then [ start r1, max (finish r1) (finish r2) ]
            else [ r1; r2 ]
    let rec unionAll (rs : T list) = 
        let rsSorted = rs |> Seq.sortBy start
        rsSorted
        |> Seq.fold (fun acc r -> 
            match acc with
            | [] -> [ r ]
            | last::rest -> (union last r |> List.rev)@ rest
        ) []