[<AutoOpen>]
module common

open System.Collections.Generic

module Bfs = 
    module Matrix = 
        type Adjacency<'a> = (int*int) -> 'a -> 'a[,] -> ((int*int)*'a) list
        module Adjacencies = 
            let A4 : Adjacency<_> = 
                fun (i,j) _ m ->
                    [
                        (i-1, j)
                        (i+1, j)
                        (i, j-1)
                        (i, j+1)
                    ] |> List.choose (fun (i',j') -> 
                        if (i' >= Array2D.base1 m 
                            && j' >= Array2D.base2 m 
                            && i' < Array2D.length1 m 
                            && j' < Array2D.length2 m)
                        then Some ((i',j'), m[i', j'])
                        else None
                    )
            let where condition (adj: Adjacency<'a>) : Adjacency<'a> =                 
                fun (i,j) value m ->
                    adj (i,j) value m
                    |> List.filter(fun (_, value') -> condition value value')

        type Target<'a> = (int*int) -> 'a -> bool
        module Targets =
            let value x : Target<_> 
                = fun _ x' -> x' = x
            let at i j : Target<_>
                = fun (i', j') _ -> i = i' && j = j'

        type Settings<'a> = { Adjacency: Adjacency<'a> }
        module Settings = 
            let common = 
                {
                    Adjacency = Adjacencies.A4
                }

        let rec private findPath' (matrix: 'a[,]) (target : Target<'a>) (visited: HashSet<int*int>) (q : Queue<((int*int)*'a) list>) (settings : Settings<'a>) =
            if (q.Count = 0)
            then None
            else 
                let ((i,j), value)::rest = q.Dequeue();
                if target (i,j) value then Some (((i,j),value)::rest)
                else 
                    let adjacent = settings.Adjacency (i,j) matrix[i,j] matrix

                    adjacent
                    |> Seq.filter (fst >> visited.Contains >> not)
                    |> Seq.iter (
                        fun ((i', j'), value') -> 
                            visited.Add (i',j') |> ignore
                            q.Enqueue (((i',j'),value')::((i,j),value)::rest)
                    )
                    findPath' matrix target visited q settings


        let findPath (settings : Settings<'a>) (matrix: 'a[,]) ((starti, startj): int*int) (target : Target<'a>) = 
            let visited = HashSet<int*int>()
            visited.Add (starti, startj) |> ignore
            let queue = Queue<((int*int)*'a) list>()
            queue.Enqueue [(starti, startj),matrix[starti,startj]]
            findPath' matrix target visited queue settings