[<AutoOpen>]
module common

#nowarn "25"

open System.Collections.Generic

module Bfs = 

    [<AutoOpen>]
    module common = 
        type Path<'a> = 'a list
        type Result<'a> = 
            | Found of Path<'a>
            | NotFound of Path<'a> list
        
    module Custom = 
        type Adjacency<'a> = 'a -> 'a list
        
        type Target<'a> = 'a -> bool
        module Targets =
            let value x : Target<_> = (fun x' -> x = x')

        type Settings<'a,'key> = { VisitedKey: 'a -> 'key; }
        module Settings =
            let defaults : Settings<_,_> = {VisitedKey = id;}
        
        type Parameters<'a> = { Adjacency: Adjacency<'a> }

        let rec private findPath' 
                ((state::prevStates) : Path<'a> list) 
                (target : Target<'a>) 
                (visited: HashSet<'key>) 
                (q : Queue<'a list>)
                (settings : Settings<'a, 'key>)
                (parameters : Parameters<'a>) =
            if (q.Count = 0)
            then
                NotFound (state::prevStates)
            else 
                let current::rest = q.Dequeue();
                if target current
                then
                    Found (current::rest)
                else 
                    let adjacent = parameters.Adjacency current

                    adjacent
                    |> Seq.filter (fun a ->
                            let key = settings.VisitedKey a
                            not <| visited.Contains key
                        )
                    // |> Seq.filter (settings.VisitedKey >> visited.Contains >> not)
                    |> Seq.iter (
                        fun value' -> 
                            visited.Add (settings.VisitedKey value') |> ignore
                            q.Enqueue((value'::current::rest))
                    )
                    findPath' ((current::rest)::state::prevStates) target visited q settings parameters

        let findPath (settings : Settings<'a,'key>) (parameters : Parameters<'a>)  (start: 'a) (target : Target<'a>) = 
            let visited = HashSet<'key>()
            visited.Add (settings.VisitedKey start) |> ignore
            let queue = Queue<'a list>()
            queue.Enqueue([start])
            findPath' [[start]] target visited queue settings parameters
    
    module Matrix = 
        type State<'a> = { Coordinates: int*int; Value: 'a; Matrix: 'a[,] }

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

        type Parameters<'a> = { Adjacency: Adjacency<'a> }
        module Parameters = 
            let common = 
                {
                    Adjacency = Adjacencies.A4
                }

        let private createSettings<'a>() : Custom.Settings<State<'a>,int*int> =
            {
                VisitedKey = fun s -> s.Coordinates
            }

        let private convertParameters parametes : Custom.Parameters<State<_>> = 
            {
                Adjacency = 
                    fun state -> 
                        parametes.Adjacency state.Coordinates state.Value state.Matrix
                        |> List.map (fun (c, v) -> { Coordinates = c; Value = v; Matrix = state.Matrix})
            }

        let findPath (parameters : Parameters<'a>) (matrix: 'a[,]) ((starti,startj) as start: int*int) (target : Target<'a>) = 
            let customSettings = createSettings<'a>()
            let customParameters = convertParameters parameters
            let customTarget : Custom.Target<State<'a>> =
                fun state -> target state.Coordinates state.Value
            let result = Custom.findPath customSettings customParameters {Coordinates = start; Value = matrix[starti, startj]; Matrix = matrix} customTarget
            result
