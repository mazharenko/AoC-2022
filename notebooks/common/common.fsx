[<AutoOpen>]
module common

let displayPipe x =
    x |> display |> ignore
    x

module Pattern1 =
    let read (f : string -> 'a) (data : string) = 
        data.Split([|"\n"; "\r"|], System.StringSplitOptions.RemoveEmptyEntries) 
        |> Array.map f

module Pattern2 = 
    let read (f : string -> 'a) (data : string) = 
        data.Split([|"\n\n"; "\r\n\r\n"|], System.StringSplitOptions.RemoveEmptyEntries) 
        |> Array.map f