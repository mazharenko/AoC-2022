[<AutoOpen>]
module common

// todo to ipynb when the import feature is released?

module Rotation2 =
    type Rotation = private | T of int[,]
    with static member (*) (T(r1), T(r2)) =
            Array2D.init 2 2 (fun i j ->
                (r1[i,*], r2[*,j])
                ||> Seq.zip
                |> Seq.map (fun (x,y) -> x*y)
                |> Seq.sum
            ) |> T
        
    let id : Rotation =
        [
            [1; 0]
            [0; 1]
        ]
        |> array2D |> T
    let cw : Rotation =
        [
            [0; 1]
            [-1; 0]
        ] |> array2D |> T                        
    let ccw : Rotation =
        [
            [0; -1]
            [1; 0]
        ] |> array2D |> T
    
    let rotate (T(rotation):Rotation) (point:Point) =
        let x =
            rotation[0,0] * Point.x point
            + rotation[0,1] * Point.y point
        let y =
            rotation[1,0] * Point.x point
            + rotation[1,1] * Point.y point
        Point(x,y)        
        