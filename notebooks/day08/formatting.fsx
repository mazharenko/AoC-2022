[<AutoOpen>]
module formatting

open System.Drawing
open System.Text.Json

module private Array2D = 
    let toSeq (a:'a[,]) : seq<'a> =
        a |> Seq.cast<'a>

let private toPoints (source: Color[,]) =
    source
    |> Array2D.mapi (fun i j color -> 
        if color.IsEmpty || color.A = 0uy then None
        else 
            Some {|
                X = i
                Y = j
                Color = color
            |}
    )
    |> Array2D.toSeq
    |> Seq.choose id

let private template = 
    """
        <div id="[ID]"></id>
        <script>
            var render_[ID] = function() {
                const data = [DATA];
                DrawMatrix.render('[ID]', data, [SETTINGS])
            }
        </script>
        <script>
            if (typeof DrawMatrix !== 'object') {
                var script = document.createElement("script");
                script.setAttribute("src", "./drawMatrix.js");
                script.onload = function() {
                    render_[ID]();
                };
                document.getElementsByTagName("head")[0].appendChild(script);
            }
            else
                render_[ID]();
        </script>

    """

// type Settings(width : int) = 
//     // member val Width = width with get, set
//     member this.Width = width

type Settings = {Width: int}

type Displayable = { Matrices: Color[,] list; Settings: Settings }

let toDisplayableMany (f : 'a -> Color) (matrices : 'a[,] list) = 
    { Matrices = matrices |> List.map (Array2D.map f); Settings = { Width = 200 }}

let toDisplayable (f : 'a -> Color) (matrix : 'a[,])  = 
    toDisplayableMany f [ matrix ] 
 
let withSettings settings displayable  = 
    {displayable with Settings = settings}

let withMatrix (matrix : 'a[,]) (f : 'a -> Color) displayable = 
    {displayable with Matrices = (matrix |> Array2D.map f) :: displayable.Matrices}

let private formatMatrix displayable = 
    let guid = System.Guid.NewGuid().ToString("N")
    let matrices = displayable.Matrices

    let data = 
        matrices
        |> List.rev
        |> Seq.collect toPoints
        |> Seq.map (
            fun x -> 
            [|
                x.X :> obj
                x.Y
                $"rgba({x.Color.R}, {x.Color.G}, {x.Color.B}, {(float x.Color.A) / float 256})"
            |]
        )
    
    let size = displayable.Settings.Width / (matrices |> Seq.map Array2D.length2 |> Seq.max)
    let height = size * ((matrices |> Seq.map Array2D.length1 |> Seq.max))
    let jsSettings = 
        $"""{{
            width: {displayable.Settings.Width},
            height: {height},
            size: {size}
        }}"""

    template
        .Replace("[ID]", guid)
        .Replace("[DATA]", JsonSerializer.Serialize data)
        .Replace("[SETTINGS]", jsSettings)


Formatter.Register<Displayable>((fun d -> formatMatrix d), "text/html")
