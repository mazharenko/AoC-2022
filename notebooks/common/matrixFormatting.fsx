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
            (function () {
                if (typeof DrawMatrix === 'object') {
                    return;
                }

                DrawMatrix = {
                    render: function(containerId, data, settings) {
                        let container = document.getElementById(containerId);
                        container.innerHTML = "";
                        let canvas = document.createElement("canvas");
                        container.appendChild(canvas);

                        const size = settings.size;
                        canvas.width = settings.width;
                        canvas.height = settings.height;

                        var ctx = canvas.getContext("2d");
                        ctx.clearRect(0, 0, canvas.width, canvas.height);

                        for (point of data) {
                            const [i, j, color] = point;
                            ctx.fillStyle = color;
                            ctx.fillRect(size * j, size * i, size, size);
                        }
                    }
                }
            })();
        </script>
        <script>
            render_[ID]();
        </script>

    """

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
        .Replace("[SETTINGS]", jsSettings)
        .Replace("[DIR]", __SOURCE_DIRECTORY__)
        .Replace("[DATA]", JsonSerializer.Serialize data)


Formatter.Register<Displayable>((fun d -> formatMatrix d), "text/html")
