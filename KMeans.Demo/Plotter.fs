module KMeans.Demo.Plotter
open System
open System.Collections.Generic
open XPlot.Plotly

let showClusters (clusters: Dictionary<double [], List<int>>, vectors: double [] list) (plotTitle: string) = 
    let random = new Random ()
    [for KeyValue (centroid, clusterVectors) in clusters do 
        let clusterColor = [for _ in 1..3 -> random.Next (50, 240) |> string] 
                            |> String.concat ", " 
                            |> sprintf "rgb(%s)"

        let clusterVectors = clusterVectors |> Seq.map(fun i -> vectors.[i])
        yield Scatter (
            x = (clusterVectors |> Seq.map (fun vector -> vector.[0])),
            y = (clusterVectors |> Seq.map (fun vector -> vector.[1])),
            mode = "markers",
            marker = Marker (
                color = clusterColor,
                size = 10
            )
        )
        
        yield Scatter (
            x = [centroid.[0]],
            y = [centroid.[1]],
            mode = "markers",
            marker = Marker (
                color = clusterColor,
                size = 15,
                symbol = "star",
                line = Line (color = "black", width = 1)
            )
        )]
    |> Chart.Plot
    |> Chart.WithLayout (Layout (title = plotTitle))
    |> Chart.WithWidth 1200
    |> Chart.WithHeight 700
    |> Chart.Show
    
    ()