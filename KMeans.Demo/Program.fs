namespace KMeans.Demo
open System
open System.IO
open KMeans

module Program =
    let getVectorsFromFile inFilepath = 
        use inFile = File.OpenText inFilepath
        [while not inFile.EndOfStream do 
            yield (inFile.ReadLine ()).Split ' ' |> Array.map double]

    [<EntryPoint>]
    let main argv =
        let inFilepath = argv.[0]
        let clusterCount = int argv.[1]
        let iterationsLimit = if argv.Length > 2 then int argv.[2] |> Some else None

        let vectors = getVectorsFromFile inFilepath
        let clusters = Clustering.apply (vectors, clusterCount, iterationsLimit)
        Plotter.showClusters (clusters, vectors) "K-Means Clustering"
        
        0