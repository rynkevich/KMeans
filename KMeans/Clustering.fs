module KMeans.Clustering
open System
open System.Collections.Generic

let private DefaultIterationsLimit = 20

let private getMeanOfCluster (vectors: double [] list, clusterVectors: ICollection<int>) =
    let divideBy y x = x / y
    let calculateMeanComponent dimension =
        [for clusterVector in clusterVectors -> vectors.[clusterVector].[dimension]]
        |> Seq.sum
        |> divideBy (double clusterVectors.Count)

    let dimensionsCount = vectors.Head.Length
    [|for dimension in 0..(dimensionsCount - 1) -> calculateMeanComponent dimension|]

let private formClusters (vectors: double [] list, centroids: double [] list) =
    let distance (a: double []) (b: double []) = 
        let sqr x = pown x 2
        seq {for dimension in 0..(a.Length - 1) -> a.[dimension] - b.[dimension] |> sqr} 
        |> Seq.sum 
        |> sqrt

    let newListWithElement element = 
        let list = new List<int> ()
        list.Add element
        list

    let clusters = new Dictionary<double [], List<int>> ()
    for index, vector in vectors |> Seq.indexed do
        let correspondingCentroid = centroids |> List.minBy (distance vector)
        match clusters.TryGetValue correspondingCentroid with
        | true, cluster -> cluster.Add index
        | _ -> clusters.[correspondingCentroid] <- newListWithElement index
    clusters

let private getRandomSublist (source: double [] list, sublistSize: int) =
    let random = new Random ()
    let rec nextRandomItemIndex (exclusionIndexList: ICollection<int>) =
        let index = random.Next (0, source.Length - 1)
        if exclusionIndexList.Contains index then
            nextRandomItemIndex exclusionIndexList
        else
            exclusionIndexList.Add index
            index

    let nthOfList list index =
        List.item index list

    let exclusionIndexList = new List<int> ()
    [for _ in 1..sublistSize -> (nextRandomItemIndex exclusionIndexList) |> nthOfList source]
    
let apply (vectors: double [] list, clusterCount: int, iterationsLimit: int option) =
    let rec getClusters centroids iterationsLimit =
        let clusters = formClusters (vectors, centroids)
        let meansOfClusters = [for KeyValue (_, clusterVectors) in clusters -> 
                                    getMeanOfCluster (vectors, clusterVectors)]
        if Set.ofList centroids = Set.ofList meansOfClusters || iterationsLimit = 0 then
            clusters
        else
            getClusters meansOfClusters (iterationsLimit - 1)

    let centroids = getRandomSublist (vectors, clusterCount)
    match iterationsLimit with
    | Some limit -> getClusters centroids limit
    | None -> getClusters centroids DefaultIterationsLimit