module KMeans.DataGenerator
open System.IO
open System

[<EntryPoint>]
let main argv =
    let elementCount = int argv.[0]
    let highBound = int argv.[1]
    let outFilepath = argv.[2]

    let random = new Random ()
    let elementArray = [for _ in 1..elementCount -> [|for _ in 0..1 -> random.Next (1, highBound)|]]

    use outFile = File.CreateText outFilepath
    for element in elementArray do
        fprintfn outFile "%d %d" element.[0] element.[1]
    0
