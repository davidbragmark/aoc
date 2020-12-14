// Parse: https://stackoverflow.com/questions/11766002/f-read-from-file-vs-download-string-and-split-by-newline
let readLines filePath = System.IO.File.ReadAllText(filePath)

let input  : string [] =
  readLines "input06.txt"
  |> fun x -> x.Split([|"\n\n"|], System.StringSplitOptions.RemoveEmptyEntries)

let taskA =
  input
  |> Seq.map
        (fun group -> Seq.distinct group |> Seq.filter (fun c -> System.Char.IsLetter c) |> Seq.length) |> Seq.sum

let taskB =
  input
  |> Seq.map
        (fun group -> group.Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)
                      |> Seq.map Set.ofSeq |> Set.intersectMany |> Seq.length) |> Seq.sum
