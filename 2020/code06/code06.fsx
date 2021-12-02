// Parse: https://stackoverflow.com/questions/11766002/f-read-from-file-vs-download-string-and-split-by-newline
let readLines filePath = System.IO.File.ReadAllText(filePath)

let input  : string [] =
  readLines "input06.txt"
  |> fun x -> x.Split([|"\n\n"|], System.StringSplitOptions.RemoveEmptyEntries)

let union (group : string) =
  Seq.distinct group |> Seq.filter (fun c -> System.Char.IsLetter c) |> Seq.length

let intersection (group  : string) =
  group.Split([|'\n'|], System.StringSplitOptions.RemoveEmptyEntries)
  |> Seq.map Set.ofSeq |> Set.intersectMany |> Seq.length

let taskA =
  input
  |> Seq.map union
  |> Seq.sum

let taskB =
  input
  |> Seq.map intersection
  |> Seq.sum
