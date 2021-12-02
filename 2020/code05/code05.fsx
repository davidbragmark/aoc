let readLines filePath = System.IO.File.ReadLines(filePath);;

let data =
  readLines "input05.txt"
  |> Seq.toList

let parseChar char =
  match char with
    | 'B' -> '1'
    | 'F' -> '0'
    | 'R' -> '1'
    | 'L' -> '0'

let seatIds =
  data
  |> List.map (fun line -> line |> String.map parseChar)
  |> List.map (fun line -> (line.[0..6], line.[7..9]))
  |> List.map (fun (row, column) -> (System.Convert.ToInt32(row, 2), System.Convert.ToInt32(column, 2)))
  |> List.map (fun (row, column) -> row * 8 + column )
  |> List.sort

let taskA = seatIds |> List.max

let taskB=
  seatIds
  |> List.pairwise
  |> List.find (fun (a,b) -> a <> b-1)
  |> fun (a,b) -> (a+b)/2

printfn "This is input: %A" input
