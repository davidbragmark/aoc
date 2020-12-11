
type Step = { x : int; y : int; count: int}

let readLines filePath = System.IO.File.ReadLines(filePath);;

let input =
  readLines "input03.txt"

let transform = function
  | '#' -> true
  | _ -> false

let parseLine line =
  Array.ofSeq line
  |> Array.map transform

let forest : bool [] list =
  input
  |> List.ofSeq
  |> List.map parseLine

let height = input |> Seq.length
let width = input |> Seq.head |> Seq.length

let rec step (xcord : int, ycord : int) (xs : int, ys : int) (input : bool [] list) trees =
  match input with
  | [] -> trees
  | row :: tail ->
    let tree = if (Array.get row xcord) then trees + 1L else trees
    let column : int = (xcord + xs) % width
    if (List.length tail >= ys) then
      step (column, ycord + ys) (xs, ys) (List.skip (ys - 1) tail) tree
    else tree

let taskA = step (0,0) (3,1) forest 0L

let stepsB = [(1,1); (3,1); (5,1); (7,1); (1,2)]

let taskB =
  stepsB
  |> List.map (fun slope -> step (0,0) slope forest 0L)
  |> List.reduce (*)
