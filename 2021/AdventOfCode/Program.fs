// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp


open System

open AdventOfCode

let solveDay01_1 (input: int []) =
  input
  |> Array.pairwise
  |> Array.filter (fun (first, second) -> second > first)
  |> Array.length

let solveDay01_2 (input: int []) =
  input
  |> Array.windowed 3
  |> Array.map Array.sum
  |> solveDay01_1

let solveDay02_1 (input: (string * int64) []) =
  input
  |> Seq.ofArray
  |> Seq.fold (fun (state: int64 * int64) step ->
      let vert, horizontal = state
      match step with
      | "forward", x -> (vert, horizontal + x)
      | "up", x      -> (vert - x, horizontal)
      | "down", x    -> (vert + x, horizontal)
      | _ -> failwith "should not happen"
    )
    (0L, 0L)
  |> fun (x, y) -> x * y

let solveDay02_2 (input: (string * int64) []) =
  input
  |> Seq.ofArray
  |> Seq.fold (fun (state: int64 * int64 * int64) step ->
      let vert, horizontal, aim = state
      match step with
      | "forward", x -> (vert + aim * x, horizontal + x, aim)
      | "up", x      -> (vert, horizontal, aim - x)
      | "down", x    -> (vert, horizontal, aim + x)
      | _ -> failwith "should not happen"
    )
    (0L, 0L, 0L)
  |> fun (x, y, _) -> x * y

[<EntryPoint>]
let main argv =
    let day02p1 = solveDay02_1 Input.Day02.input
    let day02p2 = solveDay02_2 Input.Day02.input

    printfn "Day 02 part 1: %A" day02p1
    printfn "Day 02 part 2: %A" day02p2

    0 // return an integer exit code