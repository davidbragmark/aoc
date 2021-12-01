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

[<EntryPoint>]
let main argv =
    let day01p1 = solveDay01_1 Input.Day01
    let day01p2 = solveDay01_2 Input.Day01
    //let aoc = solveDay01 testInput

    //printfn "Hello world %A" aoc
    printfn "Day 01 part 1: %A" day01p1
    printfn "Day 01 part 2: %A" day01p2

    0 // return an integer exit code