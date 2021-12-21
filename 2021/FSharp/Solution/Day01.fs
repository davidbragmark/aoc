namespace AdventOfCode.Solution

module Day01 =
  let part1 (input: int []) =
    input
    |> List.ofArray
    |> List.pairwise
    |> List.fold (fun (state: int) (first, second) -> if second > first then state + 1 else state) 0

  let part2 (input: int []) =
    input
    |> Array.windowed 3
    |> Array.map Array.sum
    |> part1

