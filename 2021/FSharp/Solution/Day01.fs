namespace AdventOfCode.Solution

module Day01 =
  let part1 (input: int []) =
    input
    |> Array.pairwise
    |> Array.filter (fun (first, second) -> second > first)
    |> Array.length

  let part2 (input: int []) =
    input
    |> Array.windowed 3
    |> Array.map Array.sum
    |> part1

