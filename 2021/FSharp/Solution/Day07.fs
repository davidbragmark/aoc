namespace AdventOfCode.Solution

open AdventOfCode
open System.Text.RegularExpressions

module Day07 =

  let parse (input: string) =
    input.Split ","
    |> Array.map int
    |> List.ofArray
    |> List.sort

  let calc_sum (l: int list) =
    let target = l[l.Length / 2]

    l
    |> List.fold (fun (fuelcount: int) (position: int) -> fuelcount + abs (position - target)) 0

  let part1 (input: string) = input |> parse |> calc_sum

  let cost (n: int) = (n * (n + 1)) / 2

  let calc_sum_2 (crabs: int list) =
    let low = List.min crabs
    let high = List.max crabs

    [ low..high ]
    |> List.map (fun target ->
      crabs
      |> List.map (fun crab_position -> cost (abs (target - crab_position)))
      |> List.sum)
    |> List.min

  let part2 (input: string) = input |> parse |> calc_sum_2
