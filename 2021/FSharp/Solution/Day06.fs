namespace AdventOfCode.Solution

open AdventOfCode
open System.Text.RegularExpressions

module Day06 =

  let initArray = [| for _ in 0..8 -> 0L |]

  let parse (input: string) =
    input.Split ","
    |> Array.map int

  let initData (arr: int64 array) (p: int array) =
    for i in p do
      arr[i] <- arr[i] + 1L
    arr

  let nextDay (days: int) (arr: int64 array) =
    for _ in 1..days do
      let newborn = arr[0]
      for i in 1..8 do
        arr[i-1] <- arr[i]
      arr[8] <- newborn
      arr[6] <- arr[6] + newborn
    arr

  let solve (days: int) (input: string) =
    input
    |> parse
    |> initData initArray
    |> nextDay days
    |> Array.sum

  let part1 input =
    solve 80 input

  let part2 input =
    solve 256 input
