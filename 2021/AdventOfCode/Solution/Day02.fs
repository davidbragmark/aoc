namespace AdventOfCode.Solution

module Day02 =
  let part1 (input: (string * int64) []) =
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

  let part2 (input: (string * int64) []) =
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
