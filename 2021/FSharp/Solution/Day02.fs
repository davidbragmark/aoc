namespace AdventOfCode.Solution

type Direction =
 | Forward of int64
 | Up of int64
 | Down of int64

type State  = {
  vert: int64
  horizontal: int64
  aim: int64
}

module Day02 =
  let INITIAL_STATE = ({ vert = 0L; horizontal = 0L; aim = 0L})

  let direction (step: string * int64) =
    match step with
    | "forward", x -> Forward x
    | "up", x      -> Up x
    | "down", x    -> Down x
    | _ -> failwith "should not happen"

  let step_part1 (state: State) (step: Direction) =
    match step with
    | Forward x -> { state with horizontal = state.horizontal + x }
    | Up x      -> { state with vert = state.vert - x }
    | Down x    -> { state with vert = state.vert + x }

  let step_part2 (state: State) (step: Direction) =
    match step with
    | Forward x -> { state with vert = state.vert + state.aim * x; horizontal = state.horizontal + x }
    | Up x      -> { state with aim = state.aim - x }
    | Down x    -> { state with aim = state.aim + x }

  let solve (input: (string * int64) []) (step_function: State -> Direction -> State) =
    input
    |> Seq.ofArray
    |> Seq.map direction
    |> Seq.fold (fun (state: State) (step: Direction) -> step_function state step) INITIAL_STATE
    |> fun state -> state.vert * state.horizontal

  let part1 (input: (string * int64) []) =
    solve input step_part1

  let part2 (input: (string * int64) []) =
    solve input step_part2
