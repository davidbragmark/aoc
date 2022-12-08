type hand = Scissor | Paper | Rock
type condition = Win | Lose | Draw

let parse_win_condition = function 'X' -> Lose | 'Y' -> Draw | _ -> Win

let parse_rock_paper_scissor = function
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | _ -> Scissor

let hand_score = function _, Rock -> 1 | _, Paper -> 2 | _, Scissor -> 3

let draw_battle ((opponent : hand), (you : condition)) =
  let draw =
    match you with
    | Win -> (
        match opponent with Rock -> Paper | Scissor -> Rock | Paper -> Scissor)
    | Draw -> opponent
    | Lose -> (
        match opponent with Rock -> Scissor | Scissor -> Paper | Paper -> Rock)
  in

  (opponent, draw)

let parse_battle (opponent, you) =
  match (opponent, you) with
  | Rock, Rock -> 3
  | Rock, Paper -> 6
  | Rock, Scissor -> 0
  | Paper, Rock -> 0
  | Paper, Paper -> 3
  | Paper, Scissor -> 6
  | Scissor, Rock -> 6
  | Scissor, Paper -> 0
  | Scissor, Scissor -> 3

let total_hand_score (opponent, you) =
  parse_battle (opponent, you) + hand_score (opponent, you)

let sum = List.fold_left ( + ) 0

let read_lines file =
  In_channel.with_open_text file In_channel.input_all
  |> Str.(split (regexp "\n"))

let parse_line line =
  line |> fun x -> (parse_rock_paper_scissor x.[0], parse_win_condition x.[2])

let solve_file (file_path : string) =
  Printf.printf "\nInput file: %s\n" file_path;

  file_path
  |> read_lines
  |> List.map parse_line
  |> List.map draw_battle
  |> List.map total_hand_score
  |> sum
  |> Printf.printf "%d\n"


let () = "bin/input.txt" |> solve_file
