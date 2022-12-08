let file_name = "bin/input03.txt"
let _ = Printf.printf "\nInput: %s\n" file_name
let input = Advent.read_lines file_name

(* https://ocaml.org/docs/sets *)
module CS = Set.Make (Char)

let char_set string = string |> String.to_seq |> CS.of_seq

let priority_value char =
  let char_value = int_of_char char in
  match char with
  | 'a' .. 'z' -> char_value - 96
  | _ -> char_value - 38
;;

(* Part 1 *)
(* let intersections_per_line (line : string) = *)
(*   let length = line |> String.length in *)
(*   let fst = String.sub line 0 (length / 2) |> char_set in *)
(*   let snd = String.sub line (length / 2) (length / 2) |> char_set in *)
(*   let intersection = CS.inter fst snd in *)
(*   intersection *)
(* ;; *)

(* Part 2 *)
let rec part2 acc (lines: string list) =
    let inter x y z = CS.inter (CS.inter (char_set x) (char_set y)) (char_set z) in
    let count x y z = (inter x y z) |> CS.choose |> priority_value in
    match lines with
    | [] -> acc
    | _ :: [] -> acc
    | _ :: _ :: [] -> acc
    | x :: y :: z :: [] -> acc + count x y z
    | x :: y :: z :: rest -> part2 (acc + count x y z) rest
;;

let _ =
  input
  |> part2 0
  |> Printf.printf "Part 2: %d\n"
;;
