open Core

let file = Aoc2024.Helpers.read_lines_from_file "input/input03.txt"

type token = Mul of int * int | Do | Dont

let parseToken str =
  match str with
  | "do()" -> Do
  | "don't()" -> Dont
  | mul ->
      let first, second =
        Scanf.sscanf mul "mul(%d,%d)" (fun n1 n2 -> (n1, n2))
      in
      Mul (first, second)

let part_1 lines =
  let pattern = Re.Pcre.regexp {|mul\((?<first>\d+),(?<second>\d+)\)|} in
  let concat_lines = lines |> String.concat in
  Re.matches ~pos:0 pattern concat_lines
  |> List.fold ~init:0 ~f:(fun acc next ->
         let first, second =
           Scanf.sscanf next "mul(%d,%d)" (fun n1 n2 -> (n1, n2))
         in
         acc + (first * second))

let part_2 lines =
  let pattern =
    Re.Pcre.regexp {|do\(\)|don't\(\)|mul\((?<first>\d+),(?<second>\d+)\)|}
  in
  let concat_lines = lines |> String.concat in
  Re.matches ~pos:0 pattern concat_lines
  |> List.map ~f:parseToken
  |> List.fold ~init:(true, 0) ~f:(fun (should, acc) token ->
         match token with
         | Do -> (true, acc)
         | Dont -> (false, acc)
         | Mul (f, s) ->
             if should then (should, acc + (f * s)) else (should, acc))
  |> snd

let () = file |> part_1 |> Aoc2024.Helpers.print_int
let () = file |> part_2 |> Aoc2024.Helpers.print_int
