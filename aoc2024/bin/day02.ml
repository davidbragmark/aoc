open Core

let file = Aoc2024.Helpers.read_lines_from_file "input/input02.txt"
let splitString s = String.split_on_chars s ~on:[ ' ' ]
let lines = file |> List.map ~f:splitString

let convertLine line =
  line
  |> List.filter ~f:(fun str -> String.length str > 0)
  |> List.map ~f:int_of_string

let convertedLists = lines |> List.map ~f:convertLine
let validLineStep x y = abs (x - y) >= 1 && abs (x - y) <= 3
let validLineInc x y = y > x
let validLineDec x y = y < x

let checkLine line : bool =
  let arr = line |> Array.of_list in
  let line_length = List.length line in
  let step_bool =
    arr
    |> Array.for_alli ~f:(fun index value ->
           if index = line_length - 1 then true
           else validLineStep value (Array.get arr (index + 1)))
  in
  let incr_bool =
    arr
    |> Array.for_alli ~f:(fun index value ->
           if index = line_length - 1 then true
           else validLineInc value (Array.get arr (index + 1)))
  in
  let decr_bool =
    arr
    |> Array.for_alli ~f:(fun index value ->
           if index = line_length - 1 then true
           else validLineDec value (Array.get arr (index + 1)))
  in

  step_bool && (decr_bool || incr_bool)

let sub_levels lst =
  let init = List.length lst in
  lst
  |> List.mapi ~f:(fun idx _ ->
         let first_part = List.take lst idx in
         let last = if idx = init - 1 then [] else List.drop lst (idx + 1) in
         List.append first_part last)

let any_sub_level_ok (sub_levels : int list list) : bool =
  sub_levels |> List.exists ~f:checkLine

let () =
  convertedLists |> List.hd_exn |> sub_levels
  |> List.iter ~f:Aoc2024.Helpers.print_listof_ints

let () =
  let part_a = convertedLists |> List.filter ~f:checkLine |> List.length in
  [ part_a ] |> Aoc2024.Helpers.print_listof_ints

let () =
  let part_b =
    convertedLists |> List.map ~f:sub_levels
    |> List.filter ~f:any_sub_level_ok
    |> List.length
  in
  [ part_b ] |> Aoc2024.Helpers.print_listof_ints
