open Core

let file = Aoc2024.Helpers.read_lines_from_file "input/sample01.txt"
let splitString s = String.split_on_chars s ~on:[ ' ' ]
let lines = file |> List.map ~f:splitString

let convertLine line =
  line
  |> List.filter ~f:(fun str -> String.length str > 0)
  |> List.map ~f:int_of_string

let convertedLists = lines |> List.map ~f:convertLine

let left_sort =
  convertedLists
  |> List.map ~f:(fun l -> List.hd_exn l)
  |> List.sort ~compare:Poly.compare

let right_sort =
  convertedLists
  |> List.map ~f:(fun l -> List.last_exn l)
  |> List.sort ~compare:Poly.compare

let () =
  let part_a =
    List.zip_exn left_sort right_sort
    |> List.fold ~f:(fun acc (l, r) -> acc + abs (l - r)) ~init:0
  in
  [ part_a ] |> Aoc2024.Helpers.print_listof_ints

let () =
  let left =
    left_sort |> List.group ~break:( <> )
    |> List.map ~f:(fun keyList -> (List.hd_exn keyList, List.length keyList))
  in

  let right =
    right_sort |> List.group ~break:( <> )
    |> List.map ~f:(fun keyList -> (List.hd_exn keyList, List.length keyList))
  in

  let part_b =
    left
    |> List.map ~f:(fun (lk, lv) ->
           right
           |> List.map ~f:(fun (rk, rv) ->
                  if lk = rk then lv * (lk * rv) else 0))
    |> List.concat
    |> List.fold ~f:(fun acc sum -> acc + sum) ~init:0
  in

  [ part_b ] |> Aoc2024.Helpers.print_listof_ints
