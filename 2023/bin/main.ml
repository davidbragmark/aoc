open Core

let input = Aoc2023.Helpers.read_lines_from_file "input/input01.txt"

let numbers = [("one", "one1e"); ("two", "two2o"); ("three", "three3e"); ("four", "four4"); ("five", "five5e"); ("six", "six6"); ("seven", "seven7"); ("eight", "eight8t"); ("nine", "nine9e");]

let traverseString (str: string): string =
  numbers
  |> List.fold ~init:str ~f:(fun acc (pattern, num) ->
      String.substr_replace_all acc ~pattern:pattern ~with_:num
  )
;;

let mapToCharList (s: string) =
  s
  |> String.to_list
  |> List.map ~f:Char.to_int
  |> List.filter ~f:(fun c -> c < 97)
  |> List.filter_map ~f:Char.of_int
;;

let firstLastDigits (chars: char list) : int =
  let first = chars |> List.hd
  in
  let last = chars |> List.rev |> List.hd
  in
  match first,last with
  | None, None -> 0
  | None, Some l -> String.of_char l |> Int.of_string
  | Some f, None -> String.of_char f |> Int.of_string
  | Some f, Some l -> String.of_list [f; l] |> Int.of_string
;;

let part2 =
  input
  |> List.map ~f:traverseString
  |> List.map ~f:mapToCharList
  |> List.map ~f:firstLastDigits
  |> List.fold_left ~init:0 ~f:(+)

;;

let part1 =
  input
  |> List.map ~f:mapToCharList
  |> List.map ~f:firstLastDigits
  |> List.fold_left ~init:0 ~f:(+)
;;

let _ =
  let p1 = part1 in
    Fmt.(pf stdout "\tResult Part1: %d@." p1)
  ;;
  let p2 = part2 in
    Fmt.(pf stdout "\tResult Part2: %d@." p2)
  ;;

