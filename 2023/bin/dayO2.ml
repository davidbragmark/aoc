open Core

let input = Aoc2023.Helpers.read_lines_from_file "input/input02.txt"

type hand = Red of int | Green of int | Blue of int
type record = { red : int option; green : int option; blue : int option }

let map_to_record (hands : hand list) =
  List.fold hands ~init:{ red = None; green = None; blue = None }
    ~f:(fun acc hand ->
      match (acc, hand) with
      | { red = Some x; _ }, Red y when y > x -> { acc with red = Some y }
      | { green = Some x; _ }, Green y when y > x -> { acc with green = Some y }
      | { blue = Some x; _ }, Blue y when y > x -> { acc with blue = Some y }
      | { red = None; _ }, Red y -> { acc with red = Some y }
      | { green = None; _ }, Green y -> { acc with green = Some y }
      | { blue = None; _ }, Blue y -> { acc with blue = Some y }
      | record, _ -> record)

let multiply_max_cubes (record : record) =
  match (record.green, record.blue, record.red) with
  | Some g, Some b, Some r -> g * b * r
  | Some g, Some b, None -> g * b
  | Some g, None, Some r -> g * r
  | None, Some b, Some r -> b * r
  | Some g, None, None -> g
  | None, Some b, None -> b
  | None, None, Some r -> r
  | None, None, None -> 0

let parse_to_hand_type (num, color) =
  match (num, color) with
  | _num, "red" -> Red _num
  | _num, "green" -> Green _num
  | _num, _ -> Blue _num

let is_valid_hand (num, color) =
  match (num, color) with
  | _num, "red" when _num > 12 -> false
  | _num, "green" when _num > 13 -> false
  | _num, "blue" when _num > 14 -> false
  | _, _ -> true

let parse_value_and_color (str : string) : int * string =
  let split = String.split_on_chars (Stdlib.String.trim str) ~on:[ ' ' ] in
  let value, color = (Int.of_string (List.hd_exn split), List.last_exn split) in
  (value, color)

let parse_game_hand (str : string) =
  let all_hands = String.split_on_chars str ~on:[ ';' ] in
  let sub_hands =
    List.map all_hands ~f:(fun str ->
        String.split_on_chars (Stdlib.String.trim str) ~on:[ ',' ])
  in
  let hands =
    List.map sub_hands ~f:(fun sub -> List.map sub ~f:parse_value_and_color)
  in
  let valid =
    List.for_all hands ~f:(fun h -> List.for_all h ~f:is_valid_hand)
  in
  valid

let parse_game_hand_part2 (str : string) =
  let all_hands = String.split_on_chars str ~on:[ ';' ] in
  let sub_hands =
    List.map all_hands ~f:(fun str ->
        String.split_on_chars (Stdlib.String.trim str) ~on:[ ',' ])
  in
  let hands =
    List.map sub_hands ~f:(fun sub -> List.map sub ~f:parse_value_and_color)
  in
  hands
  |> List.map ~f:(fun hand -> List.map hand ~f:parse_to_hand_type)
  |> List.concat |> map_to_record |> multiply_max_cubes

let split_line (line : string) : string * string =
  let split = String.split_on_chars line ~on:[ ':' ] in
  let game, hand = (List.hd_exn split, List.last_exn split) in
  (game, hand)

let () =
  let parsed_lines = List.map input ~f:split_line in
  let unparsed_hands = List.map parsed_lines ~f:snd in
  let v =
    List.foldi unparsed_hands ~init:0 ~f:(fun idx acc value ->
        let invalid_game = parse_game_hand value in
        if invalid_game then acc + idx + 1 else acc)
  in
  Fmt.(pf stdout "\tPart1: %d@." v)

let () =
  let parsed_lines = List.map input ~f:split_line in
  let unparsed_hands = List.map parsed_lines ~f:snd in
  unparsed_hands
  |> List.map ~f:parse_game_hand_part2
  |> List.fold ~init:0 ~f:( + )
  |> Fmt.(pf stdout "\tPart2: %d@.")
