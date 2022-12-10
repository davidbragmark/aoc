module RSet = Set.Make (Int)

type points = {
  p1: int;
  p2: int;
}

type ranges = {
  r1: RSet.t;
  r2: RSet.t;
}

let file_name = "bin/input04.txt"
let _ = Printf.sprintf "\nInput: %s\n" file_name
let input = Advent.read_lines file_name

let ( -- ) i j =
  let rec aux n acc =
    if n < i then
      acc
    else
      aux (n - 1) (n :: acc)
  in
  aux j []
;;

let map_points (line : string) =
  let nstring = String.split_on_char '-' line in
  {
    p1 = int_of_string (List.nth nstring 0);
    p2 = int_of_string (List.nth nstring 1);
  }
;;

let map_ranges p1 p2 =
  { r1 = p1.p1 -- p1.p2 |> RSet.of_list;
    r2 = p2.p1 -- p2.p2 |> RSet.of_list; }
;;

let exist_subset ranges =
  let r1 = ranges.r1 in
  let r2 = ranges.r2 in
  if RSet.cardinal r1 > RSet.cardinal r2 then
    RSet.subset r2 r1
  else
    RSet.subset r1 r2
;;

let exist_any ranges =
  let r1 = ranges.r1 in
  let r2 = ranges.r2 in
  let inter = RSet.inter r1 r2 in
  RSet.cardinal inter > 0
;;

let parse_input (input : string list) =
  input
  |> List.map (fun (line : string) -> String.split_on_char ',' line)
  |> List.map (fun (lines : string list) ->
       let first = List.nth lines 0 in
       let second = List.nth lines 1 in
       map_points first, map_points second)
  |> List.map (fun (point1, point2) -> map_ranges point1 point2)
;;

let part1 (input : string list) =
  input
  |> parse_input
  |> List.filter exist_subset
  |> List.length
  |> Printf.printf "\nPart 1: %d"
;;

let part2 (input : string list) =
  input
  |> parse_input
  |> List.filter exist_any
  |> List.length
  |> Printf.printf "\nPart 2: %d"
;;

let _ = input |> part1
let _ = input |> part2

