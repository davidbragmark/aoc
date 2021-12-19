namespace AdventOfCode.Solution

open AdventOfCode
open System.Text.RegularExpressions

type Coord = int * int
type Coords = { c1: Coord; c2: Coord }

type HashMap = Map<Coord, int>

module Day05 =
  let parseNumber (split_char: string) (line: string) =
    Regex.Split(line, split_char)
    |> List.ofArray
    |> fun x -> printfn $"Split line: %A{x}"; x
    |> fun output ->
      let c1 = output.[0].Split "," |> fun x -> int x.[0], int x.[1]
      let c2 = output.[1].Split "," |> fun x -> int x.[0], int x.[1]
      { c1 = c1; c2 = c2}

  let parse (input: string) =
    let data =
      input.Split "\n"
      |> List.ofArray
      |> List.filter (System.String.IsNullOrWhiteSpace >> not)
      |> List.map (parseNumber " -> ")

    data

  let hashMapify (hashMap: HashMap) (coord: Coord) =
    match hashMap.TryFind coord with
    | Some value -> Map.change coord (fun _ -> Some(value + 1)) hashMap
    | None -> Map.add coord 1 hashMap

  let rec addToHashMap (hashMap: HashMap) (coords: Coord list) =
    match coords with
    | [] -> hashMap
    | [ x ] -> hashMapify hashMap x
    | x :: xs -> addToHashMap (hashMapify hashMap x) xs

  let createLines (coords: Coords) =
    let x1, y1 = coords.c1
    let x2, y2 = coords.c2

    let dir_x = if x1 < x2 then 1 else -1
    let dir_y = if y1 < y2 then 1 else -1

    //Part 1: Straight lines
    if x1 = x2 || y1 = y2 then
        [ for x in x1..dir_x..x2 do
            for y in y1..dir_y..y2 -> (x, y) ]
    else
    //Part 2: Diagonal lines
      List.zip
        [ for x in x1..dir_x..x2 -> x ]
        [ for y in y1..dir_y..y2 -> y ]

  let solve input =
    parse input
    |> List.map createLines
    |> List.fold (fun (state: HashMap) (item: Coord list) -> addToHashMap state item) Map.empty
    |> Map.toList
    |> List.filter (fun (key, value) -> value > 1)
    |> List.length
