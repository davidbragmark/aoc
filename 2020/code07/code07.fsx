#r "nuget: FSharpPlus"
open FSharpPlus
open System
open System.Text.RegularExpressions

let readLines filePath = System.IO.File.ReadLines(filePath);;

let input =
  readLines "input07.txt"
  |> List.ofSeq


type Bag = Map<string, (int*string) list>
let Bags = Map.empty

let convert =
  input
  |> List.map (String.trim ".")
  |> List.map (String.replace "no other bag" "")
  |> List.map (String.replace "bags" "")
  |> List.map (String.replace "bag" "")
  |> List.map (fun line -> line.Split([|", ";"contain"|], System.StringSplitOptions.RemoveEmptyEntries))
  |> List.map List.ofSeq
  |> List.map (fun group -> group |> List.map String.trimWhiteSpaces)

let parseNested (line : string list) =
  line
  |> List.map (fun s -> (int(s.[0..1]), s.[2..]));;


let insertToMap (line : string list) =
  let key = List.head line
  let value : (int * string) list =
    line
    |> List.skip 1
    |> List.filter (fun s -> s |> Array.ofSeq |> Array.head |> System.Char.IsDigit)
    |> parseNested

  (key, value)

let allBags =
  convert
  |> List.map insertToMap
  |> Map.ofList

//insertParents :: String -> [(Int, String)] -> Map.Map String [String] -> Map.Map String [String]
let rec insertParents (p : string) (bags : (int * string) list) (m : Map<string, string list>) =
  match bags with
    | [] -> m
    | (_, color) :: bags ->
        Map.unionWith color m (insertParents p bags m)

//parentMap :: Map.Map String [(Int, String)] -> Map.Map String [String]
//let parentMap containMap = Map.foldrWithKey insertParents Map.empty containMap

let applies bag (allBags : (string * (int * string) list)) =
  snd allBags |> List.map snd |> List.contains bag

let rec findBagsThatCanContain bag =
  let containerBags = allBags |> Map.toList |> List.filter (applies bag) |> List.map fst
  match containerBags with
  | [] -> []
  | bags -> bags @ (bags |> List.collect findBagsThatCanContain)

findBagsThatCanContain "shiny gold" |> Set.ofSeq |> Seq.length

//let taskA (allBags : Map<string, (int * string) list>) (search : string) =
//  let rec containsSearch (bags : (int * string) list) =
//    match bags with
//      | [] -> false
//      | (_, bag) :: bags ->
//        match bag = search with
//        | false -> false
//        | true -> printfn "Bags: %A" bags
//                  bags |> List.filter (fun (_, n) -> printfn "filtered: %s n"; containsSearch (allBags.Item n))
//                  true
//  allBags |> Map.filter (fun _ bags -> containsSearch bags) |> Map.count
