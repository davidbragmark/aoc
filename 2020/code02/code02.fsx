#r "nuget: FSharp.Text.RegexProvider"

open FSharp.Text.RegexProvider

let readLines filePath = System.IO.File.ReadLines(filePath)

let data = readLines "input02.txt" |> Seq.toList


type rx = Regex< @"(?<min>^\d+) (?<max>\d+) (?<character>\w) (?<pwd>\w+)$" >

type Line =
  { min: int
    max: int
    char: char
    pwd: string }

let parseLine line =
  let parse = rx().TypedMatch(line)
  let min = parse.min.Value
  let max = parse.max.Value
  let character = parse.character.Value
  let pwd = parse.pwd.Value

  { min = int min
    max = int max
    char = char character
    pwd = pwd }

let count x = Seq.filter ((=) x) >> Seq.length

let validPwd (min: int) (max: int) (character: char) (pwd: string) =
  let c = count character pwd
  if min <= c && max >= c then true else false

let validPwd2 (min: int) (max: int) (character: char) pwd =
  let nMin = min - 1
  let nMax = max - 1
  let indexed = seq pwd |> Seq.indexed |> Seq.toList
  let c = character

  if not ((indexed.[nMin] = (nMin, c)) = (indexed.[nMax] = (nMax, c)))
  then true
  else false


let taskA =
  data
  |> List.map parseLine
  |> List.filter (fun line -> validPwd line.min line.max line.char line.pwd)
  |> List.length

let taskB =
  data
  |> List.map parseLine
  |> List.filter (fun line -> validPwd2 line.min line.max line.char line.pwd)
  |> List.length
