
let readLines filePath = System.IO.File.ReadLines(filePath);;

let input : int list =
  readLines "input01.txt"
  |> Seq.toList
  |> List.map int

let answer =
  [ for a in input do
    for b in input do
    for c in input do
      if a>b && a + b + c = 2020 then yield a*b*c];;

printf "\n\n    Answer: %A\n\n" (answer|>List.head)
