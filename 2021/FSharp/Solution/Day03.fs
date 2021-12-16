namespace AdventOfCode.Solution

open AdventOfCode

module Day03 =

  let rows = Input.Day03.input.Length
  let columns = Input.Day03.input.[0].Length

  let parseList =
    Input.Day03.input
    |> Array.map (fun str ->
      str |> Seq.map (fun c -> c.ToString() |> int)
      |> Array.ofSeq
    )

  type Strategy = (int [] [] * int [] []) -> int [] []

  let oxygen (zeros: int [] [], ones: int [] []) =
    match ones.Length >= zeros.Length with
    | true -> ones
    | false -> zeros

  let carbon (zeros: int [] [], ones: int [] []) =
    match ones.Length >= zeros.Length with
    | true -> zeros
    | false -> ones

  let getOnes (column: int) (candidates: int [] []) =
    candidates |> Array.filter (fun c -> c[column] = 1)

  let getZeros (column: int) (candidates: int [] []) =
    candidates |> Array.filter (fun c -> c[column] = 0)

  let separateIntoOneAndZeros (column: int) (candidates: int [] []) =
    (candidates |> getZeros column, candidates |> getOnes column)

  let binaryStringToInt b = System.Convert.ToInt32(b, 2)

  // Step one column at the time and filter out ones or zeros
  let rec selectCandidate (selector: Strategy) (column: int) (candidates: int [] []) =
      match candidates with
      | [||] -> failwith "should not happen"
      | [| last |] -> last |> Array.fold (fun binary number -> binary + string number) "" |> binaryStringToInt
      | candidates ->
          candidates
          |> separateIntoOneAndZeros column
          |> selector
          |> selectCandidate selector (column + 1)

  let calcOxygen =
    parseList
    |> selectCandidate oxygen 0

  let calcCarbon =
    parseList
    |> selectCandidate carbon 0

  // Part1
  let sumList =
    parseList
    |> Array.transpose
    |> Array.map Array.sum

  let gamma_rate =
    sumList
    |> Array.map (fun number -> if (rows / 2) < number then 1 else 0)
    |> Array.fold (fun state item -> state + (string item)) ""
    |> fun str -> System.Convert.ToInt32(str, 2)

  let epsilon_rate =
    sumList
    |> Array.map (fun number -> if (rows / 2) > number then 1 else 0)
    |> Array.fold (fun state item -> state + (string item)) ""
    |> fun str -> System.Convert.ToInt32(str, 2)

  let part1 = gamma_rate * epsilon_rate

  let part2 = calcCarbon * calcOxygen

