namespace AdventOfCode.Solution

open AdventOfCode
open System.Text.RegularExpressions

type DrawnBoardNumber = { number: int; drawn: bool }

type Board =
  { board: DrawnBoardNumber list list
    won: bool
    count: int
    winningNumber: int option }

type Selector = (Board -> int) -> Board list -> Board

module Day04 =
  let parseNumber (split_char: string) (line: string) =
    Regex.Split(line, split_char)
    |> List.ofArray
    |> List.map int

  let initializeBoard (board: int list list) =
    let board =
      board
      |> List.map (fun line ->
        line
        |> List.map (fun item -> { number = item; drawn = false }))

    { board = board
      won = false
      count = 0
      winningNumber = None }

  let parseBoard (lines: string list) =
    lines
    |> List.map (fun line -> line.Trim())
    |> List.map (fun line -> parseNumber "\s+" line)
    |> initializeBoard

  let parse (input: string) =
    let data =
      input.Split "\n"
      |> List.ofArray
      |> List.filter (System.String.IsNullOrWhiteSpace >> not)

    let numbers = data |> List.head |> parseNumber ","

    let boards =
      data
      |> List.skip 1
      |> List.chunkBySize 5
      |> List.map parseBoard

    numbers, boards

  //  Actions
  let hasWon (board: DrawnBoardNumber list list) =
    let winCondition = fun board ->
     board
     |> List.exists (fun line ->
       line
       |> List.forall (fun drawnNumber -> drawnNumber.drawn))

    let p1 = board |> List.transpose |> winCondition
    let p2 = board |> winCondition

    p1 || p2

  let drawOnBoard (number: int) (board: Board) =
    let brd =
      board.board
      |> List.map (fun line ->
        line
        |> List.map (fun item ->
          if item.number = number then
            { item with drawn = true }
          else
            item))
    let won = hasWon brd

    { board with
        board = brd
        won = hasWon brd
        count = board.count + 1
        winningNumber = match won with | true -> Some number | false -> None }

  let rec drawNumber (numbers: int list) (board: Board) =
    match numbers with
    | _ when board.won -> board // Dont draw more on board if won
    | [] -> board
    | nr::xs -> drawNumber xs (drawOnBoard nr board)

  let score (board: Board) =
    let unmarked =
      board.board
      |> List.concat
      |> List.filter (fun item -> item.drawn = false)
      |> List.sumBy (fun item -> item.number)
    unmarked * board.winningNumber.Value

  let play (selector: Selector) (input: int list * Board list) =
    let numbers, boards = input
    boards
    |> List.map (drawNumber numbers)
    |> List.filter (fun board -> board.won)
    |> selector (fun board -> board.count)
    |> score

  let part1 (input: int list * Board list)=
    play List.minBy input

  let part2 (input: int list * Board list)=
    play List.maxBy input

  let solve input =
    parse input
    |> part1

