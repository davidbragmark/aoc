// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp


open System

open AdventOfCode

[<EntryPoint>]
let main argv =
    let day02p1 = Solution.Day02.part1 Input.Day02.input
    let day02p2 = Solution.Day02.part2 Input.Day02.input

    printfn "Day 02 part 1: %A" day02p1
    printfn "Day 02 part 2: %A" day02p2

    0 // return an integer exit code