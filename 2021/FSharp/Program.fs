// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open System

open AdventOfCode

[<EntryPoint>]
let main argv =
    let input04 = System.IO.File.ReadAllText "Input/input04.txt"

    //let day01p1 = Solution.Day01.part1 Input.Day01.input
    //let day01p2 = Solution.Day01.part2 Input.Day01.input
    //let day02p1 = Solution.Day02.part1 Input.Day02.input
    //let day02p2 = Solution.Day02.part2 Input.Day02.input
    //let day03p1 = Solution.Day03.part1
    //let day03p2 = Solution.Day03.part2

    let day04 = Solution.Day04.solve input04

    printfn $"Day 04: %A{day04}"

    //printfn "Day 01 part 1: %A"   day01p1
    //printfn "Day 01 part 2: %A\n" day01p2

    //printfn "Day 02 part 1: %A"   day02p1
    //printfn "Day 02 part 2: %A\n" day02p2

    //printfn "Day 03 part 1: %A"   day03p1
    //printfn "Day 03 part 2: %A\n" day03p2
    0 // return an integer exit code