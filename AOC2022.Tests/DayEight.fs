module DayEightTests

open System.IO
open Xunit
open Utility

let solution =
    "./data/day8test.txt" |> File.ReadAllLines |> trimEnds |> Day8.solution

[<Fact>]
let ``Part 1`` () = Assert.Equal("21", solution.Part1)

[<Fact>]
let ``Part 2`` () = Assert.Equal("8", solution.Part2)
