module DayNineTests

open System.IO
open Xunit
open Utility

let solution =
    "./data/day9test.txt" |> File.ReadAllLines |> trimEnds |> Day9.solution

[<Fact>]
let ``Part 1`` () = Assert.Equal("13", solution.Part1)

[<Fact>]
let ``Part 2`` () = Assert.Equal("1", solution.Part2)
