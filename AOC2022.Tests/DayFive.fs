module DayFiveTests

open System.IO
open Xunit

let solution =
    File.ReadAllLines "./data/day5test.txt" |> fun data -> Day5.solution data

[<Fact>]
let ``Part 1`` () = Assert.Equal("CMZ", solution.Part1)

[<Fact>]
let ``Part 2`` () = Assert.Equal("MCD", solution.Part2)
