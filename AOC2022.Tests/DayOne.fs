module DayOneTests

open System.IO
open Xunit

let solution =
    File.ReadAllLines "./data/day1test.txt" |> fun data -> Day1.solution data

[<Fact>]
let ``Part 1`` () = Assert.Equal("24000", solution.Part1)

[<Fact>]
let ``Part 2`` () = Assert.Equal("45000", solution.Part2)
