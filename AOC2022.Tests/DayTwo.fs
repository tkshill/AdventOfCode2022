module DayTwoTests

open System.IO
open Xunit

let solution = File.ReadAllLines "day2test.txt" |> fun data -> Day2.solution data

[<Fact>]
let ``Part 1`` () = Assert.Equal("15", solution.Part1)

[<Fact>]
let ``Part 2`` () = Assert.Equal("12", solution.Part2)
