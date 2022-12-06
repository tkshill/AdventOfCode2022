module DayThreeTests

open System.IO
open Xunit

let solution = File.ReadAllLines "day3test.txt" |> fun data -> Day3.solution data

[<Fact>]
let ``Part 1`` () = Assert.Equal("157", solution.Part1)

[<Fact>]
let ``Part 2`` () = Assert.Equal("70", solution.Part2)
