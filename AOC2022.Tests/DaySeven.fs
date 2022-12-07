module DaySevenTests

open System.IO
open Xunit

let solution =
    File.ReadAllLines "./data/day7test.txt" |> fun data -> Day7.solution data

[<Fact>]
let ``Part 1`` () = Assert.Equal("95437", solution.Part1)

[<Fact>]
let ``Part 2`` () =
    Assert.Equal("24933642", solution.Part2)
