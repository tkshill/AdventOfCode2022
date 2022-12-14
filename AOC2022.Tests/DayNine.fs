module DayNineTests

open Xunit
open Utility

let solution =
    match getInput 9 with
    | Some input -> Day9.solution input
    | _ -> failwith "Input cannot be found"

[<Fact>]
let ``Part 1`` () = Assert.Equal("13", solution.Part1)

[<Fact>]
let ``Part 2`` () = Assert.Equal("1", solution.Part2)
