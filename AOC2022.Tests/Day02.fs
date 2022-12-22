module Day02Tests

open Xunit
open Utility

let solution =
    match getInput 2 with
    | Some input -> Day02.solution input
    | _ -> failwith "Input cannot be found"

[<Fact>]
let ``Part 1`` () = Assert.Equal("15", solution.Part1())

[<Fact>]
let ``Part 2`` () = Assert.Equal("12", solution.Part2())
