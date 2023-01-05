module Day12Tests

open Xunit
open Utility

let solution =
    match getInput 12 with
    | Some input -> Day12.solution input
    | _ -> failwith "Input cannot be found"

[<Fact>]
let ``Part 1`` () = Assert.Equal("31", solution.Part1())

[<Fact>]
let ``Part 2`` () = Assert.Equal("29", solution.Part2())