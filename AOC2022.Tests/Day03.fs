module Day03Tests

open Xunit
open Utility

let solution =
    match getInput 3 with
    | Some input -> Day03.solution input
    | _ -> failwith "Input cannot be found"

[<Fact>]
let ``Part 1`` () = Assert.Equal("157", solution.Part1())

[<Fact>]
let ``Part 2`` () = Assert.Equal("70", solution.Part2())
