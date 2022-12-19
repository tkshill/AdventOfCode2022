module Day08Tests

open Xunit
open Utility

let solution =
    match getInput 8 with
    | Some input -> Day08.solution input
    | _ -> failwith "Input cannot be found"

[<Fact>]
let ``Part 1`` () = Assert.Equal("21", solution.Part1)

[<Fact>]
let ``Part 2`` () = Assert.Equal("8", solution.Part2)
