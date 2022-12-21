module Day14Tests

open Xunit
open Utility

let solution =
    match getInput 14 with
    | Some input -> Day14.solution input
    | _ -> failwith "Input cannot be found"

[<Fact>]
let ``Part 1`` () = Assert.Equal("24", solution.Part1)

[<Fact>]
let ``Part 2`` () = Assert.Equal("93", solution.Part2)
