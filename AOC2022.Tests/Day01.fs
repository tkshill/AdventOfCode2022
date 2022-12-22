module Day01Tests

open Xunit
open Utility

let solution =
    match getInput 1 with
    | Some input -> Day01.solution input
    | _ -> failwith "Input cannot be found"

[<Fact>]
let ``Part 1`` () = Assert.Equal("24000", solution.Part1())

[<Fact>]
let ``Part 2`` () = Assert.Equal("45000", solution.Part2())
