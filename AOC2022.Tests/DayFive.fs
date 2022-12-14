module DayFiveTests

open Xunit
open Utility

let solution =
    match getInput 5 with
    | Some input -> Day11.solution input
    | _ -> failwith "Input cannot be found"

[<Fact>]
let ``Part 1`` () = Assert.Equal("CMZ", solution.Part1)

[<Fact>]
let ``Part 2`` () = Assert.Equal("MCD", solution.Part2)
