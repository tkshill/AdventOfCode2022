module Day15Tests

open Xunit

let input = match getInput 15 with
    | Some input -> input
    | _ -> failwith "Input cannot be found"

[<Fact>]
let ``Part 1`` () = Assert.Equal(26,  Day15.part1 10 input)

[<Fact>]
let ``Part 2`` () = Assert.Equal(56000011L, Day15.part2 20 input)