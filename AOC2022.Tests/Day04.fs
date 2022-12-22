module Day04Tests

open Xunit
open Utility

let solution =
    match getInput 4 with
    | Some input -> Day04.solution input
    | _ -> failwith "Input cannot be found"

[<Theory>]
[<InlineData("99-99,18-99", "1")>]
[<InlineData("2-86,1-86", "1")>]
[<InlineData("9-21,10-22", "0")>]
[<InlineData("1-24,7-23", "1")>]
[<InlineData("33-37,35-38", "0")>]
[<InlineData("15-57,14-56", "0")>]
[<InlineData("1-88,3-89", "0")>]
[<InlineData("26-56,27-57", "0")>]
[<InlineData("11-94,93-98", "0")>]
[<InlineData("40-92,3-91", "0")>]
let ``Part 1 tests for individual lines`` (data, expected) =
    Assert.Equal(expected, Day04.part1 (Array.singleton data))

[<Fact>]
let ``Part 1`` () = Assert.Equal("2", solution.Part1())

[<Theory>]
[<InlineData("2-4,6-8", false)>]
[<InlineData("2-3,4-5", false)>]
[<InlineData("5-7,7-9", true)>]
[<InlineData("2-8,3-7", true)>]
[<InlineData("6-6,4-6", true)>]
[<InlineData("2-6,4-8", true)>]
let ``Part 2 tests for individual lines`` (data, expected) =
    Assert.Equal(expected, Day04.isOverlap (Day04.stringToPairs data))

[<Fact>]
let ``Part 2`` () = Assert.Equal("4", solution.Part2())
