module DayThreeTests

open System.IO
open Xunit

let solution = File.ReadAllLines "day3test.txt" |> fun data -> Day3.Solution data

[<Theory>]
[<InlineData("aa", 1)>]
[<InlineData("vJrwpWtwJgWrhcsFMMfFFhFp", 16)>]
[<InlineData("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", 38)>]
[<InlineData("PmmdzqPrVvPwwTWBwg", 42)>]
let ``Part 1 logic`` (input, expected) =
    Assert.Equal(expected, Day3.calculate input)

[<Fact>]
let ``Part 1`` () = Assert.Equal("157", solution.Part1)

[<Fact>]
let ``Part 2`` () = Assert.Equal("70", solution.Part2)
