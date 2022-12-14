module DaySixTests

open Xunit
open Utility

let solution =
    match getInput 11 with
    | Some input -> Day11.solution input
    | _ -> failwith "Input cannot be found"

[<Theory>]
[<InlineData("mjqjpqmgbljsphdztnvjfqwrcgsmlb", "7")>]
[<InlineData("bvwbjplbgvbhsrlpgdmjqwftvncz", "5")>]
[<InlineData("nppdvjthqldpwncqszvftbrmjlhg", "6")>]
[<InlineData("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", "10")>]
[<InlineData("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", "11")>]
let ``Part 1`` (input, expected) =
    Assert.Equal(expected, Day6.part1 (Seq.singleton input))

// [<Fact>]
[<Theory>]
[<InlineData("mjqjpqmgbljsphdztnvjfqwrcgsmlb", "19")>]
[<InlineData("bvwbjplbgvbhsrlpgdmjqwftvncz", "23")>]
[<InlineData("nppdvjthqldpwncqszvftbrmjlhg", "23")>]
[<InlineData("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", "29")>]
[<InlineData("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", "26")>]
let ``Part 2`` (input, expected) =
    Assert.Equal(expected, Day6.part2 (Seq.singleton input))
