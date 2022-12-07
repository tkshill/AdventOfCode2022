module DaySixTests

open System.IO
open Xunit

let solution =
    File.ReadAllLines "./data/day6test.txt" |> fun data -> Day6.solution data

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
