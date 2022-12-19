module Day13Tests

open Xunit
open Utility

let solution =
    match getInput 13 with
    | Some input -> Day13.solution input
    | _ -> failwith "Input cannot be found"

[<Fact>]
let ``Plumb tests`` () =
    let a = seq { "[1,1,3,1,1]" } |> Day13.parse |> Seq.head
    let b = seq { "[[1],[2,3,4]]" } |> Day13.parse |> Seq.head

    Assert.True(Day13.plumb a b)

[<Fact>]
let ``Part 1`` () = Assert.Equal("13", solution.Part1)

[<Fact>]
let ``Part 2`` () = Assert.Equal("140", solution.Part2)
