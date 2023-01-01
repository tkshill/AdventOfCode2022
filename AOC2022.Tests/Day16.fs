module Day16Tests

open Xunit

let input =
    match getInput 16 with
    | Some input -> input
    | _ -> failwith "Input cannot be found"

[<Fact>]
let ``Part 1`` () = 
    let solution = Day16.solution input
    Assert.Equal("1651",  solution.Part1())