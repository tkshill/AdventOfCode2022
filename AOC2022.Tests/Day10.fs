module Day10Tests

open Xunit
open Utility

let solution =
    match getInput 10 with
    | Some input -> Day10.solution input
    | _ -> failwith "Input cannot be found"

[<Fact>]
let ``Part 1`` () = Assert.Equal("13140", solution.Part1)


let part2result =
    "##..##..##..##..##..##..##..##..##..##..\n\
    ###...###...###...###...###...###...###.\n\
    ####....####....####....####....####....\n\
    #####.....#####.....#####.....#####.....\n\
    ######......######......######......####\n\
    #######.......#######.......#######....."

[<Fact>]
let ``Part 2`` () =
    Assert.Equal(part2result, solution.Part2)
