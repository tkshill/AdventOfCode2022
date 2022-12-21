module Day14

open FParsec
open FParsec.Pipes
open Utility
open FSharpx.Collections

let pTuple = %% +.pint32 -- ',' -- +.pint32 -%> auto

let pLine: Parser<(int32 * int32) seq, unit> =
    %% +.(pTuple * (qty[0..] / " -> ")) -|> ResizeArray.toSeq

let pointsToPath (x, y) =
    function
    | x2, _ when x <> x2 -> seq { for i in min x x2 .. max x x2 -> (i, y) }
    | _, y2 when y <> y2 -> seq { for i in min y y2 .. max y y2 -> (x, i) }

let goesAbyssal (x, y) =
    isnt (Set.exists (fun (x2, y2) -> y2 >= y && x = x2))

let moment cavern (x, y) =
    [ (x, inc y); (dec x, inc y); (inc x, inc y) ]
    |> Seq.tryFind (isnt (flip Set.contains cavern))
    |> Option.defaultValue (x, y)

let rec dropGrain cavern =
    function
    | sand when goesAbyssal sand cavern -> true, cavern
    | sand when moment cavern sand = sand -> false, Set.add sand cavern
    | sand -> dropGrain cavern (moment cavern sand)

let rec countGrains cavern =
    function
    | count, true -> count
    | count, false ->
        let hitLimit, cavern' = dropGrain cavern (500, 0)
        countGrains cavern' (inc count, hitLimit)

let part1: string seq -> int =
    Seq.choose (runParser pLine)
    >> Seq.collect (Seq.pairwise >> Seq.collect (unpack pointsToPath))
    >> set
    >> flip countGrains (-1, false)

let dropGrain2 cavern sand = cavern

let rec countGrains2 count =
    function
    | cavern when Set.contains (500, 0) cavern -> count
    | cavern -> countGrains2 (inc count) (dropGrain2 cavern (500, 0))



let part2 input = fun _ -> 0

let solution input =
    { Part1 = part1 input |> string
      Part2 = part2 input |> string }
