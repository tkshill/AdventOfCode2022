module Day14

open FParsec
open FParsec.Pipes
open Utility

let pT = %% +.pint32 -- ',' -- +.pint32 -%> auto

let pL: Parser<(int32 * int32) seq, unit> = %% +.(pT * (qty[0..] / " -> ")) -|> seq

let pointsToPath (x, y) =
    function
    | x2, _ when x <> x2 -> seq { for i in min x x2 .. max x x2 -> (i, y) }
    | _, y2 when y <> y2 -> seq { for i in min y y2 .. max y y2 -> (x, i) }

let mutable abyssalTransform = fst
let mutable abyssalClause = true
let mutable depthLimit = Unchecked.defaultof<int>

let goesAbyssal (x, y) =
    isnt (Set.exists (fun (x2, y2) -> y2 >= y && x = x2))

let moment cavern (x, y) =
    [ (x, inc y); (dec x, inc y); (inc x, inc y) ]
    |> List.tryFind (isnt (flip Set.contains cavern))
    |> Option.defaultValue (x, y)

let rec dropGrain cavern =
    function
    | _ when Set.contains (500, 0) cavern -> true, cavern
    | sand when goesAbyssal sand cavern -> abyssalClause, abyssalTransform (cavern, sand)
    | sand when moment cavern sand = sand -> false, Set.add (log id sand) cavern
    | sand -> dropGrain cavern (moment cavern sand)

let rec count n =
    function
    | true, cavern -> n, (false, cavern)
    | false, cavern -> count (inc n) (dropGrain cavern (500, 0))

let parse =
    Seq.choose (runParser pL)
    >> Seq.collect (Seq.pairwise >> Seq.collect (unpack pointsToPath))
    >> set
    >> toTuple false

let part1: string seq -> int = parse >> count -1 >> fst

let actions (s: int * (bool * Set<int * int>)) =
    depthLimit <- s |> snd |> snd |> seq |> Seq.maxBy snd |> snd |> ((+) 1)
    abyssalTransform <- fun (cavern, sand) -> Set.add (log id (fst sand, depthLimit)) cavern
    abyssalClause <- false

let part2: string seq -> int = parse >> count -1 >> fst // withEffect actions >> unpack count >> fst >> (flip (-) 1)

let solution input =
    { Part1 = part1 input |> string
      Part2 = part2 input |> string }
