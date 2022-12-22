module Day14

open FParsec
open FParsec.Pipes
open Utility

type SandFall =
    | Complete
    | Incomplete

let pT = %% +.pint32 -- ',' -- +.pint32 -%> auto

let pL: Parser<(int * int) seq, unit> = %% +.(pT * (qty[0..] / " -> ")) -|> seq

let spread (x, y) =
    function
    | x2, _ when x <> x2 -> seq { for i in min x x2 .. max x x2 -> (i, y) }
    | _, y2 when y <> y2 -> seq { for i in min y y2 .. max y y2 -> (x, i) }

let mutable abyssalTransform = fst
let mutable abyssCondition = Complete
let mutable depthLimit = Unchecked.defaultof<int>

let goesAbyssal (x, y) =
    isnt (Set.exists (fun (x2, y2) -> y2 >= y && x = x2))

let advanceSand cavern (x, y) =
    [ (x, inc y); (dec x, inc y); (inc x, inc y) ]
    |> List.tryFind (isnt (flip Set.contains cavern))
    |> Option.defaultValue (x, y)

let rec dropGrain cavern =
    function
    | [] -> Complete, cavern, []
    | next :: rest when goesAbyssal next cavern -> abyssCondition, abyssalTransform (cavern, next), rest
    | next :: rest when advanceSand cavern next = next -> Incomplete, Set.add next cavern, rest
    | next :: rest -> dropGrain cavern (advanceSand cavern next :: next :: rest)

let rec count grains =
    function
    | Complete, cavern, previous -> grains, (Incomplete, cavern, previous)
    | Incomplete, cavern, previous -> count (inc grains) (dropGrain cavern previous)

let parseAndPass =
    Seq.choose (runParser pL)
    >> Seq.collect (Seq.pairwise >> Seq.collect (unpack spread))
    >> set
    >> fun s -> Incomplete, s, [ (500, 0) ]
    >> count 0

let part1 = parseAndPass >> fst >> flip (-) 1

let actions (_, (_, s, _)) =
    depthLimit <- s |> Seq.maxBy snd |> snd |> ((+) 1)
    abyssalTransform <- fun (cavern, sand) -> Set.add (fst sand, depthLimit) cavern
    abyssCondition <- Incomplete

let part2 = parseAndPass >> withEffect actions >> unpack count >> fst

let solution = Solution.build (part1, part2)
