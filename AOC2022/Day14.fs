module Day14

open FParsec
open FParsec.Pipes

type SandFall = Complete | Incomplete

let pT = %% +.pint32 -- ',' -- +.pint32 -%> auto

let pL: Parser<(int * int) seq, unit> = %% +.(pT * (qty[0..] / " -> ")) -|> seq

let spread (x, y) =
    function
    | x2, _ when x <> x2 -> seq { for i in min x x2 .. max x x2 -> (i, y) }
    | _, y2 when y <> y2 -> seq { for i in min y y2 .. max y y2 -> (x, i) }

let mutable abyssalTransform, abyssCondition, depthLimit = fst, Complete, Unchecked.defaultof<int>

let goesAbyssal (x, y) = isnt (Set.exists (fun (x2, y2) -> y2 >= y && x = x2))

let advanceGrain cavern (x, y) =
    [ (x, inc y); (dec x, inc y); (inc x, inc y) ]
    |> List.tryFind (isnt (flip Set.contains cavern))
    |> Option.defaultValue (x, y)

let rec dropGrain cavern = function
    | []                                                        -> Complete, cavern, []
    | latest :: priors when goesAbyssal latest cavern           -> abyssCondition, abyssalTransform (cavern, latest), priors
    | latest :: priors when advanceGrain cavern latest = latest -> Incomplete, Set.add latest cavern, priors
    | latest :: priors                                          -> dropGrain cavern (advanceGrain cavern latest :: latest :: priors)

let rec count grainCount = function
    | Complete, cavern, sandPath -> grainCount, (Incomplete, cavern, sandPath)
    | Incomplete, cavern, sandPath -> count (inc grainCount) (dropGrain cavern sandPath)

let parseAndPass =
    Seq.choose (runParser pL)
    >> Seq.collect (Seq.pairwise >> Seq.collect (unpack spread)) >> set
    >> fun cavern -> count -1 (Incomplete, cavern, [ (500, 0) ])

let part1 = parseAndPass >> fst

let actions (_, (_, cavern, _)) =
    depthLimit <- (Seq.maxBy snd >> snd >> (+) 1) cavern
    abyssalTransform <- fun (cavern, grain) -> Set.add (fst grain, depthLimit) cavern
    abyssCondition <- Incomplete

let part2 = parseAndPass >> withEffect actions >> unpack count >> fst >> flip (-) 1

let solution = Solution.build (part1, part2)
