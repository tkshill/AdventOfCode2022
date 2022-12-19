module Day13

open FParsec
open FParsec.Pipes
open FSharpx.Collections
open Utility

type Nested =
    | N of int
    | L of Nested list

let nested, nestedRef = createParserForwardedToRef<Nested, unit> ()

let nParser: Parser<Nested, unit> = %% +.integerParser -|> N

let lParser: Parser<Nested, unit> =
    %% '[' -- +.(nested * (qty[0..] / ',')) -- ']' -|> (ResizeArray.toList >> L)

do nestedRef := %[ nParser; lParser ]

let rec plumb (L left) (L right) =
    match (left, right) with
    | _, _ when left = [] || right = [] -> left = []
    | N num :: _, N num2 :: _ when num <> num2 -> num < num2
    | N num :: lst, L _ :: _ -> plumb (L((L [ N num ]) :: lst)) (L right)
    | L _ :: _, N num :: lst2 -> plumb (L left) (L((L [ N num ]) :: lst2))
    | L lst :: _, L lst2 :: _ when lst <> lst2 -> plumb (L lst) (L lst2)
    | _ :: lst, _ :: lst2 -> plumb (L lst) (L lst2)

let parse = Seq.choose ((run lParser) >> parserResultToOption)

let part1: string seq -> int =
    parse
    >> Seq.chunkBySize 2
    >> Seq.map (seqToTuple >> unpack plumb)
    >> indexFromOne
    >> Seq.filter (snd >> id)
    >> Seq.sumBy fst

let rec quicksort =
    let recur p (lt, gt) = quicksort lt @ [ p ] @ quicksort gt

    function
    | [] -> []
    | pivot :: rest -> rest |> List.partition (flip plumb pivot) |> recur pivot

let additions = parse [| "[[2]]"; "[[6]]" |]

let part2: string array -> int =
    parse
    >> Seq.append additions
    >> Seq.toList
    >> quicksort
    >> indexFromOne
    >> Seq.filter (snd >> (=) >> flip Seq.exists additions)
    >> Seq.map fst
    >> Seq.reduce (*)

let solution input =
    { Part1 = part1 input |> string
      Part2 = part2 input |> string }
