module Day11

open Utility
open System
open System.Collections.Generic

type Monkey =
    { Items: int64 list array
      Op: char
      Const: int64 option
      Factor: int64
      IfTrue: int
      IfFalse: int
      Inspections: int }

type MonkeyTracker = Dictionary<int, Monkey>

let cache = new Dictionary<int64, int64 list>()

let rec getFactor acc initial threshold num =
    function
    | proposed when (float proposed) > threshold || proposed = num -> num :: acc
    | proposed when num % proposed = 0L -> getFactor (proposed :: acc) initial threshold (num / proposed) proposed
    | proposed -> getFactor acc initial threshold num (proposed + 1L)

let distinctPrimeFactors n =
    if cache.ContainsKey(n) then
        cache[n]
    else
        let factors = getFactor [] n (sqrt (float n)) n 2 |> List.distinct
        cache.Add(n, factors)
        factors

let parseLine (chunk: string[]) =
    { Items =
        chunk[1][18..]
        |> fun s -> s.Split(", ") |> Array.map (int64 >> distinctPrimeFactors)
      Op = chunk[2][23]
      Const = chunk[2][25..] |> fun s -> if fst (Int32.TryParse s) then Some(int s) else None
      Factor = chunk[3] |> splitByChars [| ' ' |] |> Array.last |> int64
      IfTrue = chunk[4] |> splitByChars [| ' ' |] |> Array.last |> int
      IfFalse = chunk[5] |> splitByChars [| ' ' |] |> Array.last |> int
      Inspections = 0 }

let calculateWorry divider (d: MonkeyTracker) k l =
    match (d[k].Op, d[k].Const) with
    | '*', None -> l
    | '*', Some v -> List.distinct (v :: l)
    | '+', Some v -> List.fold (*) 1L l |> (+) v |> distinctPrimeFactors

let inspector dv (d: MonkeyTracker) k l =
    let l' = calculateWorry dv d k l

    let k' =
        if List.contains d[k].Factor l' then
            d[k].IfTrue
        else
            d[k].IfFalse

    d[k] <- { d[k] with Inspections = (d[k].Inspections) + 1 }
    d[k] <- { d[k] with Items = Array.tail d[k].Items }
    d[k'] <- { d[k'] with Items = Array.append d[k'].Items (Array.singleton l') }

let rec loop dv (dict: MonkeyTracker) =
    function
    | 0 -> dict
    | count ->
        Seq.iter (fun key -> Array.iter (inspector dv dict key) (log id dict[key].Items)) dict.Keys
        loop dv dict (dec count)

let folder (dict: MonkeyTracker) (idx, monkey) =
    dict.Add(idx, monkey)
    dict

let solve cycles divider =
    Array.chunkBySize 7
    >> Array.map parseLine
    >> Array.indexed
    >> Array.fold folder (new MonkeyTracker())
    >> flip (loop divider) cycles
    >> fun dict -> dict.Values
    >> Seq.map (fun m -> m.Inspections)
    >> Array.ofSeq
    >> Array.sortDescending
    >> Array.truncate 2
    >> Array.fold (*) 1



let part1: string array -> int = solve 20 3

let part2: string array -> int = solve 1000 1

let solution input =
    { Part1 = part1 input |> string
      Part2 = part2 input |> string }
