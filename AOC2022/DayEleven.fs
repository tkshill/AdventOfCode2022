module Day11

open Utility
open System
open System.Collections.Generic

type Monkey =
    { Items: int64 list
      Op: char
      Const: int64 option
      Factor: int64
      IfTrue: int
      IfFalse: int
      Inspections: int64 }

    static member factor(m: Monkey) = m.Factor

type MonkeyTracker = Dictionary<int, Monkey>

let parseLine (chunk: string[]) =
    { Items = chunk[1][18..] |> fun s -> s.Split(", ") |> Array.map int64 |> Array.toList
      Op = chunk[2][23]
      Const = chunk[2][25..] |> fun s -> if fst (Int32.TryParse s) then Some(int s) else None
      Factor = chunk[3] |> splitByChars [| ' ' |] |> Array.last |> int64
      IfTrue = chunk[4] |> splitByChars [| ' ' |] |> Array.last |> int
      IfFalse = chunk[5] |> splitByChars [| ' ' |] |> Array.last |> int
      Inspections = 0 }

let calculateWorry (worryOp: MonkeyTracker -> int64 -> int64) (d: MonkeyTracker) k l =
    match (d[k].Op, d[k].Const) with
    | '*', Some v -> (worryOp d) (l * v)
    | '+', Some v -> (worryOp d) (l + v)
    | _ -> (worryOp d) (l * l)

let inspector worryOp (d: MonkeyTracker) k l =
    let l' = calculateWorry worryOp d k l
    let k' = if l' % d[k].Factor = 0 then d[k].IfTrue else d[k].IfFalse

    d[k] <- { d[k] with Inspections = (d[k].Inspections) + 1L }
    d[k] <- { d[k] with Items = List.tail d[k].Items }
    d[k'] <- { d[k'] with Items = d[k'].Items @ [ l' ] }

let rec loop worryOp (dict: MonkeyTracker) =
    function
    | 0 -> dict
    | count ->
        Seq.iter (fun key -> List.iter (inspector worryOp dict key) dict[key].Items) dict.Keys
        loop worryOp dict (dec count)

let folder (dict: MonkeyTracker) (idx, monkey) =
    dict.Add(idx, monkey)
    dict

let parse mt =
    Seq.chunkBySize 7 >> Seq.map parseLine >> Seq.indexed >> Seq.fold folder mt

let solve cycles worryOp =
    parse (new MonkeyTracker())
    >> flip (loop worryOp) cycles
    >> fun dict -> dict.Values
    >> Seq.map (fun m -> m.Inspections)
    >> Seq.sortDescending
    >> Seq.truncate 2
    >> Seq.fold (*) 1L

let worryOp1 _ = flip (/) 3L

let part1: string array -> int64 = solve 20 worryOp1

let worryOp2 (mt: MonkeyTracker) =
    mt.Values |> Seq.map Monkey.factor |> Seq.reduce (*) |> flip (%)

let part2: string array -> int64 = solve 10000 worryOp2

let solution input =
    { Part1 = part1 input |> string
      Part2 = part2 input |> string }
