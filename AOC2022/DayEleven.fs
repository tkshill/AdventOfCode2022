module Day11

open Utility
open System
open System.Collections.Generic

type Monkey =
    { Items: int64 array
      Operation: int64 -> int64 -> int64
      Constant: int64 option
      Factor: int64
      IfTrue: int
      IfFalse: int
      Inspections: int }

type MonkeyTracker = Dictionary<int, Monkey>

let parseLine (chunk: string[]) =
    { Items = chunk[1][18..] |> fun s -> s.Split(", ") |> Array.map int64
      Operation = chunk[2][23] |> fun c -> if c = '*' then (*) else (+)
      Constant = chunk[2][25..] |> fun s -> if fst (Int32.TryParse s) then Some(int s) else None
      Factor = chunk[3] |> split [| ' ' |] |> Array.last |> int64
      IfTrue = chunk[4] |> split [| ' ' |] |> Array.last |> int
      IfFalse = chunk[5] |> split [| ' ' |] |> Array.last |> int
      Inspections = 0 }

let calculateWorry divider (d: MonkeyTracker) k l =
    (Option.defaultValue l d[k].Constant) |> d[ k ].Operation l |> flip (/) divider

let inspector dv (d: MonkeyTracker) k l =
    let mutable l' = calculateWorry dv d k l

    let k' = if l' % d[k].Factor = 0 then d[k].IfTrue else d[k].IfFalse

    l' <- l' % d[k].Factor

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
    >> log Seq.toList
    >> Seq.map (fun m -> m.Inspections)
    >> Array.ofSeq
    >> Array.sortDescending
    >> Array.truncate 2
    >> Array.fold (*) 1

let part1: string array -> int = solve 20 3

let part2: string array -> int = solve 20 1

let solution input =
    { Part1 = part1 input |> string
      Part2 = part2 input |> string }
