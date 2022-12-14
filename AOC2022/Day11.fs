module Day11

open Utility
open System.Collections.Generic

[<Struct>]
type Monkey =
    { Items: int64 list
      Op: char
      Const: int64 option
      Factor: int64
      IfTrue: int
      IfFalse: int
      Inspections: int64 }

    static member factor(m: Monkey) = m.Factor
    static member inspections(m: Monkey) = m.Inspections

type MonkeyTracker = Dictionary<int, Monkey>
let values (dict: Dictionary<'a, 'b>) = dict.Values

let parseLine (chunk: string[]) =
    { Items = chunk[1][18..] |> splitByString ", " |> Array.toList |> List.map int64
      Op = chunk[2][23]
      Const = chunk[2][25..] |> fun s -> if s = "old" then None else Some(int64 s)
      Factor = chunk[3] |> splitByChars [| ' ' |] |> Array.last |> int64
      IfTrue = chunk[4] |> splitByChars [| ' ' |] |> Array.last |> int
      IfFalse = chunk[5] |> splitByChars [| ' ' |] |> Array.last |> int
      Inspections = 0L }

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

let rec loop worryOp (dict: MonkeyTracker) = function
    | 0 -> dict
    | count ->
        Seq.iter (fun key -> List.iter (inspector worryOp dict key) dict[key].Items) dict.Keys
        loop worryOp dict (dec count)

let folder (dict: MonkeyTracker) (idx, monkey) = dict.Add(idx, monkey); dict

let parse mt = Seq.chunkBySize 7 >> Seq.map parseLine >> Seq.indexed >> Seq.fold folder mt

let solve cycles worryOp =
    parse (new MonkeyTracker())
    >> flip (loop worryOp) cycles >> values
    >> Seq.map Monkey.inspections >> Seq.sortDescending >> Seq.truncate 2 >> Seq.fold (*) 1L

let worryOp1 _ = flip (/) 3L

let part1 = solve 20 worryOp1

let worryOp2 = values >> Seq.map Monkey.factor >> Seq.reduce (*) >> flip (%)

let part2 = solve 10000 worryOp2

let solution = Solution.build (part1, part2)
