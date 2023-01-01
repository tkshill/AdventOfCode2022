module Day16

open FParsec
open FParsec.Pipes

type Valve = string
type Flow = int
type Order = int
type Distance = int

type Graph = Map<Valve, Valve list>

type Graph1 = Map<Valve, Flow * Map<Valve, Distance>>

type Graph2 = Map<string * int, int>

let pV:Parser<string, unit> = %% +. (asciiUpper * qty[2]) -|> toString

let pL = %% "Valve " -- +. pV -- " has flow rate=" -- +. pint32 -- %["; tunnels lead to valves "; "; tunnel leads to valve "] -- +. (pV * (qty[1..] / ", ")) 
        -|> fun v f vs -> v, (f, List.ofSeq (seq vs))

let rec permutations = function
        | []      -> seq [List.empty]
        | x :: xs -> Seq.collect (insertions x) (permutations xs)
    and insertions x = function
        | []             -> [[x]]
        | (y :: ys) as xs -> (x::xs)::(List.map (fun x -> y::x) (insertions x ys))

let rec bfs' (graph: Graph) visited distances = function
        | [] -> distances
        | head::tail -> 
                graph[head] |> List.except visited 
                |> Seq.fold (fun (v', d', qa') n -> n::v', Map.add n (d'[head] + 1) d', n::qa') (visited, distances, tail)
                |> unpack2 (bfs' graph)

and bfs graph node = bfs' graph [node] (Map.empty.Add(node, 0)) [node] |> Map.remove node

let reachable visited time v = [("AA", 0, 0)]

// let rec ayeStar' score graph visited goal = function
//         | (goal,_)::_ -> score[goal]
//         | (current, time) :: rest ->
//                 graph[current] |> Seq.filter  |> Seq.map toflows 
//                 |> Seq.fold scores, visiteds 

// and ayeStar scores start goal graph = 0

let flowFinder (graph:Graph1) valve = function
        | (previousValve, timeRemaining, flow) when timeRemaining - (snd graph[valve])[previousValve] <= 0 -> valve, 0, flow
        | (previousValve, timeRemaining, flow) -> 
                let timeRemaining' = timeRemaining - (snd graph[valve])[previousValve] - 1
                valve, timeRemaining', flow + (fst graph[valve] * timeRemaining')

let part1 =
        runParser pL
        >> Map.ofSeq 
        >> fun m -> Map.filter (pack (snd >> fst >> (<>) 0)) m |> Map.map (fun valve (flow, _) -> flow, bfs (Map.map (pack (snd >> snd)) m) valve)
        >> fun m -> Map.toList m |> List.map fst |> log id |> permutations |> Seq.map ( fun valves -> Seq.foldBack (flowFinder m) valves ("AA", 30, 0))
        >> Seq.map trd >> Seq.max 

let solution = Solution.build(part1)