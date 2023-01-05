module Day12

open System

type Distances = Map<(int * int), {| Previous: int * int; Value: char; Distance: int |}>

let mutable (stop:int*int), (characters: Map<int * int, char>) = (Unchecked.defaultof<int>, Unchecked.defaultof<int>), Map.empty

let setGraph key value g = Map.change key (Option.map (fun _ -> value)) g;

let surroundings (x, y) = Seq.filter (flip Map.containsKey characters) [dec x, y; x, dec y; x, inc y; inc x, y]

let isReachableBy (x0, y0) (x1, y1) = int characters[x1, y1] - int characters[x0, y0] <= 1 

let prioritizeUp (distances: Distances) (x, y) = float distances[(x, y)].Distance + (1.0 / float characters[x, y])

let update (distances: Distances) current next =
    if (Option.defaultValue {| Previous=current; Value='z'; Distance=Int32.MaxValue |} (Map.tryFind next distances)).Distance <= inc distances[current].Distance then distances
    else Map.add next {|Previous=current; Value = characters[next]; Distance = inc distances[current].Distance|} distances
    
let rec plumb' visited (distances: Distances) = function
        | head::tail when head <> stop ->
            surroundings head |> Seq.except visited |> Seq.filter (isReachableBy head)
            |> Seq.fold (fun (visited', distances', queue) position -> position::visited', update distances' head position, position::queue) (visited, distances, tail)
            |> fun (visited', distances', queue) -> plumb' visited' distances' (List.sortBy (prioritizeUp distances') queue)
        | _ -> distances

and plumb start = plumb' [] (Map.empty.Add(start, {|Previous=start; Value=characters[start]; Distance=0|})) [start]

let toMap (strings: string[]) =
    let x, y = Seq.length strings, Seq.length (Seq.head strings)
    Seq.fold (fun characters (key, character) -> Map.add key character characters) Map.empty (seq {for i in 0..x - 1 do for j in 0..y-1 -> (i, j), strings[i][j]})

let trySolve c = 
    let start = Map.findKey (fun _ character -> character = 'S') c
    stop <- Map.findKey (fun _ character -> character = 'E') c

    c |> setGraph start 'a' |> setGraph stop 'z' |> fun c -> characters <- c;
    
    plumb start |> Map.tryFind stop 
    |> Option.defaultValue {|Previous=stop; Value=characters[stop]; Distance=Int32.MaxValue|} |> fun n -> n.Distance

let part1 = toMap >> trySolve

let rec findStarts ds (distances: Distances) = function
    | current when distances[current].Previous = current -> (current, distances[current].Distance)::ds
    | current when distances[current].Value = 'a' -> findStarts ((current, distances[current].Distance)::ds) distances distances[current].Previous
    | current -> findStarts ds distances distances[current].Previous

let rec optimize currentMinimum = function
    | [] -> currentMinimum
    | []::tail -> optimize currentMinimum tail
    | (start::rest)::tail ->
        match plumb start with
        | distances when Option.isSome (Map.tryFind stop distances) -> 
            findStarts [] distances stop
            |> fun ds -> List.map (snd >> ((-) distances[stop].Distance)) ds, List.map fst ds
            |> fun (minimum, checked) -> optimize (Seq.min (currentMinimum::minimum)) ((List.except checked rest)::tail)
        | _ ->
            optimize currentMinimum tail

let rec startCollector start = function
    | checked, [] -> [], [start]::checked
    | checked, head::tail -> 
        if List.exists (flip Seq.contains (surroundings start)) head then [], (start::head)::checked @ tail else startCollector start (head::checked, tail)

let trySolve2 c =
    stop <-  Map.findKey (fun _ character -> character = 'E') c
    let firstStart = Map.findKey (fun _ character -> character = 'S') c
    characters <- c |> setGraph firstStart 'a' |> setGraph stop 'z'

    characters
    |> Map.filter (fun _ character -> character = 'a') |> flip (Map.foldBack (fun k _ s -> startCollector k s)) ([], [])
    |> snd |> List.map (List.sortByDescending (fun (x0, y0) -> abs(fst stop - x0) + abs (snd stop - y0))) |> optimize Int32.MaxValue 

let part2 = toMap >> trySolve2

let solution = Solution.build(part1, part2)