module Day15

open FParsec.Pipes

let pL = %% "Sensor at x=" -- +.(pT ", y=") -- ": closest beacon is at x=" -- +.(pT ", y=") -%> auto

let manhattanDistance (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)

let rec solve yLine (beaconsOnLine, sensorSpans) ((xSensor, ySensor),(xBeacon, yBeacon)) =
    let xSpan = manhattanDistance (xSensor, ySensor) (xBeacon, yBeacon) - abs (yLine - ySensor)
    (if yBeacon = yLine then [xBeacon] else []) @ beaconsOnLine, [ (xSensor - xSpan)..(xSensor + xSpan) ] @ sensorSpans

let part1 yLine = runParser pL >> Seq.fold (solve yLine) ([], []) >> unpack List.except >> List.length

let manhattanEdges limit ((x, y), md) =
    let withinLimit v = v >= 0 && v <= limit
    seq { for i in x - md..x + md ->
            match i, y + md - abs (i - x), y - (md - abs (i - x)) with
            | i, _, _ when not (withinLimit i) -> []
            | i, y0, y1 when y0 <> y1 && withinLimit y0 && withinLimit y1 -> [(i, y0); (i, y1)]
            | i, y0, _ when withinLimit y0 -> [(i, y0)]
            | i, _, y1 when withinLimit y1 -> [(i, y1)] } |> List.concat

let solve2 limit (beaconsAndMDs) = 
    Seq.collect ((tupleMap id ((+) 1)) >> manhattanEdges limit) beaconsAndMDs
    |> Seq.find (fun unReachable -> Seq.forall (fun (beacon, md) -> manhattanDistance beacon unReachable > md) beaconsAndMDs)

let part2 limit = runParser pL >> Seq.map (tupleFold id manhattanDistance) >> solve2 limit >> tupleMap int64 int64 >> tupleFold ((*) 4000000L) (+) >> snd

let solution = Solution.build (part1 2000000, part2 4000000)
