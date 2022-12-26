module Day15

open FParsec.Pipes

let pL = %% "Sensor at x=" -- +.(pT ", y=") -- ": closest beacon is at x=" -- +.(pT ", y=") -%> auto

let rec manhattan yLine (beacons, sensorSpans) ((xSensor, ySensor),(xBeacon, yBeacon)) =
    let xSpan = abs (ySensor - yBeacon) + abs (xSensor - xBeacon) - abs (yLine - ySensor)
    let sensorSpan = [ (xSensor - xSpan)..(xSensor + xSpan) ]
    (if yBeacon = yLine then [xBeacon] else []) @ beacons, sensorSpan @ sensorSpans

let part1 y = runParser pL >> Seq.fold (manhattan y) ([], []) >> unpack List.except >> log List.sort >> List.length

let solution = Solution.build (part1 2000000)
