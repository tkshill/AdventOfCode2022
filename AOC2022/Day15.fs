module Day15

open FParsec
open FParsec.Pipes

let pL =
    %% "Sensor at x=" -- +.(pT ", y=") -- ": closest beacon is at " -- +.(pT ", y=")
    -%> auto

let part1 = runParser pL >> 

let solution = Solution.build (part1)
