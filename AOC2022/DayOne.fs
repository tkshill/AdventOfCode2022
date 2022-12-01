module Day1

open System.IO

let folder (head :: tail) line =
    if line = "" then
        0 :: head :: tail
    else
        (int line + head) :: tail

let part1 input =
    input |> File.ReadAllLines |> Seq.fold folder [ 0 ] |> Seq.max

let part2 input =
    input
    |> File.ReadAllLines
    |> Seq.fold folder [ 0 ]
    |> Seq.sort
    |> Seq.rev
    |> Seq.take 3
    |> Seq.sum
