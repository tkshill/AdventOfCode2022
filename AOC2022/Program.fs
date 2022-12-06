open System.IO
open System.Net.Http
open Utility

let getSolution (input: string seq) : string -> SolutionRecord option =
    function
    | "1" -> Some(Day1.solution input)
    | "2" -> Some(Day2.solution input)
    | "3" -> Some(Day3.solution input)
    | "4" -> Some(Day4.solution input)
    | "5" -> Some(Day5.solution input)
    | _ -> None


let getData input =
    let fileName = $"./day{input}.txt"

    if File.Exists(fileName) then
        Some(File.ReadAllLines fileName)
    else
        // task {
        //     use client = new HttpClient()

        // client.DefaultRequestHeaders.Authorization <-
        //     Headers.AuthenticationHeaderValue()
        //     let! response = client.GetStringAsync($"https://adventofcode.com/2022/day/{input}")
        //     do! File.WriteAllTextAsync($"./day{input}.txt", response)
        // }
        // |> Async.AwaitTask
        // |> Async.RunSynchronously

        // if File.Exists(fileName) then
        //     Some(File.ReadAllLines fileName)
        // else
        None

[<EntryPoint>]
let main args =
    maybe {
        let! day = Seq.tryHead args
        let! part = Seq.tryItem 1 args
        let! input = getData day
        let! solution = getSolution input day

        match part with
        | "1" -> printfn "%A" solution.Part1
        | "2" -> printfn "%A" solution.Part2
        | _ ->
            printfn "%A" solution.Part1
            printfn "%A" solution.Part2
    }
    |> ignore

    0
