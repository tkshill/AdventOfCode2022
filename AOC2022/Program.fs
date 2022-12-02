open System.IO
open System.Net.Http

type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | Some x -> f x
        | _ -> None

    member this.Delay(f) = f ()
    member this.Return(x) = Some x
    member this.ReturnFrom(m) = m
    member this.Zero() = None

let maybe = new MaybeBuilder()

let flip f a b = f b a

let days = Map [ ("1", Day1.Solution) ]

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

        let part = Seq.tryItem 1 args
        let! input = getData day
        let! solution = Map.tryFind day days

        let result = solution input

        match part with
        | Some "1" -> printfn "%A" result.Part1
        | Some "2" -> printfn "%A" result.Part2
        | _ ->
            printfn "%A" result.Part1
            printfn "%A" result.Part2
    }
    |> ignore

    0
