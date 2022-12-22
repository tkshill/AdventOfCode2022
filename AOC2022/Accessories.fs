[<AutoOpen>]
module Utility

open System.IO
open FParsec
open FParsec.Pipes
open FSharpx.Collections

let flip f a b = f b a

type Part = (string[]) -> string

let dummyPart: Part = fun _ -> failwith "Not implemented yet"

type Solution(input: string[], firstPart: Part, secondPart: Part) =
    let part1 = firstPart
    let part2 = secondPart
    let data = input
    member this.Part1(?input) = defaultArg input data |> part1
    member this.Part2(?input) = defaultArg input data |> part2

    static member build(?firstPart, ?secondPart) =
        let part1 = firstPart |> Option.map (flip (>>) string) |> flip defaultArg dummyPart
        let part2 = secondPart |> Option.map (flip (>>) string) |> flip defaultArg dummyPart
        fun input -> Solution(input, part1, part2)

type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | Some x -> f x
        | _ -> None

    member this.Zero() = None

let maybe = new MaybeBuilder()

let next s value =
    let index = Seq.findIndex (fun v -> v = value) s

    if index = Seq.length s - 1 then
        Seq.head s
    else
        Seq.item (index + 1) s

let seqToTuple sequence = (Seq.head sequence, Seq.last sequence)

let splitByChars (chars) (s: string) = s.Split(chars)
let splitByString (str: string) (s: string) = s.Split(str)

let splitBy condition sequence =
    (Seq.takeWhile condition sequence, Seq.skipWhile condition sequence)

let dropLast sequence =
    Seq.removeAt (Seq.length sequence - 1) sequence

let rec trimEnds sequence =
    match sequence with
    | [||]
    | [| "" |] -> Array.empty
    | x when Array.head x = "" -> trimEnds (Array.tail x)
    | x when Array.last x = "" -> trimEnds (x[.. (Array.length x - 2)])
    | _ -> sequence

let unpack f (a, b) = f a b

let pack f a b = f (a, b)

let notEqualTo value = ((<>) value)

let tupleMap f f2 (a, b) = (f a, f2 b)

let isnt f = f >> not

type Collections.List<'a> with

    static member dropLast(ls: 'a list) = List.removeAt (Seq.length ls - 1) ls

let cond condition ifTrue ifFalse = if condition then ifTrue else ifFalse

let seqFrom2D (a: 'a[,]) : seq<'a> =
    seq {
        for x in a do
            yield x :?> 'a
    }

let boolToInt b = if b then 1 else 0

let log transformer value =
    printfn "%A" (transformer value)
    value

let inc v = v + 1
let dec v = v - 1

let getInput dayNumber =
    let fileName = $"./data/day{dayNumber}.txt"

    if File.Exists(fileName) then
        Some(File.ReadAllLines fileName |> trimEnds)
    else
        None

let integerParser: Parser<int, unit> =
    %% +.(qty.[1..] * digit)
    -|> (ResizeArray.toArray >> System.String >> System.Int32.Parse)

let parserResultToOption =
    function
    | Success(value, _, _) -> Some value
    | _ -> None

let indexWithOffset (n: int) (s: 'a seq) =
    s |> Seq.indexed |> Seq.map (tupleMap ((+) n) id)

let runParser parser = run parser >> parserResultToOption

// type Log'<'a>(value: 'a, ?transformer: 'a -> 'b, ?preString: string, ?postString: string) =
//     do this.say(value, transformer, preString, postString)

//     member this.say() =
//         printfn "%A" $"{preString}{transformer (value)}{postString}"

let withEffect (f: 'a -> unit) (v: 'a) =
    f v
    v

let toTuple a b = a, b

let trd (_, _, z) = z
