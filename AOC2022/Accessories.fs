module Utility

type SolutionRecord = { Part1: string; Part2: string }
type Solution = string seq -> SolutionRecord

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

let tupleMap f (a, b) = (f a, f b)

let next s value =
    let index = Seq.findIndex (fun v -> v = value) s

    if index = Seq.length s - 1 then
        Seq.head s
    else
        Seq.item (index + 1) s

let endsToTuple sequence = (Seq.head sequence, Seq.last sequence)

let split chars (s: string) = s.Split(chars)

let splitBy condition sequence =
    (Seq.takeWhile condition sequence, Seq.skipWhile condition sequence)

let dropLast sequence =
    Seq.removeAt (Seq.length sequence - 1) sequence

let rec trimEnds sequence =
    match Seq.toList sequence with
    | [] -> Seq.empty
    | [ x ] when x = "" -> Seq.empty
    | head :: tail when head = "" -> trimEnds (Seq.ofList tail)
    | x when Seq.last x = "" -> trimEnds (dropLast x)
    | _ -> sequence

let unpack f (a, b) = f a b
