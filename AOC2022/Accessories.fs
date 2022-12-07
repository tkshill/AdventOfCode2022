module Utility

type Solution = { Part1: string; Part2: string }
type SolutionBuilder = string seq -> Solution

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
    match sequence with
    | [||]
    | [| "" |] -> Array.empty
    | x when Array.head x = "" -> trimEnds (Array.tail x)
    | x when Array.last x = "" -> trimEnds (x[.. (Array.length x - 2)])
    | _ -> sequence

let unpack f (a, b) = f a b

let notEqualTo value = ((<>) value)

let tupleMap2 f f2 (a, b) = (f a, f2 b)

let isnt f = f >> not

type Collections.List<'a> with

    static member dropLast(ls: 'a list) = List.removeAt (Seq.length ls - 1) ls

let ternary condition ifTrue ifFalse = if condition then ifTrue else ifFalse
