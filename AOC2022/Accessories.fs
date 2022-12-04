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

let endsToTuple line = (Seq.head line, Seq.last line)

let split chars (s: string) = s.Split(chars)
