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

let cond condition ifTrue ifFalse = if condition then ifTrue else ifFalse

let to2Darray seq =
    let rows = Seq.length seq
    let columns = seq |> Seq.head |> Seq.length
    let array = Array2D.zeroCreate rows columns
    Seq.iteri (fun idx row -> Seq.iteri (fun jdx value -> Array2D.set array idx jdx value) row) seq
    array

let getAllElements (a: 'a[,]) : seq<'a> =
    seq {
        for x in a do
            yield x :?> 'a
    }

let boolToInt b = if b then 1 else 0

let log transformer value =
    printfn "%A" (transformer value)
    value
