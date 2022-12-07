module Day7

open Utility

let (|Prefix|_|) (p: string) (s: string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

type FileSystemItem =
    | File of File
    | Directory of Directory

and File =
    { Name: string
      Size: int }

    static member size(f) = f.Size

and Directory =
    { Name: string
      Items: FileSystemItem list }

let (|MakeDirectory|MoveUp|MakeFile|Skip|ReturnHome|) =
    function
    | Prefix "$ cd " name when name = "/" -> ReturnHome
    | Prefix "$ cd " name when name = ".." -> MoveUp
    | Prefix "$ cd " name -> MakeDirectory name
    | Prefix "dir " _
    | "$ ls" -> Skip
    | file -> MakeFile(file.Split([| ' ' |]) |> endsToTuple |> tupleMap2 int id)

let mutable instructions: string array = Array.empty

let rec build (dir: Directory) =
    function
    | Skip -> advance dir
    | ReturnHome when dir.Name = "/" -> advance dir
    | ReturnHome
    | MoveUp -> dir
    | MakeFile(size, name) -> advance { dir with Items = (File { Name = name; Size = size } :: dir.Items) }
    | MakeDirectory name ->
        advance { dir with Items = (Directory(advance { Name = name; Items = List.Empty })) :: dir.Items }

and advance dir =
    if Array.isEmpty instructions then
        dir
    else
        let next = Array.head instructions
        instructions <- Array.tail instructions
        build dir next

let mutable part1sum = 0

let rec cataFS fFile fDir =
    function
    | File file -> fFile file
    | Directory dir -> fDir (dir, List.map (cataFS fFile fDir) dir.Items)

let under100k (_, sizes) =
    let total = List.sum sizes

    if total <= 100000 then
        part1sum <- part1sum + total
        total
    else
        total

let part1 input =
    instructions <- input

    { Name = "/"; Items = [] }
    |> advance
    |> Directory
    |> cataFS File.size under100k
    |> ignore

    part1sum

let mutable directorySizes: (string * int) list = List.empty

let collectDirectorySizes (dir, sizes) =
    directorySizes <- (dir.Name, List.sum sizes) :: directorySizes
    List.sum (sizes)

let part2 input =
    instructions <- input

    let totalsize =
        { Name = "/"; Items = [] }
        |> advance
        |> Directory
        |> cataFS File.size collectDirectorySizes

    directorySizes
    |> List.filter (fun dir -> snd dir > (30000000 - (70000000 - totalsize)))
    |> List.minBy snd
    |> snd

let solution input =
    { Part1 = part1 input |> string
      Part2 = part2 input |> string }
