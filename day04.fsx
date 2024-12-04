type Pos = { x: int; y: int; }
type Dir = N | NW | W | SW | S | SE | E | NE

let directions = [ N; NW ; W ; SW ; S ;SE ; E; NE]

let input = "inputs/day04.txt" |> System.IO.File.ReadAllLines |> List.ofArray
let rows = input |> List.length
let cols = input.[0] |> Seq.length
let inputPadded =
    input
    |> List.append (List.init 4 (fun x -> String.replicate cols "."))
    |> (fun x -> List.append x (List.init 4 (fun x -> String.replicate cols ".")))
    |> List.map (fun x -> x.PadLeft(rows + 4, '.').PadRight(rows + 8, '.'))

let coords =
    inputPadded
    |> Seq.indexed
    |> Seq.map (fun (i, s) -> (i, s |> Seq.indexed |> List.ofSeq))
    |> Seq.map (fun (y, line) -> line |> Seq.map (fun (x, c) -> ({x = x; y = y}, c)))
    |> Seq.collect id
    |> Map.ofSeq

let nextExpected prev =
    match prev with
    | '.' -> 'X'
    | 'X' -> 'M'
    | 'M' -> 'A'
    | 'A' -> 'S'
    | s -> failwith $"Error trying to find next for {s}"

let nextPos pos dir =
    match dir with
    | N -> { pos with y = pos.y - 1 }
    | NW -> { x = pos.x - 1; y = pos.y - 1 }
    | W -> { pos with x = pos.x - 1 }
    | SW -> { x = pos.x - 1; y = pos.y + 1 }
    | S -> { pos with y = pos.y + 1 }
    | SE -> { x = pos.x + 1; y = pos.y + 1 }
    | E -> { pos with x = pos.x + 1 }
    | NE -> { x = pos.x + 1; y = pos.y - 1}



let rec isValid  (dir: Dir) (prev: char) (pos: Pos) =
    let expected = nextExpected prev
    let current = coords[pos]
    if current <> expected then false
    else if current = 'S' then true
    else isValid dir current (nextPos pos dir)

coords
|> Map.toList
|> List.map (fun (pos, c) ->
    directions
    |> List.map (fun d -> isValid d '.' pos)
)
|> List.collect id
|> List.filter (fun x -> x)
|> List.length
|> printfn "Part one: %A"
