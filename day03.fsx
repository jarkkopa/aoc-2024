open System.Text.RegularExpressions
let input = "inputs/day03.txt" |> System.IO.File.ReadAllText

let mulRegex = Regex(@"mul\((\d{1,3}),(\d{1,3})\)");
let doRegex = Regex(@"(do\(\))")
let dontRegex = Regex(@"(don\'t\(\))")

let groupValues (x: Match) =
    x.Groups
    |> Seq.tail
    |> Seq.map (_.Value >> int) 

let instructions (matches: MatchCollection)=
    matches
    |> Seq.map groupValues

let calcResult x = x |> Seq.map (Seq.reduce (fun a c -> a * c)) |> Seq.sum

input
|> mulRegex.Matches
|> instructions
|> calcResult
|> printfn "Part one: %A"

let ends =
    input
    |> dontRegex.Matches
    |> Seq.map _.Index
    |> (fun e -> [(input |> Seq.length)] |> Seq.append e)

let starts =
    input
    |> doRegex.Matches
    |> Seq.map _.Index
    |> Seq.append [0]

let findEnd (start: int) = Seq.find (fun e -> e > start)
let ranges =
    starts
    |> Seq.map (fun s -> (s, (findEnd s ends)))
    |> Seq.distinctBy snd

ranges
|> Seq.map ((fun (s, e) -> input.Substring(s, e - s)) >> mulRegex.Matches >> instructions)
|> Seq.concat
|> calcResult
|> printfn "Part two: %A"