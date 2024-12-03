open System.Text.RegularExpressions
let input = "inputs/day03.txt" |> System.IO.File.ReadAllText

let mulRegex = Regex(@"mul\((\d{1,3}),(\d{1,3})\)");

let groupValues (x: Match) =
    x.Groups
    |> Seq.tail
    |> Seq.map ((fun x -> x.Value) >> int)

input
|> mulRegex.Matches
|> Seq.map  groupValues
|> Seq.map (Seq.reduce (fun acc cur -> acc * cur))
|> Seq.sum
|> printfn "Part one: %A"
