let split (splitWith: string) (str: string) = str.Split(splitWith)

let input = "inputs/day05.txt" |> System.IO.File.ReadAllText |> split "\n\n"

let rules =
    input.[0]
    |> split "\n"
    |> Array.map (split "|")
    |> Array.groupBy Array.head
    |> Array.map (fun (key, afters) -> (int key, afters |> Array.map (Array.last >> int)))
    |> Map.ofArray

let updates =
    input.[1]
    |> split "\n"
    |> Array.map (split ",")
    |> Array.map (Array.map int)

let pagesBefore row value =
    let splitAt = row |> Array.findIndex ((=)value)
    row |> Array.take splitAt

let containsAny target values =
    (false, target)
    ||> Array.fold(fun a c -> (values |> Array.contains c) || a)

let isValid row =
    (true, row)
    ||> Array.fold (fun a c ->
        let before = pagesBefore row c
        let forbidden = if rules.ContainsKey c then rules[c] else [||]
        not(before |> containsAny forbidden) && a
    )

let getCenterItem items =
    let center = (items |> Array.length) / 2
    items.[center]

updates
|> Array.filter isValid
|> Array.map getCenterItem
|> Array.sum
|> printfn "Part one: %A"