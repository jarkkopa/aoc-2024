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
    |> Array.map (split "," >> Array.map int)

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

let getCenterItem (items: int array) = items.[items.Length / 2]

updates
|> Array.filter isValid
|> Array.sumBy getCenterItem
|> printfn "Part one: %A"

let comparePages a b =
    let aRules = rules |> Map.tryFind a 
    let bRules = rules |> Map.tryFind b
    let aFirst =
        match aRules with
        | Some r -> r |> Array.contains b
        | _ -> false
    let bFirst =
        match bRules with
        | Some r -> r |> Array.contains a 
        | _ -> false
    match (aFirst, bFirst) with
    | (true, _) -> -1
    | (_, true) -> 1
    | _ -> 0

let invalidUpdates =
    updates
    |> Array.filter (fun x -> not (isValid x))
    |> Array.map (Array.sortWith comparePages)
    |> Array.sumBy getCenterItem
    |> printfn "Part two: %A"