let input = "inputs/day02.txt" |> System.IO.File.ReadAllLines

let reports = 
    input
    |> Array.map (fun x -> x.Split(" "))
    |> Array.map (Array.map int)

let isSafe (arr: int array) =
    let trends =
        arr
        |> Array.pairwise
        |> Array.map (fun (a, b) -> a - b)
    let dir = trends |> Array.head |> sign
    trends
        |> Array.forall (fun x -> (abs x) <= 3 && (sign x) = dir)
        

reports
|> Array.map isSafe
|> Array.where ((=) true)
|> Array.length
|> printfn "Part one: %A"

let allReports reports =
    reports 
    |> Array.mapi (fun i _ -> reports |> Array.removeAt i)
    |> Array.append [| reports |]

reports
|> Array.map allReports
|> Array.map (Array.map isSafe)
|> Array.map (Array.tryFind ((=) true))
|> Array.where (fun x -> x.IsSome)
|> Array.length
|> printfn "Part two: %A"
