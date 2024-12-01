let input = "inputs/day01.txt" |> System.IO.File.ReadAllLines

let split (withStr: string)(a: string) = a.Split withStr

let dist (a,b) =
    a-b
    |> abs

let part1 =
    input
    |> Array.map (split "  " >> (Array.map int) >> (fun x -> (Array.head x, Array.last x)))
    |> Array.unzip
    |> fun (a, b) -> (Array.sort a, Array.sort b)
    |> fun (a, b) -> Array.zip a b
    |> Array.sumBy dist

part1 |> printfn "Part one: %A"