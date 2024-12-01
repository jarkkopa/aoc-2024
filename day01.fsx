let input = "inputs/day01.txt" |> System.IO.File.ReadAllLines

let split (withStr: string)(a: string) = a.Split withStr

let dist (a,b) =
    a-b
    |> abs

let part1 =
    input
    |> Array.map (split "  ")
    |> Array.map (Array.map int)
    |> Array.map (fun x -> (Array.head x, Array.last x))
    |> Array.unzip
    |> (fun x -> (x |> fst |> Array.sort, x |> snd |> Array.sort))
    |> (fun x -> Array.zip (fst x) (snd x))
    |> Array.sumBy dist

part1 |> printfn "Part one: %A"