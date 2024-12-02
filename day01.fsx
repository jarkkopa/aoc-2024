let input = "inputs/day01.txt" |> System.IO.File.ReadAllLines

let split (withStr: string)(a: string) = a.Split withStr

let splitAndUnzip =
    Array.map (split "  " >> (Array.map int) >> (fun x -> (Array.head x, Array.last x)))
    >> Array.unzip

input
    |> splitAndUnzip
    |> fun (a, b) -> (Array.sort a, Array.sort b)
    ||> Array.zip
    |> Array.sumBy (fun (a, b) -> abs (a - b))
    |> printfn "Part one: %A"

let lengthBy pred = Array.filter pred >> Array.length

input
    |> splitAndUnzip
    |> fun (fstArr, sndArr) -> fstArr |> Array.map (fun a -> (a, sndArr |> lengthBy ((=)a)))
    |> Array.sumBy (fun (a, b) -> a * b)
    |> printfn "Part two: %A"
