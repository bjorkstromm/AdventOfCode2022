open System
open System.IO

let sums filename = 
    let folder acc row =
        if row = "" then []::acc
        else
            let n = int row
            let head = List.head acc
            let tail = List.tail acc

            (n::head)::tail

    filename
    |> File.ReadAllLines
    |> Seq.fold folder [[]]
    |> Seq.map (List.sum)

let part1 filename =
    filename
    |> sums
    |> Seq.max

let part2 filename =
    filename
    |> sums
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum