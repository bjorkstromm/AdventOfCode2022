open System
open System.IO

let sums filename = 
    let folder acc row =
        if row = "" then 0::acc
        else
            let n = int row
            let head = List.head acc
            let tail = List.tail acc

            (n+head)::tail

    filename
    |> File.ReadAllLines
    |> Seq.fold folder [0]

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