type Elf = {
    Low : int
    High : int
}

let parseElf (str : string) =
    match str.Split('-') with
    | [|low; high|] -> { Low = int low; High = int high }
    | _ -> failwith "Invalid input"

let parsePair (str : string) =
    match str.Split(',') with
    | [|left;right|] -> (parseElf left, parseElf right)
    | _ -> failwith "Invalid input"

let parseInput filename =
    filename
    |> System.IO.File.ReadAllLines
    |> Seq.map parsePair

let fullyContains l r =
    l.Low <= r.Low && r.High <= l.High

let part1 filename =
    filename
    |> parseInput
    |> Seq.filter (fun (l, r) ->
        fullyContains l r || fullyContains r l)
    |> Seq.length

// Part 2
let overlaps l r =
    (l.Low >= r.Low && l.Low <= r.High)
    || (l.High >= r.Low && l.High <= r.High)

let part2 filename =
    filename
    |> parseInput
    |> Seq.filter (fun (l, r) ->
        overlaps l r || overlaps r l)
    |> Seq.length