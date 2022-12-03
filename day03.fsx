
let rucksackItems filename =
    filename
    |> System.IO.File.ReadAllLines

let splitRucksackItems (line : string) =
    let [|fst;snd|] =
        line.ToCharArray()
        |> Array.splitInto 2
    (fst, snd)

let findCommonItem (c1, c2) =
    c1
    |> Seq.filter (fun c -> c2 |> Seq.contains c)
    |> Seq.head

let getPriority item =
    let isLower = item |> System.Char.IsLower
    let (offset, diff) = if isLower then 1,97 else 27,65

    (int item) - diff + offset

let part1 filename =
    filename
    |> rucksackItems
    |> Seq.map (splitRucksackItems >> findCommonItem >> getPriority)
    |> Seq.sum

"test.txt" |> part1