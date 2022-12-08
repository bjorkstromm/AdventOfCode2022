let parseMap filename =
    filename
    |> System.IO.File.ReadAllLines
    |> Array.map (fun line ->
        line.ToCharArray()
        |> Array.map (fun c -> c |> System.Char.GetNumericValue |> int))
    |> array2D

let isVisible map (x,y) =
    let end' = (map |> Array2D.length1) - 1 // It's square, so length1 = length2

    if x = 0 || y = 0 || x = end' || y = end' then true
    else
        let h = map.[x,*]
        let v = map.[*,y]

        let height = map.[x,y]

        let hl = h.[0..(max 0 (y-1))] |> Array.max
        let hr = h.[y+1..] |> Array.max
        let vl = v.[0..(max 0 (x-1))] |> Array.max
        let vr = v.[x+1..] |> Array.max

        height > hl || height > hr || height > vl || height > vr


let mapVisible map =
    let end' = (map |> Array2D.length1) - 1 // It's square, so length1 = length2

    [|0..end'|]
    |> Array.map (fun x ->
        [|0..end'|]
        |> Array.map (fun y -> (isVisible map (x,y), map.[x,y])))
    |> array2D

let part1 filename =
    filename
    |> parseMap
    |> mapVisible
    |> Seq.cast<(bool * int)>
    |> Seq.filter (fun (visible, _) -> visible)
    |> Seq.length

"day08.txt" |> part1