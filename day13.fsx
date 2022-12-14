type PacketData =
    | Integer of int
    | List of PacketData list
    override this.ToString() =
        match this with
        | Integer x -> x.ToString()
        | List x -> "[" + System.String.Join(",", x) + "]"

let parsePacketData (data : string) =
    let rec loop (acc : PacketData list, data : char list) =
        match data with
        | [] -> (acc |> List.rev, [])
        | '[' :: tail ->
            let (list, rest) = loop ([], tail)
            let acc = List list :: acc
            loop (acc, rest)
        | ']' :: rest -> (acc |> List.rev, rest)
        | ',' :: rest -> loop (acc, rest)
        | _ ->
            let num =
                data
                |> List.takeWhile (fun x -> x <> ',' && x <> ']')
                |> List.toArray
                |> System.String

            let rest =
                data
                |> List.skip (num.Length)

            let acc = Integer (num |> int) :: acc
            loop (acc, rest)

    ([], data.ToCharArray() |> List.ofArray)
    |> loop
    |> fst
    |> List.head

let compare left right =
    let rec loop left right =
        match left, right with
        | [], [] -> 0
        | [], _  -> -1
        | _, []  -> 1
        | Integer x :: left, Integer y :: right ->
            if x > y then 1
            elif x < y then -1
            else loop left right
        | List x :: left, List y :: right ->
            match loop x y with
            | 0 -> loop left right
            | i -> i
        | Integer x :: left, List y :: right ->
            loop (List [Integer x]::left) (List y::right)
        | List x :: left, Integer y :: right ->
            loop (List x::left) (List [Integer y]::right)

    loop [left] [right]

let parsePacketDataPairs filename =
    let rec loop acc (lines : string []) =
        if lines.Length = 0 then
            acc |> List.rev
        else
            let left = lines.[0] |> parsePacketData
            let right = lines.[1] |> parsePacketData
            if lines.Length < 3 then
                [||] |> loop ((left, right) :: acc)
            else
                loop ((left, right) :: acc) (lines |> Array.skip 3)
    filename
    |> System.IO.File.ReadAllLines
    |> loop []

let part1 filename =
    filename
    |> parsePacketDataPairs
    |> List.mapi (fun i (left, right) ->
        let result = compare left right
        (i+1, result))
    |> List.filter (fun (_, result) -> result = -1)
    |> List.sumBy fst

let part2 filename =
    let div1 = List [ List [ Integer 2 ] ]
    let div2 = List [ List [ Integer 6 ] ]
    let sorted =
        filename
        |> parsePacketDataPairs
        |> Seq.map (fun (left, right) -> [left; right])
        |> Seq.concat
        |> Seq.append [div1;div2]
        |> Seq.sortWith compare
        |> Seq.toArray

    let idx1 = sorted |> Array.findIndex (fun x -> x = div1) |> (+) 1
    let idx2 = sorted |> Array.findIndex (fun x -> x = div2) |> (+) 1

    idx1 * idx2

"day13.txt" |> part2