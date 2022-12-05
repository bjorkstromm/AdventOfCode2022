open System.Collections.Generic
open System.Text.RegularExpressions

type MoveOp = {
    Count: int
    From: int
    To: int
}

let createInitial lines =
    let head = lines |> List.head

    let map =
        head
        |> List.mapi (fun n _ -> n + 1, new Stack<char>())
        |> Map.ofList

    lines
    |> List.tail
    |> List.iter (fun line ->
        line
        |> List.iteri (fun n c ->
            match c with
            | Some c -> map.[n + 1].Push(c)
            | None -> ()))

    map

let parseInitial lines =
    let rec loop acc xs =
        let chars = xs |> Seq.truncate 4 |> Seq.toArray
        match chars with
        | [|'[';c;']';' '|] ->
            let tail = xs |> Seq.skip 4 
            loop (Some c::acc) tail
        | [|'[';c;']'|] ->
            ((Some c)::acc) |> List.rev
        | [|' ';' ';' ';' '|] ->
            let tail = xs |> Seq.skip 4 
            loop (None::acc) tail
        | [|' ';' ';' '|] ->
            (None::acc) |> List.rev
        | [|' ';n;' ';' '|] ->
            let tail = xs |> Seq.skip 4 
            loop (Some n::acc) tail
        | [|' ';n;' '|] ->
            (Some n::acc) |> List.rev
        | _ -> acc |> List.rev

    let initialLines =
        lines
        |> Seq.takeWhile (fun line -> line <> "")
        |> Seq.map (loop [])
        |> Seq.toList

    let rest = 
        lines
        |> Array.skip (initialLines.Length + 1)

    let initial =
        initialLines
        |> List.rev
        |> createInitial

    (initial, rest)

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseMoveOp str =
    match str with
    | Regex @"^move ([0-9]+) from ([0-9]+) to ([0-9]+)$" [count;from;to'] ->
        { Count = int count; From = int from; To = int to' }
    | _ -> failwithf "Invalid line: %s" str

let parse filename =
    let (initial, rest) =
        filename
        |> System.IO.File.ReadAllLines
        |> parseInitial

    let moves =
        rest
        |> Seq.map parseMoveOp

    (initial, moves)

let exec (m : Map<int,Stack<char>>) op =
    let count = op.Count
    let from = op.From
    let to' = op.To

    let rec loop count =
        if count > 0 then
            let c = m.[from].Pop()
            m.[to'].Push(c)
            loop (count - 1)

    loop count
    m

let part1 filename =
    let m, ops =
        filename
        |> parse

    ops
    |> Seq.fold exec m
    |> Map.toSeq
    |> Seq.sortBy (fun (n,_) -> n)
    |> Seq.map (fun (_,s) -> s.Peek())
    |> Seq.toArray
    |> System.String