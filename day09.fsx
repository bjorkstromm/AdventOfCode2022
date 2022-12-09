type Direction =
    | Up
    | Down
    | Left
    | Right

type Instruction =
    {
        Direction : Direction
        Length : int
    }

type Coordinate =
    {
        X : int
        Y : int
    }
    static member Default = { X = 0; Y = 0 }

type Rope =
    {
        Head : Coordinate
        Tail : Coordinate
    }
    static member Default =
    {
        Head = Coordinate.Default
        Tail = Coordinate.Default
    }

let parseInstructions lines =
    let parseInstruction (line : string) =
        match line.Split(' ') with
        | [|d;l|] ->
            let dir =
                match d with
                | "U" -> Up
                | "D" -> Down
                | "L" -> Left
                | "R" -> Right
                | _ -> failwithf "Invalid direction: %s" d
            let len = l |> int
            { Direction = dir; Length = len }
        | _ -> failwithf "Invalid instruction: %s" line

    lines
    |> Seq.map parseInstruction

let move dir rope =
    let touching (a : Coordinate) (b : Coordinate) =
        abs (a.X - b.X) <= 1 && abs (a.Y - b.Y) <= 1

    let head =
        match dir with
        | Up    -> { rope.Head with Y = rope.Head.Y - 1 }
        | Down  -> { rope.Head with Y = rope.Head.Y + 1 }
        | Left  -> { rope.Head with X = rope.Head.X - 1 }
        | Right -> { rope.Head with X = rope.Head.X + 1 }

    if rope.Tail |> touching head then { rope with Head = head }
    else
        let tail =
            match dir with
            | Up    -> { head with Y = head.Y + 1 }
            | Down  -> { head with Y = head.Y - 1 }
            | Left  -> { head with X = head.X + 1 }
            | Right -> { head with X = head.X - 1 }

        { Head = head; Tail = tail }

let part1 filename =
    let folder (rope, visited) instruction =
        let rec loop rope visited i =
            if i = instruction.Length then (rope, visited)
            else
                let rope' = move instruction.Direction rope
                let visited' = rope'.Tail :: visited
                loop rope' visited' (i + 1)

        loop rope visited 0

    filename
    |> System.IO.File.ReadAllLines
    |> parseInstructions
    |> Seq.fold folder (Rope.Default, [Coordinate.Default])
    |> snd
    |> List.rev
    |> List.distinct
    |> List.length