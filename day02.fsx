type Shape =
    | Rock
    | Paper
    | Scissors

type ShapeMap = Map<char, Shape>

type Round = Shape * Shape

type Outcome =
    | Win
    | Draw
    | Loss

type OutcomeMap = Map<char, Outcome>

let pointShape shape =
    match shape with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let pointOutcome outcome =
    match outcome with
    | Win -> 6
    | Draw -> 3
    | Loss -> 0

let points shape outcome =
    pointShape shape + pointOutcome outcome

let parse (lmap : ShapeMap) (rmap : ShapeMap) (line : string) =
    let parts = line.ToCharArray()
    let l = lmap.[parts.[0]]
    let r = rmap.[parts.[2]]
    Round (l, r)

let parseFile (lmap : ShapeMap) (rmap : ShapeMap) (filename : string) =
    filename
    |> System.IO.File.ReadAllLines
    |> Seq.map (parse lmap rmap)

let playRound (round : Round) =
    let (l, r) = round
    let outcome =
        match round with
        | (Rock, Rock) -> Draw
        | (Rock, Paper) -> Win
        | (Rock, Scissors) -> Loss
        | (Paper, Rock) -> Loss
        | (Paper, Paper) -> Draw
        | (Paper, Scissors) -> Win
        | (Scissors, Rock) -> Win
        | (Scissors, Paper) -> Loss
        | (Scissors, Scissors) -> Draw

    outcome |> points r


let part1 (filename : string) =
    let lmap = Map.ofList [('A', Rock); ('B', Paper); ('C', Scissors)]
    let rmap = Map.ofList [('X', Rock); ('Y', Paper); ('Z', Scissors)]

    filename
    |> parseFile lmap rmap
    |> Seq.map playRound
    |> Seq.sum

// Part 2

let parse2 (lmap : ShapeMap) (rmap : OutcomeMap) (line : string) =
    let parts = line.ToCharArray()
    let l = lmap.[parts.[0]]
    let o = rmap.[parts.[2]]

    let r =
        match (l, o) with
        | (Rock, Win) -> Paper
        | (Rock, Draw) -> Rock
        | (Rock, Loss) -> Scissors
        | (Paper, Win) -> Scissors
        | (Paper, Draw) -> Paper
        | (Paper, Loss) -> Rock
        | (Scissors, Win) -> Rock
        | (Scissors, Draw) -> Scissors
        | (Scissors, Loss) -> Paper

    Round (l, r)

let parseFile2 (lmap : ShapeMap) (rmap : OutcomeMap) (filename : string) =
    filename
    |> System.IO.File.ReadAllLines
    |> Seq.map (parse2 lmap rmap)

let part2 (filename : string) =
    let lmap = Map.ofList [('A', Rock); ('B', Paper); ('C', Scissors)]
    let rmap = Map.ofList [('X', Loss); ('Y', Draw); ('Z', Win)]

    filename
    |> parseFile2 lmap rmap
    |> Seq.map playRound
    |> Seq.sum
