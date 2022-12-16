type Coordinate =
    {
        X : int
        Y : int
    }

type Sensor =
    {
        Position : Coordinate
        ClosestBeacon : Coordinate
        Strength : int
    }

let (|Regex|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let parseSensor line =
    match line with
    | Regex @"^Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)$"
            [x;y;vx;vy] ->
            let x = x |> int
            let y = y |> int
            let vx = vx |> int
            let vy = vy |> int
            let strength = abs (vx - x) + abs (vy - y)
            { 
                Position = { X = x; Y = y }
                ClosestBeacon = { X = vx; Y = vy }
                Strength = strength
            }
    | _ -> failwithf "Invalid line: %s" line

let parseSensors lines =
    lines
    |> Array.map parseSensor

let parseSensorFile filename =
    filename
    |> System.IO.File.ReadAllLines
    |> parseSensors

let coverage line sensors =
    let reservedPos =
        sensors
        |> Array.map (fun s -> [s.Position; s.ClosestBeacon])
        |> List.concat
        |> Set.ofList

    let folder acc sensor =
        [-sensor.Strength .. sensor.Strength]
        |> List.choose (fun dx ->
            let (x,y) = sensor.Position.X + dx, line
            let len = abs (x - sensor.Position.X) + abs (y - sensor.Position.Y)

            let coord = { X = x; Y = y }
            let isFree = not (reservedPos.Contains coord)

            if len <= sensor.Strength && y = line && isFree then
                Some { X = x; Y = y }
            else
                None)
        |> List.append acc

    sensors
    |> Array.fold folder []
    |> List.distinct


let part1 filename line =
    filename
    |> parseSensorFile
    |> coverage line
    |> List.length

part1 "test.txt" 10 |> printfn "Part 1: %d"
part1 "day15.txt" 2000000 |> printfn "Part 1: %d"