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

//part1 "test.txt" 10 |> printfn "Part 1: %d"
//part1 "day15.txt" 2000000 |> printfn "Part 1: %d"

// Part 2

let outsidePoints sensor =
    seq {
        let max = sensor.Strength + 1
        let (x,y) = (sensor.Position.X, sensor.Position.Y)
        for i = max downto 1 do
            let x1 = x + max - i
            let x2 = x1 * (-1)
            let x3 = x - i
            let x4 = x3 * (-1)
            let y1 = y - i
            let y2 = y1 * (-1)
            let y3 = y + max - i
            let y4 = y3 * (-1)
            yield (x1,y1)
            yield (x2,y2)
            yield (x3,y4)
            yield (x4,y3)
    }

let findBeacon max sensors =
    let mutable coordinate = (0,0)
    sensors
    |> Array.tryFind (fun sensor ->
        sensor
        |> outsidePoints
        |> Seq.tryFind (fun (x,y) ->
            if x < 0 || x > max || y < 0 || y > max then false
            else
                let found =
                    sensors
                    |> Array.forall (fun s ->
                        let len = abs (x - s.Position.X) + abs (y - s.Position.Y)
                        len > s.Strength)
                if found then
                    coordinate <- (x, y)
                found)
        |> Option.isSome)
    |> ignore
    coordinate


let part2 filename max =
    filename
    |> parseSensorFile
    |> findBeacon max
    |> (fun (x,y) -> ((x |> uint64) * 4000000UL) + (y |> uint64))

part2 "test.txt" 20// |> printfn "Part 2: %A"
part2 "day15.txt" 4000000// |> printfn "Part 1: %d"