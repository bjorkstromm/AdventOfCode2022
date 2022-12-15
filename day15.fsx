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

type Tile =
    | Empty
    | SensorCoverage
    | Beacon
    | Sensor of Sensor

type TileMap =
    {
        Map : Tile[,]
        MinX : int
        MinY : int
    }
    with
        member this.Width = this.Map |> Array2D.length1
        member this.Height = this.Map |> Array2D.length2

let printMap (map : TileMap) =
    printf "    "
    for x in 0 .. map.Width - 1 do
        printf "%d" (abs(map.MinX + x) % 10)
    printfn ""
    for y in 0 .. (map.Height - 1) do
        printf "%3d " (map.MinY + y)
        for x in 0 .. map.Width - 1 do
            let symbol =
                match map.Map[x,y] with
                | Empty -> "."
                | SensorCoverage -> "#"
                | Beacon -> "B"
                | Sensor _ -> "S"
            printf "%s" symbol
        printfn ""
    map

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

let createMap sensors =
    let minX = sensors |> Array.map (fun s -> s.Position.X - s.Strength) |> Array.min
    let minY = sensors |> Array.map (fun s -> s.Position.Y - s.Strength) |> Array.min
    let maxX = sensors |> Array.map (fun s -> s.Position.X + s.Strength) |> Array.max
    let maxY = sensors |> Array.map (fun s -> s.Position.Y + s.Strength) |> Array.max

    let width = abs (maxX - minX) + 1
    let height = abs (maxY - minY) + 1
    let map = Array2D.init width height (fun _ _ -> Empty)

    printfn "Min: x=%d y=%d" minX minY
    printfn "Size: %d x %d" width height

    sensors
    |> Array.iter (fun s ->
        let (x,y) = s.Position.X - minX, s.Position.Y - minY
        map.[x,y] <- Sensor s

        let (x,y) = s.ClosestBeacon.X - minX, s.ClosestBeacon.Y - minY
        map.[x,y] <- Beacon

        for dx in -s.Strength .. s.Strength do
            for dy in -s.Strength .. s.Strength do
                let (x,y) = s.Position.X + dx, s.Position.Y + dy
                let len = abs (x - s.Position.X) + abs (y - s.Position.Y)
                if len <= s.Strength then
                    let (x,y) = x - minX, y - minY
                    match map.[x,y] with
                    | Empty -> map.[x,y] <- SensorCoverage
                    | _ -> ()
    )

    {
        Map = map
        MinX = minX
        MinY = minY
    }

let coverage line map =
    let y = line - map.MinY
    map.Map.[*,y]
    |> Array.filter (fun t -> t = SensorCoverage)
    |> Array.length

let part1 filename line =
    filename
    |> parseSensorFile
    |> createMap
    |> printMap
    |> coverage line

part1 "test.txt" 10 |> printfn "Part 1: %d"
//part1 "day15.txt" 2000000 |> printfn "Part 1: %d"