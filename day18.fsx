let parseCube (line : string) =
    match line.Split(',') with
    | [|x;y;z|] -> (x |> int,y |> int,z |> int)
    | _ -> failwithf "Invalid input %s" line

let parseCubes lines =
    lines
    |> Seq.map parseCube
    |> Set.ofSeq

let getfaces (x,y,z) =
    seq {
        (x+1,y,z)
        (x-1,y,z)
        (x,y+1,z)
        (x,y-1,z)
        (x,y,z+1)
        (x,y,z-1)
    }

let notConnected cubes cube =
    cube
    |> getfaces
    |> Seq.filter (fun side -> cubes |> Set.contains side |> not)
    |> Seq.length

let surfaceArea cubes =
    cubes
    |> Seq.sumBy (notConnected cubes)

let part1 filename =
    filename
    |> System.IO.File.ReadAllLines
    |> parseCubes
    |> surfaceArea

"test.txt" |> part1
"day18.txt" |> part1