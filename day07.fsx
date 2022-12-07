open System.Collections.Generic
open System.Text.RegularExpressions

type FileSystemItem =
    | FileInfo of FileInfo
    | DirectoryInfo of DirectoryInfo
and FileInfo = {
    Name: string
    Size: uint64
}
and DirectoryInfo = {
    Name: string
    Children: FileSystemItem list
}

type Directory = {
    FullName: string
    TotalSize: uint64
}

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

let (|ChangeDir|_|) str =
    match str with
    | Regex @"^\$ cd (.*)$" [dir] -> Some dir
    | _ -> None

let (|ListDir|_|) str =
    match str with
    | Regex @"^\$ ls$" [] -> Some ()
    | _ -> None

let (|DirOutput|_|) str =
    match str with
    | Regex @"^dir (.*)$" [dir] -> Some { Name = dir; Children = []}
    | _ -> None

let (|FileOutput|_|) str =
    match str with
    | Regex @"^([0-9]+) (.*)$" [size; file] -> Some { Name = file; Size = size |> uint64 }
    | _ -> None

let parse lines =
    let folder (cwd : string list, dict : Dictionary<string, DirectoryInfo>) line =
        match line with
        | ChangeDir dir ->
            match dir with
            | ".." -> match cwd with
                      | [] | [_] -> (cwd, dict)
                      | _ -> (List.tail cwd, dict)
            | _ -> (dir :: cwd, dict)
        | ListDir _ -> (cwd, dict)
        | DirOutput dir ->
            let fullName = cwd |> List.rev |> String.concat "/"
            let curr = dict.GetValueOrDefault(fullName, { Name = List.head cwd; Children = []} )
            dict[fullName] <- { curr with Children = DirectoryInfo dir :: curr.Children }
            (cwd, dict)
        | FileOutput file ->
            let fullName = cwd |> List.rev |> String.concat "/"
            let curr = dict.GetValueOrDefault(fullName, { Name = List.head cwd; Children = []} )
            dict[fullName] <- { curr with Children = FileInfo file :: curr.Children }
            (cwd, dict)
        | _ -> failwithf "Invalid line: %s" line

    let rec build (dir : string) (dict : Dictionary<string, DirectoryInfo>) : DirectoryInfo =
        let curr = dict[dir]

        let children =
            curr.Children
            |> List.map (fun item ->
                match item with
                | DirectoryInfo d ->
                    let fullName = dir + "/" + d.Name
                    DirectoryInfo (build fullName dict)
                | FileInfo f -> FileInfo f)

        { curr with Children = children }

    let dict = Dictionary<string, DirectoryInfo>()

    lines
    |> Seq.fold folder ([], dict)
    |> snd
    |> build "/"

let rec getSize item =
    match item with
    | FileInfo file -> file.Size
    | DirectoryInfo dir -> dir.Children |> List.sumBy getSize

let rec getDirectories item =
    let asDirectoryInfo item =
        match item with
        | DirectoryInfo dir -> Some dir
        | _ -> None

    let prependName (parent : DirectoryInfo) (child : Directory) : Directory =
        { child with FullName = parent.Name + "/" + child.FullName }

    let children =
        item.Children
        |> List.choose asDirectoryInfo
        |> List.map getDirectories
        |> List.concat
        |> List.map (prependName item)

    { FullName = item.Name; TotalSize = getSize (DirectoryInfo item) } :: children


let part1 filename =
    filename
    |> System.IO.File.ReadAllLines
    |> parse
    |> getDirectories
    |> List.filter (fun dir -> dir.TotalSize <= 100000UL)
    |> List.sumBy (fun dir -> dir.TotalSize)

let part2 filename =
    let dirs =
        filename
        |> System.IO.File.ReadAllLines
        |> parse
        |> getDirectories

    let totalSpace = 70000000UL
    let totalNeeded = 30000000UL
    let totalUsed = (dirs |> List.head).TotalSize
    let toBeDeleted = totalNeeded - (totalSpace - totalUsed)

    dirs
    |> List.filter (fun dir -> dir.TotalSize >= toBeDeleted)
    |> List.sortBy (fun dir -> dir.TotalSize)
    |> List.head