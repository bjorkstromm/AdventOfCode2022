let findFirstUniqueWindow size str =
    let isUnique str =
        str
        |> Array.distinct
        |> Array.length
        |> (=) str.Length 

    let isNotUnique str =
        not (isUnique str)

    str
    |> Seq.windowed size
    |> Seq.takeWhile isNotUnique
    |> Seq.length
    |> (+) size

let part1 filename =
    filename
    |> System.IO.File.ReadAllText
    |> findFirstUniqueWindow 4

"bvwbjplbgvbhsrlpgdmjqwftvncz" |> findFirstUniqueWindow 4 //first marker after character 5
"nppdvjthqldpwncqszvftbrmjlhg" |> findFirstUniqueWindow 4 //first marker after character 6
"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" |> findFirstUniqueWindow 4 //first marker after character 10
"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" |> findFirstUniqueWindow 4 //first marker after character 11

// part 2

let part2 filename =
    filename
    |> System.IO.File.ReadAllText
    |> findFirstUniqueWindow 14

"bvwbjplbgvbhsrlpgdmjqwftvncz" |> findFirstUniqueWindow 14 //first marker after character 5
"nppdvjthqldpwncqszvftbrmjlhg" |> findFirstUniqueWindow 14 //first marker after character 6
"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" |> findFirstUniqueWindow 14 //first marker after character 10
"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" |> findFirstUniqueWindow 14 //first marker after character 11