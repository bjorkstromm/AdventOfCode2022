type MonkeyTest =
    {
        DivisibleBy : int
        TrueTarget : int
        FalseTarget : int
    }

type Monkey =
    {
        Id : int
        Items : int list
        Operation : int -> int
        Test : MonkeyTest
        InspectedItems : int
    }

let addItem item monkey =
    { monkey with Items = monkey.Items @ [item] }

let updateMonkey id item (monkeys : Map<int, Monkey>) =
    monkeys |> Map.change id (fun monkey ->
        monkey
        |> Option.get
        |> addItem item
        |> Some)

let inspectionDone id (monkeys : Map<int, Monkey>) =
    monkeys |> Map.change id (fun monkey ->
        monkey
        |> Option.get
        |> (fun monkey ->
            { monkey with
                Items = []
                InspectedItems = monkey.InspectedItems + monkey.Items.Length })
        |> Some)

let inspectItems monkeys id =
    let monkey = monkeys |> Map.find id

    let inspectItem monkeys item =
        printfn "  inspecting %d" item
        let worryLevel = (item |> monkey.Operation) / 3
        printfn "    worry level is %d" worryLevel
        let target =
            if worryLevel % monkey.Test.DivisibleBy = 0 then
                monkey.Test.TrueTarget
            else
                monkey.Test.FalseTarget
        printfn "    throwing to %d" target
        monkeys |> updateMonkey target worryLevel

    printfn "Monkey %d:" monkey.Id
    monkey.Items
    |> List.fold inspectItem monkeys
    |> inspectionDone monkey.Id

let round (monkeys : Map<int, Monkey>) n =
    let monkeys =
        monkeys.Keys
        |> Seq.toList
        |> List.fold inspectItems monkeys

    printfn "After round %d:" n
    for monkey in monkeys.Values do
        printfn "Monkey %d (%d): %A" monkey.Id monkey.InspectedItems monkey.Items

    monkeys

// Test data
// let monkeys =
//     [
//         {
//             Id = 0
//             Items = [79;98]
//             Operation = fun x -> x * 19
//             Test = { DivisibleBy = 23; TrueTarget = 2; FalseTarget = 3 }
//             InspectedItems = 0
//         }
//         {
//             Id = 1
//             Items = [54;65;75;74]
//             Operation = fun x -> x + 6
//             Test = { DivisibleBy = 19; TrueTarget = 2; FalseTarget = 0 }
//             InspectedItems = 0
//         }
//         {
//             Id = 2
//             Items = [79;60;97]
//             Operation = fun x -> x * x
//             Test = { DivisibleBy = 13; TrueTarget = 1; FalseTarget = 3 }
//             InspectedItems = 0
//         }
//         {
//             Id = 3
//             Items = [74]
//             Operation = fun x -> x + 3
//             Test = { DivisibleBy = 17; TrueTarget = 0; FalseTarget = 1 }
//             InspectedItems = 0
//         }
//     ]
//     |> List.map (fun m -> (m.Id, m))
//     |> Map.ofList

// Data
let monkeys =
    [
        {
            Id = 0
            Items = [64]
            Operation = fun x -> x * 7
            Test = { DivisibleBy = 13; TrueTarget = 1; FalseTarget = 3 }
            InspectedItems = 0
        }
        {
            Id =  1
            Items = [60; 84; 84; 65]
            Operation = fun x -> x + 7
            Test = { DivisibleBy = 19; TrueTarget = 2; FalseTarget = 7}
            InspectedItems = 0
        }
        {
            Id =  2
            Items = [52; 67; 74; 88; 51; 61]
            Operation = fun x -> x * 3
            Test = { DivisibleBy = 5; TrueTarget = 5; FalseTarget = 7}
            InspectedItems = 0
        }
        {
            Id =  3
            Items = [67; 72]
            Operation = fun x -> x + 3
            Test = { DivisibleBy = 2; TrueTarget = 1; FalseTarget = 2}
            InspectedItems = 0
        }
        {
            Id =  4
            Items = [80; 79; 58; 77; 68; 74; 98; 64]
            Operation = fun x -> x * x
            Test = { DivisibleBy = 17; TrueTarget = 6; FalseTarget = 0}
            InspectedItems = 0
        }
        {
            Id =  5
            Items = [62; 53; 61; 89; 86]
            Operation = fun x -> x + 8
            Test = { DivisibleBy = 11; TrueTarget = 4; FalseTarget = 6}
            InspectedItems = 0
        }
        {
            Id =  6
            Items = [86; 89; 82]
            Operation = fun x -> x + 2
            Test = { DivisibleBy = 7; TrueTarget = 3; FalseTarget = 0}
            InspectedItems = 0
        }
        {
            Id =  7
            Items = [92; 81; 70; 96; 69; 84; 83]
            Operation = fun x -> x + 4
            Test = { DivisibleBy = 3; TrueTarget = 4; FalseTarget = 5}
            InspectedItems = 0
        }
    ]
    |> List.map (fun m -> (m.Id, m))
    |> Map.ofList

let rounds = 20

[1..rounds]
|> List.fold round monkeys
|> Map.toSeq
|> Seq.map (fun (id, monkey) -> monkey.InspectedItems)
|> Seq.sortDescending
|> Seq.take 2
|> Seq.reduce (*)