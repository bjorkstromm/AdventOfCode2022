type MonkeyTest =
    {
        DivisibleBy : uint64
        TrueTarget : int
        FalseTarget : int
    }

type Monkey =
    {
        Id : int
        Items : uint64 list
        Operation : uint64 -> uint64
        Test : MonkeyTest
        InspectedItems : uint64
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
                InspectedItems = monkey.InspectedItems + (uint64)monkey.Items.Length })
        |> Some)

let inspectItems (monkeys, worryLevelDivisor) id =
    let monkey = monkeys |> Map.find id

    let inspectItem monkeys item =
        printfn "  inspecting %d" item
        let worryLevel = (item |> monkey.Operation) / worryLevelDivisor
        printfn "    worry level is %d" worryLevel
        let target =
            if worryLevel % monkey.Test.DivisibleBy = 0UL then
                printfn "      divisable by %d" monkey.Test.DivisibleBy
                monkey.Test.TrueTarget
            else
                printfn "      not divisable by %d" monkey.Test.DivisibleBy
                monkey.Test.FalseTarget
        printfn "    throwing to %d" target
        monkeys |> updateMonkey target worryLevel

    printfn "Monkey %d:" monkey.Id
    let monkeys =
        monkey.Items
        |> List.fold inspectItem monkeys
        |> inspectionDone monkey.Id
    (monkeys, worryLevelDivisor)

let round (monkeys : Map<int, Monkey>, worryLevelDivisor) n =
    printfn "\n== Starting round %d ==" n

    let (monkeys, _) =
        monkeys.Keys
        |> Seq.toList
        |> List.fold inspectItems (monkeys, worryLevelDivisor)

    printfn "\n== After round %d ==" n
    for monkey in monkeys.Values do
        printfn "Monkey %d inspected items %d times (%A)" monkey.Id monkey.InspectedItems monkey.Items

    (monkeys, worryLevelDivisor)

// Test data
let monkeys =
    [
        {
            Id = 0
            Items = [79UL;98UL]
            Operation = fun x -> x * 19UL
            Test = { DivisibleBy = 23UL; TrueTarget = 2; FalseTarget = 3 }
            InspectedItems = 0UL
        }
        {
            Id = 1
            Items = [54UL;65UL;75UL;74UL]
            Operation = fun x -> x + 6UL
            Test = { DivisibleBy = 19UL; TrueTarget = 2; FalseTarget = 0 }
            InspectedItems = 0UL
        }
        {
            Id = 2
            Items = [79UL;60UL;97UL]
            Operation = fun x -> x * x
            Test = { DivisibleBy = 13UL; TrueTarget = 1; FalseTarget = 3 }
            InspectedItems = 0UL
        }
        {
            Id = 3
            Items = [74UL]
            Operation = fun x -> x + 3UL
            Test = { DivisibleBy = 17UL; TrueTarget = 0; FalseTarget = 1 }
            InspectedItems = 0UL
        }
    ]
    |> List.map (fun m -> (m.Id, m))
    |> Map.ofList

// Data
// let monkeys =
//     [
//         {
//             Id = 0
//             Items = [64UL]
//             Operation = fun x -> x * 7UL
//             Test = { DivisibleBy = 13UL; TrueTarget = 1; FalseTarget = 3 }
//             InspectedItems = 0UL
//         }
//         {
//             Id =  1
//             Items = [60UL; 84UL; 84UL; 65UL]
//             Operation = fun x -> x + 7UL
//             Test = { DivisibleBy = 19UL; TrueTarget = 2; FalseTarget = 7}
//             InspectedItems = 0UL
//         }
//         {
//             Id =  2
//             Items = [52UL; 67UL; 74UL; 88UL; 51UL; 61UL]
//             Operation = fun x -> x * 3UL
//             Test = { DivisibleBy = 5UL; TrueTarget = 5; FalseTarget = 7}
//             InspectedItems = 0UL
//         }
//         {
//             Id =  3
//             Items = [67UL; 72UL]
//             Operation = fun x -> x + 3UL
//             Test = { DivisibleBy = 2UL; TrueTarget = 1; FalseTarget = 2}
//             InspectedItems = 0UL
//         }
//         {
//             Id =  4
//             Items = [80UL; 79UL; 58UL; 77UL; 68UL; 74UL; 98UL; 64UL]
//             Operation = fun x -> x * x
//             Test = { DivisibleBy = 17UL; TrueTarget = 6; FalseTarget = 0}
//             InspectedItems = 0UL
//         }
//         {
//             Id =  5
//             Items = [62UL; 53UL; 61UL; 89UL; 86UL]
//             Operation = fun x -> x + 8UL
//             Test = { DivisibleBy = 11UL; TrueTarget = 4; FalseTarget = 6}
//             InspectedItems = 0UL
//         }
//         {
//             Id =  6
//             Items = [86UL; 89UL; 82UL]
//             Operation = fun x -> x + 2UL
//             Test = { DivisibleBy = 7UL; TrueTarget = 3; FalseTarget = 0}
//             InspectedItems = 0UL
//         }
//         {
//             Id =  7
//             Items = [92UL; 81UL; 70UL; 96UL; 69UL; 84UL; 83UL]
//             Operation = fun x -> x + 4UL
//             Test = { DivisibleBy = 3UL; TrueTarget = 4; FalseTarget = 5}
//             InspectedItems = 0UL
//         }
//     ]
//     |> List.map (fun m -> (m.Id, m))
//     |> Map.ofList

// Part 1
//let rounds = 20
//let worryLevelDivisor = 3UL

// Part 2
let rounds = 2
let worryLevelDivisor = 1UL

[1..rounds]
|> List.fold round (monkeys, worryLevelDivisor)
|> fst
|> Map.toSeq
|> Seq.map (fun (_, monkey) -> monkey.InspectedItems)
|> Seq.sortDescending
|> Seq.take 2
|> Seq.reduce (*)