module Server.Tests

open Expecto

open Shared
open Server

// let server = testList "Server" [
//     testCase "Adding valid Todo" <| fun _ ->
//         let validTodo = Todo.create "TODO"
//         let expectedResult = Ok ()

//         let result = Storage.addTodo validTodo

//         Expect.equal result expectedResult "Result should be ok"
//         Expect.contains Storage.todos validTodo "Storage should contain new todo"
// ]

let server = testList "Server" [
    testCase "Adding valid BoQItem" <| fun _ ->
        let validBoQItem =
            BoQItemDto.create "Pool Tile" 10.0 "m^2" "Big Tile" 100.0
                            "Do Tiling" 50.0 1500.0

        let expectedResult = Ok ()

        let result = Storage.addBoQItem validBoQItem

        Expect.equal result expectedResult "Result should be ok"
        Expect.contains Storage.boqItems validBoQItem "Storage should contain new boq item"
]

let all =
    testList "All"
        [
            Shared.Tests.shared
            server
        ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all