module Server.Tests

open Expecto

open System
open CoCoTender.Domain
open Shared
open Server

let server = testList "Server" [
    testCase "Adding valid BoQItem" <| fun _ ->
        let desc = "Pool Item"
        let qty = Quantity (10.0, "m^2")
        let material = Material { Name = "Big Tile"; UnitCost = 100.0; Unit = "m^2" }
        let labor = Labor { Name = "Do Tiling"; UnitCost = 50.0; Unit = "m^2" }

        let validBoQItem = BoQItem.tryCreate (Guid.NewGuid()) desc qty material labor


        let expectedResult = Ok ()

        let result = Storage.addBoQItem validBoQItem.OkValue

        Expect.equal result expectedResult "Result should be ok"
        Expect.contains Storage.boqItems validBoQItem.OkValue "Storage should contain new boq item"
]

let all =
    testList "All"
        [
            Shared.Tests.shared
            server
        ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all