module Server.Tests

open Expecto

open System
open CoCoTender.Domain
open Shared
open Server

let itemId = Guid.NewGuid()
let desc = "Pool Item"
let qty = Quantity (10.0, "m^2")
let material = Material { Name = "Big Tile"; UnitCost = 100.0; Unit = "m^2" }
let labor = Labor { Name = "Do Tiling"; UnitCost = 50.0; Unit = "m^2" }
let validBoQItem = BoQItem.tryCreate itemId desc qty material labor

let server = testList "Server" [
    testCase "Adding valid BoQItem" <| fun _ ->
        let expectedResult = Ok ()

        let result = Storage.addBoQItem validBoQItem.OkValue

        Expect.equal result expectedResult "Result should be ok"
        Expect.contains Storage.boqItems validBoQItem.OkValue "Storage should contain new boq item"

    // testCase "Updating valid BoQItem" <| fun _ ->
    //     let validUpdateBoQItem = BoQItem.tryCreate itemId $"New {desc}" qty material labor

    //     let expectedResult = Ok ()

    //     let result = Storage.updateBoQItem validUpdateBoQItem.OkValue
    //     // printfn "BoqItem %A" Storage.boqItems

    //     Expect.equal result expectedResult "Result should be ok"
    //     Expect.contains Storage.boqItems validUpdateBoQItem.OkValue "Storage should contain update boq item"

    // testCase "Deleting BoQItem" <| fun _ ->
    //     let expectedResult = Ok ()

    //     let originalCount = Storage.boqItems.Count
    //     let result = Storage.deleteBoQItem itemId

    //     Expect.equal result expectedResult "Result should be ok"
    //     Expect.equal Storage.boqItems.Count (originalCount - 1) "Number of item should reduce by one"

]

let all =
    testList "All"
        [
            Shared.Tests.shared
            server
        ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all