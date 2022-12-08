module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open System
open Shared

let shared = testList "Shared" [
    testCase "Description is required for BoQItemDto" <| fun _ ->
        let expected = false

        let boqItem =
            BoQItemDto.create (Guid.NewGuid()) "" 10.0 "m^2" "Big Tile" 100.0
                            "Do Tiling" 50.0 1500.0
        let actual = BoQItemDto.isValid boqItem
        Expect.equal actual expected "Should be false"

    testCase "TotalCost is recalculated when creating BoQItemDto" <| fun _ ->
        let expected = 1500

        let boqItem =
            BoQItemDto.create (Guid.NewGuid()) "" 10.0 "m^2" "Big Tile" 100.0
                            "Do Tiling" 50.0 0.0
        let actual = boqItem.TotalCost
        Expect.equal actual expected "Should be 1500"

]