module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Shared

// let shared = testList "Shared" [
//     testCase "Empty string is not a valid description" <| fun _ ->
//         let expected = false
//         let actual = Todo.isValid ""
//         Expect.equal actual expected "Should be false"
// ]

let shared = testList "Shared" [
    testCase "Description is required for BoQItemDto" <| fun _ ->
        let expected = false

        let boqItem =
            BoQItemDto.create "" 10.0 "m^2" "Big Tile" 100.0
                            "Do Tiling" 50.0 1500.0
        let actual = BoQItemDto.isValid boqItem
        Expect.equal actual expected "Should be false"
]