module Client.Tests

open Fable.Mocha

open Index
open Shared

let client = testList "Client" [
    testCase "Added boq item" <| fun _ ->
        let newBoQItem =
            BoQItemDto.create "Pool Tile" 10.0 "m^2" "Big Tile" 100.0
                        "Do Tiling" 50.0 1500.0
        let model, _ = init ()

        let model, _ = update (AddedBoQItem newBoQItem) model

        Expect.equal model.BoQItems.Length 1 "There should be 1 boq item"
        Expect.equal model.BoQItems.[0] newBoQItem "Todo should equal new boq item"
]

let all =
    testList "All"
        [
#if FABLE_COMPILER // This preprocessor directive makes editor happy
            Shared.Tests.shared
#endif
            client
        ]

[<EntryPoint>]
let main _ = Mocha.runTests all
