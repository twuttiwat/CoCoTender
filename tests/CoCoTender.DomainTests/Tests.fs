module Tests

open System
open Xunit
open FsUnit
open CoCoTender
open CoCoTender.Domain

let newId = Guid.NewGuid()
let desc = "Pool Item"
let qty = Quantity (10.0, "m^2")
let material = Material { Name = "Big Tile"; UnitCost = 100.0; Unit = "m^2" }
let labor = Labor { Name = "Do Tiling"; UnitCost = 50.0; Unit = "m^2" }
let totalCost = 10*100 + 10*50

let item =
    match BoQItem.tryCreate newId desc qty material labor with
    | Ok item' -> item'
    | _ -> failwith "Could not create boq item"

module ``Create boq item`` =

    [<Fact>]
    let ``should succeed if Quantity, Material and Labor use the same Unit`` () =
        let result = BoQItem.tryCreate newId desc qty material labor
        match result with
        | Ok item ->
            let itemVal = item |> BoQItem.value
            itemVal.Description |> should equal desc
            itemVal.Quantity |> should equal qty
            itemVal.MaterialUnitCost |> should equal material
            itemVal.LaborUnitCost |> should equal labor
        | Error msg -> Assert.Fail msg

    [<Fact>]
    let ``should fail if Quantity, Material and Labor have different Units`` () =
        let material' = material |> fun (Material m) -> {m with Unit = "m"} |> Material
        let labor' = labor |> fun (Labor l) -> {l with Unit = "cm"} |> Labor

        let result = BoQItem.tryCreate newId desc qty material' labor'
        match result with
        | Error _ -> Assert.True(true)
        | Ok item -> Assert.Fail "Should not create BoQItem with different Unit"

    [<Fact>]
    let ``should always calculate total cost when create`` () =
        let result = BoQItem.tryCreate newId desc qty material labor
        match result with
        | Ok item ->
            let itemVal = item |> BoQItem.value
            itemVal.TotalCost |> should equal totalCost
        | Error msg -> Assert.Fail msg

module ``Update boq item`` =

    let totalCost = item |> BoQItem.value |> fun x -> x.TotalCost

    [<Fact>]
    let ``with quantity should always re-calculate total cost`` () =
        let result = item |> BoQItem.tryUpdateQty (Quantity (20, "m^2"))
        match result with
        | Ok item' ->
            let itemVal' = item' |> BoQItem.value
            itemVal'.TotalCost |> should equal (totalCost * 2.0)
        | Error msg -> Assert.Fail msg

    [<Fact>]
    let ``with description should not update total cost`` () =
        let result = item |> BoQItem.updateDesc "New Pool"
        let resultVal = result |> BoQItem.value
        resultVal.TotalCost |> should equal totalCost


module ``Calculate factor f`` =

    open Project

    let fTable = FactorFTable [(10,1.1); (100,1.5); (1000, 1.9)]

    [<Fact>]
    let ``should pick minimum factor f when cost exceed minimum cost`` () =
        let result = calcFactorF fTable (DirectCost 9.0)
        result |> should equal 1.1

    [<Fact>]
    let ``should pick maximum factor f when cost exceed maximum cost`` () =
        let result = calcFactorF fTable (DirectCost 1001.0)
        result |> should equal 1.9

    [<Fact>]
    let ``should calc factor f by range when cost is between any ranges`` () =
        let result = calcFactorF fTable (DirectCost 55.0)
        result |> should equal 1.3

module ``Show factor f info`` =

    open Project

    let loadFactorFTableFn =
        function () -> FactorFTable [(10,1.1); (100,1.5); (1000, 1.9)]

    [<Fact>]
    let ``should return correct info`` () =
        let fInfo = getFactorFInfo loadFactorFTableFn
        let expectedResult = [
            "<= 10", 1.1
            "<= 100", 1.5
            "<= 1000", 1.9
            "> 1000", 1.9
        ]
        fInfo |> should equal expectedResult

module ``Estimate construction cost`` =

    open Project

    let loadFactorFTableFn =
        function () -> FactorFTable [(10,1.1); (100,1.5); (1000, 1.9)]

    [<Fact>]
    let ``should return correct result`` () =
        let result = tryEstimateCost loadFactorFTableFn [item]
        match result with
        | Ok estimatedCost -> estimatedCost |> should equal 2000
        | Error msg -> Assert.Fail msg

    [<Fact>]
    let ``should not round when less than one thousand`` () =
        let smallPool =
            match item |> BoQItem.tryUpdateQty (Quantity (1.0, "m^2")) with
            | Ok item' -> item'
            | _ -> failwith "Could not update quantity for small pool"
        let result = tryEstimateCost loadFactorFTableFn [smallPool]
        match result with
        | Ok estimatedCost -> estimatedCost |> should (equalWithin 0.11) 228.33
        | Error msg -> Assert.Fail msg
