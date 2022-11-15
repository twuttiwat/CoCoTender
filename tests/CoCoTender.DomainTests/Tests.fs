module Tests

open System
open Xunit
open FsUnit
open CoCoTender
open CoCoTender.Domain

[<Fact>]
let ``Hello test`` () =
    Say.hello "world" |> should equal "Hello, world"

module ``Create boq item`` =

    let desc = "Pool Item"
    let qty = Quantity (10.0, "m^2")
    let material = Material { Name = "Big Tile"; UnitCost = 100.0; Unit = "m^2" }
    let labor = Labor { Name = "Do Tiling"; UnitCost = 50.0; Unit = "m^2" }
    let totalCost = 10*100 + 10*50

    [<Fact>]
    let ``should succeed if Quantity, Material and Labor use the same Unit`` () = 
        let result = BoQItem.tryCreate desc qty material labor
        match result with
        | Some item -> 
            let itemVal = item |> BoQItem.value
            itemVal.Description |> should equal desc
            itemVal.Quantity |> should equal qty
            itemVal.MaterialUnitCost |> should equal material
            itemVal.LaborUnitCost |> should equal labor
        | None -> Assert.Fail "Could not create BoQItem"

    [<Fact>]
    let ``should fail if Quantity, Material and Labor have different Units`` () = 
        let material' = material |> fun (Material m) -> {m with Unit = "m"} |> Material
        let labor' = labor |> fun (Labor l) -> {l with Unit = "cm"} |> Labor

        let result = BoQItem.tryCreate desc qty material' labor'
        match result with
        | None -> Assert.True(true)
        | Some _ -> Assert.Fail "Should not create BoQItem with different Unit"        

    [<Fact>]
    let ``should always calculate total cost when create`` () = 
        let result = BoQItem.tryCreate desc qty material labor
        match result with
        | Some item -> 
            let itemVal = item |> BoQItem.value
            itemVal.TotalCost |> should equal totalCost
        | None -> Assert.Fail "Could not create BoQItem"

module ``Update boq item`` =

    let desc = "Pool Item"
    let qty = Quantity (10.0, "m^2")
    let material = Material { Name = "Big Tile"; UnitCost = 100.0; Unit = "m^2" }
    let labor = Labor { Name = "Do Tiling"; UnitCost = 50.0; Unit = "m^2" }
 
    let item = 
        match BoQItem.tryCreate desc qty material labor with
        | Some item' -> item'
        | _ -> failwith "Could not create item for updating"

    let totalCost = item |> BoQItem.value |> fun x -> x.TotalCost

    [<Fact>]
    let ``with quantity should always re-calculate total cost`` () =
        let result = item |> BoQItem.tryUpdateQty (Quantity (20, "m^2"))
        match result with
        | Some item' -> 
            let itemVal' = item' |> BoQItem.value
            itemVal'.TotalCost |> should equal (totalCost * 2.0)
        | _ -> Assert.Fail "Could not update quantity" 

    [<Fact>]
    let ``with description should not update total cost`` () =
        let result = item |> BoQItem.updateDesc "New Pool" 
        let resultVal = result |> BoQItem.value
        resultVal.TotalCost |> should equal totalCost
