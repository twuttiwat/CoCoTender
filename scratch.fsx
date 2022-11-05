type Quantity = Quantity of float*string
type UnitCost = 
  {
    Name : string
    UnitCost : float
    Unit : string
  }
type MaterialUnitCost = Material of UnitCost
type LaborUnitCost = Labor of UnitCost

type BoQItem = private {
    Description : string
    Quantity : Quantity
    MaterialUnitCost : MaterialUnitCost
    LaborUnitCost : LaborUnitCost
    TotalCost : float
  }
module BoQItem =
  let tryCalcItemCost (Quantity (qty,qtyUnit)) (Material materialUnitCost) (Labor laborUnitCost) =  
    if qtyUnit = materialUnitCost.Unit && qtyUnit = laborUnitCost.Unit then 
      (qty * materialUnitCost.UnitCost + qty * laborUnitCost.UnitCost) |> Some
    else
      None

  let tryCreate desc qty materialUnitCost laborUnitCost =
    tryCalcItemCost qty materialUnitCost laborUnitCost
    |> Option.map (fun cost -> 
      {
        Description = desc
        Quantity = qty
        MaterialUnitCost = materialUnitCost
        LaborUnitCost = laborUnitCost
        TotalCost = cost
      })

  let updateDesc newDesc item = { item with Description = newDesc }

  let tryUpdateQty newQty item = 
    tryCalcItemCost newQty item.MaterialUnitCost item.LaborUnitCost
    |> Option.map (fun cost -> 
      {
        item with
          Quantity = newQty
          TotalCost = cost
      })

  let tryUpdateMaterialUnitCost (Material newUnitCost) item =
    let newMaterialUnitCost = Material newUnitCost
    tryCalcItemCost item.Quantity newMaterialUnitCost item.LaborUnitCost
    |> Option.map (fun cost -> 
      {
        item with
          MaterialUnitCost = newMaterialUnitCost
          TotalCost = cost
      })

  let tryUpdateLaborUnitCost (Labor newUnitCost) item =
    let newLaborUnitCost = Labor newUnitCost
    tryCalcItemCost item.Quantity item.MaterialUnitCost newLaborUnitCost 
    |> Option.map (fun cost -> 
      {
        item with
          LaborUnitCost = newLaborUnitCost
          TotalCost = cost
      })

///
/// Test
/// 

open BoQItem

let testCalcItemCost = 
  let materialUnitCost = Material { Name = "Big Tile"; UnitCost = 100; Unit = "m^2" }
  let laborUnitCost = Labor { Name = "Do Tiling"; UnitCost = 50; Unit = "m^2" }
  tryCalcItemCost (Quantity (10, "m^2")) materialUnitCost laborUnitCost = Some 1500 

let testCalcItemCostError = 
  let materialUnitCost = Material { Name = "Big Tile"; UnitCost = 100; Unit = "m" }
  let laborUnitCost = Labor { Name = "Do Tiling"; UnitCost = 50; Unit = "cm^2" }
  tryCalcItemCost (Quantity (10, "m^2")) materialUnitCost laborUnitCost = None 

let poolItem =
  let qty = Quantity (10, "m^2")
  let materialUnitCost = Material { Name = "Big Tile"; UnitCost = 100; Unit = "m^2" }
  let laborUnitCost = Labor { Name = "Do Tiling"; UnitCost = 50; Unit = "m^2" }
  tryCreate "Pool Tile"  qty materialUnitCost laborUnitCost
 
let testCreateBoQItem =
  let qty = Quantity (10, "m^2")
  let materialUnitCost = Material { Name = "Big Tile"; UnitCost = 100; Unit = "m^2" }
  let laborUnitCost = Labor { Name = "Do Tiling"; UnitCost = 50; Unit = "m^2" }
  let boqItem = tryCreate "Pool Tile"  qty materialUnitCost laborUnitCost
  boqItem
  |> Option.map (fun item -> item.TotalCost = 1500)
  |> Option.defaultValue false

let testUpdateDesc = 
  poolItem 
  |> Option.map (updateDesc "New Pool") 
  |> Option.map (fun x -> x.Description = "New Pool") 
  |> Option.defaultValue false

let testUpdateQty = 
  let newQty = Quantity (20,"m^2")
  poolItem 
  |> Option.bind (tryUpdateQty  newQty) 
  |> Option.map (fun x -> x.TotalCost = 3000) 
  |> Option.defaultValue false

let testUpdateMaterialUnitCost = 
  let newMaterialUnitCost = 
    Material <| { Name = "Small Tile"; UnitCost = 50; Unit = "m^2"}
  poolItem 
  |> Option.bind (tryUpdateMaterialUnitCost  newMaterialUnitCost) 
  |> Option.map (fun x -> x.TotalCost = 1000) 
  |> Option.defaultValue false

let testUpdateLaborUnitCost = 
  let newLaborUnitCost = 
    Labor <| { Name = "Do Tiling #2"; UnitCost = 100; Unit = "m^2"}
  poolItem 
  |> Option.bind (tryUpdateLaborUnitCost  newLaborUnitCost) 
  |> Option.map (fun x -> x.TotalCost = 2000) 
  |> Option.defaultValue false
