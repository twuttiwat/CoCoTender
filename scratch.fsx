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
      }
    ) 
    
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

let testCreateBoQItem =
  let qty = Quantity (10, "m^2")
  let materialUnitCost = Material { Name = "Big Tile"; UnitCost = 100; Unit = "m^2" }
  let laborUnitCost = Labor { Name = "Do Tiling"; UnitCost = 50; Unit = "m^2" }
  let boqItem = tryCreate "Pool Tile"  qty materialUnitCost laborUnitCost
  boqItem
  |> Option.map (fun item -> item.TotalCost = 1500)
  |> Option.defaultValue false
  