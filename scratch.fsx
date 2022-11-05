type Quantity = Quantity of float*string
type UnitCost = 
  {
    Name: string
    UnitCost: float
    Unit: string
  }
type MaterialUnitCost = Material of UnitCost
type LaborUnitCost = Labor of UnitCost

/// Calcuate cost of BoQItem
let tryCalcItemCost (Quantity (qty,qtyUnit)) (Material materialUnitCost) (Labor laborUnitCost) =  
  if qtyUnit = materialUnitCost.Unit && qtyUnit = laborUnitCost.Unit then 
    (qty * materialUnitCost.UnitCost + qty * laborUnitCost.UnitCost) |> Some
  else
    None

///
/// Test
/// 
let testCalcItemCost = 
  let materialUnitCost = Material { Name = "Big Tile"; UnitCost = 100; Unit = "m^2" }
  let laborUnitCost = Labor { Name = "Do Tiling"; UnitCost = 50; Unit = "m^2" }
  tryCalcItemCost (Quantity (10, "m^2")) materialUnitCost laborUnitCost = Some 1500 

let testCalcItemCostError = 
  let materialUnitCost = Material { Name = "Big Tile"; UnitCost = 100; Unit = "m" }
  let laborUnitCost = Labor { Name = "Do Tiling"; UnitCost = 50; Unit = "cm^2" }
  tryCalcItemCost (Quantity (10, "m^2")) materialUnitCost laborUnitCost = None 