type Quantity = Quantity of float*string
type UnitCost = UnitCost of float*string

/// Calcuate cost of BoQItem
let tryCalcItemCost (Quantity (qty,qtyUnit)) (UnitCost (materialUnitCost, materialUnit)) (UnitCost (laborUnitCost, laborUnit)) =  
  if qtyUnit = materialUnit && qtyUnit = laborUnit then 
    (qty * materialUnitCost + qty * laborUnitCost) |> Some
  else
    None

///
/// Test
/// 
let testCalcItemCost = 
  tryCalcItemCost (Quantity (10, "m^2")) (UnitCost (100, "m^2")) (UnitCost (50, "m^2")) = Some 1500

let testCalcItemCostError = 
  tryCalcItemCost (Quantity (10, "m^2")) (UnitCost (100, "m")) (UnitCost (50, "cm^2")) = None