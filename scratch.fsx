type Quantity = Quantity of float
type UnitCost = UnitCost of float

/// Cost of BoQItem = (Quantity of Item * Material Unit Cost) + (Quantity of Item * Labor Unit Cost)
let calcItemCost (Quantity qty) (UnitCost materialUnitCost) (UnitCost laborUnitCost) =  
  qty * materialUnitCost + qty * laborUnitCost

let testCalcItemCost = 
  (calcItemCost (Quantity 10) (UnitCost 100) (UnitCost 50)) = 1500