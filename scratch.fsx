/// Cost of BoQItem = (Quantity of Item * Material Unit Cost) + (Quantity of Item * Labor Unit Cost)
let calcItemCost qty materialUnitCost laborUnitCost =
  qty * materialUnitCost + qty * laborUnitCost