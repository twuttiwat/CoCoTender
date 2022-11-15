namespace CoCoTender.Domain

module Say =
    let hello name =
        $"Hello, {name}"


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
  let calcCost qty unitCost =
    match qty, unitCost with
    | Quantity (qty, qtyUnit), {UnitCost = unitCost; Unit = unitCostUnit} 
      when qtyUnit = unitCostUnit ->
        (qty * unitCost) |> Some
    | _ -> None

  let tryCalcItemCost qty (Material materialUnitCost) (Labor laborUnitCost) =  
    match calcCost qty materialUnitCost, calcCost qty laborUnitCost with
    | Some materialCost, Some laborCost -> Some (materialCost + laborCost)
    | _ -> None

  let isValid desc qty materialUnitCost laborUnitCost =
    match qty, materialUnitCost, laborUnitCost with
    | Quantity (_, qtyUnit), Material {Unit = materialUnit}, Labor {Unit = laborUnit} ->
      qtyUnit = materialUnit && qtyUnit = laborUnit  

  let tryCreate desc qty materialUnitCost laborUnitCost = 
    if isValid desc qty materialUnitCost laborUnitCost  then 
      tryCalcItemCost qty materialUnitCost laborUnitCost
      |> Option.map(fun cost -> {
          Description = desc
          Quantity = qty
          MaterialUnitCost = materialUnitCost
          LaborUnitCost = laborUnitCost
          TotalCost = cost
        })
    else
      None

  let value item = 
    {| 
      Description = item.Description
      Quantity = item.Quantity
      MaterialUnitCost = item.MaterialUnitCost
      LaborUnitCost = item.LaborUnitCost
      TotalCost = item.TotalCost
    |}

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
