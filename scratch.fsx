type MaybeBuilder() =

  member this.Bind(x, f) =
    match x with
    | None -> None
    | Some a -> f a

  member this.Return(x) =
    Some x

let maybe = new MaybeBuilder()

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
  let private calcCost qty (unitCost:UnitCost) =
    match qty, unitCost with
    | Quantity (qty, qtyUnit), {UnitCost = unitCost; Unit = unitCostUnit} 
      when qtyUnit = unitCostUnit ->
        (qty * unitCost) |> Some
    | _ -> None

  let tryCalcItemCost qty (Material materialUnitCost) (Labor laborUnitCost) =  
    match calcCost qty materialUnitCost, calcCost qty laborUnitCost with
    | Some materialCost, Some laborCost -> Some (materialCost + laborCost)
    | _ -> None

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

let poolItem =
  let qty = Quantity (10, "m^2")
  let materialUnitCost = Material { Name = "Big Tile"; UnitCost = 100; Unit = "m^2" }
  let laborUnitCost = Labor { Name = "Do Tiling"; UnitCost = 50; Unit = "m^2" }
  tryCreate "Pool Tile"  qty materialUnitCost laborUnitCost
 
let testCreateBoQItem =
  maybe {
    let! poolItem' = poolItem
    return poolItem'.TotalCost = 1500
  }
  |> Option.defaultValue false

let testUpdateDesc = 
  maybe {
    let! poolItem' = poolItem
    let updatedPoolItem = updateDesc "New Pool" poolItem'
    return updatedPoolItem.Description = "New Pool"
  }
  |> Option.defaultValue false

let testUpdateQty = 
  maybe {
    let! poolItem' = poolItem
    let newQty = Quantity (20,"m^2")
    let! updatedPoolItem = tryUpdateQty newQty poolItem'
    return updatedPoolItem.TotalCost = 3000
  }
  |> Option.defaultValue false

let testUpdateMaterialUnitCost = 
  maybe {
    let! poolItem' = poolItem
    let newMaterialUnitCost = Material <| { Name = "Small Tile"; UnitCost = 50; Unit = "m^2"}
    let! updatedPoolItem = tryUpdateMaterialUnitCost newMaterialUnitCost poolItem'
    return updatedPoolItem.TotalCost = 1000
  }
  |> Option.defaultValue false

let testUpdateLaborUnitCost = 
  maybe { 
    let! poolItem' = poolItem
    let newLaborUnitCost = Labor <| { Name = "Do Tiling #2"; UnitCost = 100; Unit = "m^2"}
    let! updatedPoolItem = tryUpdateLaborUnitCost newLaborUnitCost poolItem'
    return updatedPoolItem.TotalCost = 2000
  }
  |> Option.defaultValue false
