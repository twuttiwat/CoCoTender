#r "nuget:FsToolkit.ErrorHandling"

open FsToolkit.ErrorHandling

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

  let calcItemCost qty (Material materialUnitCost) (Labor laborUnitCost) =  
    match calcCost qty materialUnitCost, calcCost qty laborUnitCost with
    | Some materialCost, Some laborCost -> Ok (materialCost + laborCost)
    | _ -> Error "Could not calculate item cost"

  let isValid desc qty materialUnitCost laborUnitCost =
    match qty, materialUnitCost, laborUnitCost with
    | Quantity (_, qtyUnit), Material {Unit = materialUnit}, Labor {Unit = laborUnit} ->
      qtyUnit = materialUnit && qtyUnit = laborUnit  

  let areUnitsMatched  qty materialUnitCost laborUnitCost =
    match qty, materialUnitCost, laborUnitCost with
    | Quantity (_, qtyUnit), Material {Unit = materialUnit}, Labor {Unit = laborUnit} ->
      qtyUnit = materialUnit && qtyUnit = laborUnit

  let create desc qty materialUnitCost laborUnitCost = result {
     
    do! areUnitsMatched qty materialUnitCost laborUnitCost |> Result.requireTrue "Unit are Not matched." 

    let! itemCost =  calcItemCost qty materialUnitCost laborUnitCost

    return {
      Description = desc
      Quantity = qty
      MaterialUnitCost = materialUnitCost
      LaborUnitCost = laborUnitCost
      TotalCost = itemCost
    }
  }


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

  let updateDesc newDesc item = 
    { item with Description = newDesc }

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

  let recalcTotalCost item = result {
    let! totalCost = calcItemCost item.Quantity item.MaterialUnitCost item.LaborUnitCost
    return { item with TotalCost = totalCost }
  }

  let updateQty newQty item =
    { item with Quantity = newQty }
    |> recalcTotalCost

  let updateMaterialUnitCost newMaterialUnitCost item =
    { item with MaterialUnitCost = newMaterialUnitCost }
    |> recalcTotalCost

  let updateLaborUnitCost newLaborUnitCost item =
    { item with LaborUnitCost = newLaborUnitCost }
    |> recalcTotalCost

module Project =

  open BoQItem

  type DirectCost = DirectCost of float
  type FactorFTable = FactorFTable of (float*float) list

  let calcDirectCost items = 
    items 
    |> List.sumBy (fun a -> a |> value |> (fun b -> b.TotalCost))
    |> DirectCost

  let calcBoundF  lowerBound upperBound directCost =
    let lowerBoundCost,lowerBoundF = lowerBound
    let upperBoundCost,upperBoundF = upperBound
    let fRange = upperBoundF - lowerBoundF
    let costRatio = (directCost - lowerBoundCost) / (upperBoundCost - lowerBoundCost)
    costRatio * fRange + lowerBoundF

  let (|LessThanMin|GreaterThanMax|BetweenRange|) input =
    let (FactorFTable fTable),(DirectCost cost) = input
    let minCost, minF = (fTable |> List.head)
    let maxCost, maxF = (fTable |> List.last)
    if cost < minCost then LessThanMin(minF)
    else if cost > maxCost then GreaterThanMax(maxF)
    else
      let lowerBoundIndex = fTable |> List.findIndexBack (fun (cost',_) -> cost > cost')
      let upperBoundIndex = lowerBoundIndex + 1
      BetweenRange(calcBoundF (List.item lowerBoundIndex fTable) (List.item upperBoundIndex fTable) cost)

  let calcFactorF fTable directCost =
    match (fTable,directCost) with
    | LessThanMin f -> f
    | GreaterThanMax f -> f
    | BetweenRange f -> f
     
  let applyFactorF loadFactorFTable (DirectCost directCost) =
    let factorF = calcFactorF (loadFactorFTable()) (DirectCost directCost)
    directCost * factorF

  // Strip value after thousands
  let roundCost  = function 
    | a when a < 1000.0 -> a
    | b -> 
      b / 1000.0  
      |> System.Math.Truncate
      |> (*) 1000.0

  let estimateCost' loadFactorFTable = calcDirectCost >> (applyFactorF loadFactorFTable) >> roundCost 

  let estimateCost loadFactorFTable items : Result<float,string> = 
    let cost = estimateCost' loadFactorFTable items
    Ok cost

open BoQItem
open Project

let desc = "Pool Item"
let qty = Quantity (10.0, "m^2")
let material = Material { Name = "Big Tile"; UnitCost = 100.0; Unit = "m^2" }
let labor = Labor { Name = "Do Tiling"; UnitCost = 50.0; Unit = "m^2" }
let totalCost = 10.0*100.0 + 10.0*50.0

let item = 
  match BoQItem.create desc qty material labor with
  | Ok item' -> item'
  | _ -> failwith "Could not create item for updating"
  
let testItemDiffUnits = 
  let newMaterial, newLabor = 
    match material,labor with 
    | Material m, Labor lb ->  Material { m with Unit = "m"},  Labor {lb with Unit = "cm"}

  match BoQItem.create desc qty newMaterial newLabor with
  | Ok item' -> false
  | Error msg -> printfn "%s" msg; true

let testUpdateQty =
  let (Quantity (qtyVal,qtyUnit)) = qty
  let result = item |> updateQty (Quantity (qtyVal * 2.0, qtyUnit))
  match result with 
  | Ok item -> 
    let (Quantity (newQtyVal,_)) = item.Quantity
    newQtyVal = qtyVal * 2.0 && item.TotalCost = totalCost * 2.0
  | _ -> false

let loadFactorFTableFn = 
  function () -> FactorFTable [(10,1.1); (100,1.5); (1000, 1.9)]

estimateCost loadFactorFTableFn [item] 