type MaybeBuilder() =

  member _.Bind(x, f) =
    match x with
    | None -> None
    | Some a -> f a

  member _.Return(x) =
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

  let estimateCost loadFactorFTable = calcDirectCost >> (applyFactorF loadFactorFTable) >> roundCost 

///
/// Tests
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
    return (poolItem' |> BoQItem.value |> fun x -> x.TotalCost) = 1500
  }
  |> Option.defaultValue false

let testUpdateDesc = 
  maybe {
    let! poolItem' = poolItem
    let updatedItem = poolItem' |> updateDesc "New Pool"
    return (updatedItem |> BoQItem.value |> fun x -> x.Description) = "New Pool" 
  }
  |> Option.defaultValue false

let testUpdateQty = 
  maybe {
    let! poolItem' = poolItem
    let newQty = Quantity (20,"m^2")
    let! updatedPoolItem = tryUpdateQty newQty poolItem'
    return (updatedPoolItem |> BoQItem.value |> fun x -> x.TotalCost) = 3000
  }
  |> Option.defaultValue false

let testUpdateMaterialUnitCost = 
  maybe {
    let! poolItem' = poolItem
    let newMaterialUnitCost = Material <| { Name = "Small Tile"; UnitCost = 50; Unit = "m^2"}
    let! updatedPoolItem = tryUpdateMaterialUnitCost newMaterialUnitCost poolItem'
    return (updatedPoolItem |> BoQItem.value |> fun x -> x.TotalCost) = 1000
  }
  |> Option.defaultValue false

let testUpdateLaborUnitCost = 
  maybe { 
    let! poolItem' = poolItem
    let newLaborUnitCost = Labor <| { Name = "Do Tiling #2"; UnitCost = 100; Unit = "m^2"}
    let! updatedPoolItem = tryUpdateLaborUnitCost newLaborUnitCost poolItem'
    return (updatedPoolItem |> BoQItem.value |> fun x -> x.TotalCost) = 2000
  }
  |> Option.defaultValue false

open Project

let testCalcDirectCost = (calcDirectCost []) = (DirectCost 0)
let testCalcDirectCost' = 
  poolItem 
  |> Option.map (fun poolItem' -> (=) (calcDirectCost [poolItem'; poolItem']) (DirectCost 3000))
  |> Option.defaultValue false
let factorFTable1 = FactorFTable [(10,1.1); (100,1.5); (1000, 1.9)]
let loadFactorFTableTest () = factorFTable1 
let testCalcFactorF directCost factorF = (=) (calcFactorF factorFTable1  (DirectCost directCost)) factorF
let testCalcFactorFMin = testCalcFactorF 9 1.1
let testCalcFactorFMax = testCalcFactorF 1001 1.9
let testCalcFactorBound = testCalcFactorF 55 1.3
let testApplyFactorF = (=) (applyFactorF loadFactorFTableTest (DirectCost 55)) (1.3 * 55.0)
let testEstimateCost = 
  match poolItem with
  | Some poolItem' -> 
    (=) (estimateCost loadFactorFTableTest [poolItem']) 2000
  | None -> false

let calcBoundF lowerBound upperBound directCost =
  let lowerBoundCost,lowerBoundF = lowerBound
  let upperBoundCost,upperBoundF = upperBound
  let fRange = upperBoundF - lowerBoundF
  let costRatio = (directCost - lowerBoundCost) / (upperBoundCost - lowerBoundCost)
  costRatio * fRange + lowerBoundF

let testCalcBoundF = (=) (calcBoundF (10.0,1.1) (100.0,1.5) 55.0) 1.3 

