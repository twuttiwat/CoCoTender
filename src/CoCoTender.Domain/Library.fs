namespace CoCoTender.Domain

open System
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

[<CustomEquality; NoComparison>]
type BoQItem = private {
    Id : Guid
    Description : string
    Quantity : Quantity
    MaterialUnitCost : MaterialUnitCost
    LaborUnitCost : LaborUnitCost
    TotalCost : float
  }
  with
  override this.Equals(obj) =
    match obj with
    | :? BoQItem as item -> this.Id = item.Id
    | _ -> false
  override this.GetHashCode () =
    hash this.Id

module BoQItem =

  let areUnitsMatched  qty materialUnitCost laborUnitCost =
    match qty, materialUnitCost, laborUnitCost with
    | Quantity (_, qtyUnit), Material {Unit = materialUnit}, Labor {Unit = laborUnit} ->
      qtyUnit = materialUnit && qtyUnit = laborUnit

  let tryRecalcTotalCost item =
    let calcTotalCost() =
      match item.Quantity, item.MaterialUnitCost, item.LaborUnitCost with
      | Quantity (qty,_), Material {UnitCost = mUnitCost}, Labor {UnitCost = lbUnitCost} ->
        (qty * mUnitCost) + (qty * lbUnitCost)

    result
      {
        do! areUnitsMatched item.Quantity item.MaterialUnitCost item.LaborUnitCost |> Result.requireTrue "Unit are Not matched."
        return { item with TotalCost = calcTotalCost() }
      }


  let tryCreate newId desc qty materialUnitCost laborUnitCost = result {

    do! (String.IsNullOrWhiteSpace desc |> not) |> Result.requireTrue "Description is required."

    do! areUnitsMatched qty materialUnitCost laborUnitCost |> Result.requireTrue "Units are Not matched."

    let! item =
      {
        Id = newId
        Description = desc
        Quantity = qty
        MaterialUnitCost = materialUnitCost
        LaborUnitCost = laborUnitCost
        TotalCost = 0.0
      }
      |> tryRecalcTotalCost

    return item
  }

  let value item =
    {|
      Id = item.Id
      Description = item.Description
      Quantity = item.Quantity
      MaterialUnitCost = item.MaterialUnitCost
      LaborUnitCost = item.LaborUnitCost
      TotalCost = item.TotalCost
    |}

  let updateDesc newDesc item =
    { item with Description = newDesc }

  let tryUpdateQty newQty item =
    { item with Quantity = newQty }
    |> tryRecalcTotalCost

  let tryUpdateMaterialUnitCost newMaterialUnitCost item =
    { item with MaterialUnitCost = newMaterialUnitCost }
    |> tryRecalcTotalCost

  let tryUpdateLaborUnitCost newLaborUnitCost item =
    { item with LaborUnitCost = newLaborUnitCost }
    |> tryRecalcTotalCost

  let tryUpdate boqItem =  boqItem |> tryRecalcTotalCost

module Project =

  open BoQItem

  type DirectCost = DirectCost of float
  type EstimateCost = EstimateCost of float
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

  let tryEstimateCost loadFactorFTable items : Result<float,string> =
    let cost = estimateCost' loadFactorFTable items
    Ok cost

  let tryGetAllCost loadFactorFTable boqItems = result {
    let directCost = boqItems |> Seq.sumBy (fun x -> x |> BoQItem.value |> fun y -> y.TotalCost)
    let! estimateCost = tryEstimateCost loadFactorFTable boqItems
    return (DirectCost directCost, EstimateCost estimateCost)
  }