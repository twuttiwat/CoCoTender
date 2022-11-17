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
      let lowerBoundIndex = 
        printfn "lowerBoundIndex fTable %A cost %A" fTable cost 
        fTable |> List.findIndexBack (fun (cost',_) -> cost > cost')
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
