namespace Shared

open System

type BoQItemDto = {
    Id : Guid
    Description : string
    Quantity : float
    Unit : string
    Material : string
    MaterialUnitCost : float
    Labor : string
    LaborUnitCost : float
    TotalCost : float
}

module BoQItemDto =
    let isValid item =
        String.IsNullOrWhiteSpace item.Description |> not

    let recalc item =
        { item with TotalCost = item.Quantity * item.MaterialUnitCost + item.Quantity * item.LaborUnitCost}

    let create itemId description quantity unit material materialUnitCost labor laborUnitCost totalCost =
        {
            Id = itemId
            Description = description
            Quantity = quantity
            Unit = unit
            Material = material
            MaterialUnitCost = materialUnitCost
            Labor = labor
            LaborUnitCost = laborUnitCost
            TotalCost = totalCost
        }
        |> recalc


type AllCost =
    {
        DirectCost : float
        FactorF : float
        EstimateCost : float
    }

type FactorFInfo = (string*float) list

type ICoCoTenderApi = {
    getBoQItems: unit -> Async<Result<BoQItemDto list*AllCost,string>>
    addBoQItem: BoQItemDto -> Async<Result<BoQItemDto*AllCost,string>>
    updateBoQItem: BoQItemDto -> Async<Result<BoQItemDto*AllCost,string>>
    deleteBoQItem: Guid -> Async<Result<unit,string>>
    getAllCost: unit -> Async<Result<AllCost,string>>
    getFactorFInfo: unit->Async<Result<FactorFInfo,string>>
}

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName
