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

    let recalc item =
        { item with TotalCost = item.Quantity * item.MaterialUnitCost + item.Quantity * item.LaborUnitCost}

type AllCost =
    {
        DirectCost : float
        FactorF : float
        EstimateCost : float
    }

type FactorFInfo = (string*float) list

type ICoCoTenderApi = {
    getBoQItems: unit -> Async<BoQItemDto list*AllCost>
    addBoQItem: BoQItemDto -> Async<BoQItemDto*AllCost>
    updateBoQItem: BoQItemDto -> Async<BoQItemDto*AllCost>
    deleteBoQItem: Guid -> Async<unit>
    getAllCost: unit -> Async<AllCost>
    getFactorFInfo: unit->Async<FactorFInfo>
}

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName
