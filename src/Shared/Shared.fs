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

type ICoCoTenderApi =
    { getBoQItems: unit -> Async<BoQItemDto list>
      addBoQItem: BoQItemDto -> Async<BoQItemDto>
      updateBoQItem: BoQItemDto -> Async<BoQItemDto> }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName
