namespace Shared

open System

type Todo = { Id: Guid; Description: string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }

type ITodosApi =
    { getTodos: unit -> Async<Todo list>
      addTodo: Todo -> Async<Todo> }

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

    let create description quantity unit material materialUnitCost labor laborUnitCost totalCost =
        {
            Id = Guid.NewGuid()
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
      addBoQItem: BoQItemDto -> Async<BoQItemDto> }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName
