namespace Shared

open System

type Todo = { Id: Guid; Description: string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ITodosApi =
    { getTodos: unit -> Async<Todo list>
      addTodo: Todo -> Async<Todo> }

type BoQItemDto = {
    Description : string
    Quantity : float
    Unit : string
    Material : string
    MaterialUnit : string
    MaterialUnitCost : float
    Labor : string
    LaborUnit : string
    LaborUnitCost : float
    TotalCost : float
}

type ICoCoTenderApi =
    { getBoQItems: unit -> Async<BoQItemDto list> }
