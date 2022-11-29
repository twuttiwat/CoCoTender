module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

module Storage =
    let todos = ResizeArray()

    let addTodo (todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok()
        else
            Error "Invalid todo"

    do
        addTodo (Todo.create "Create new SAFE project")
        |> ignore

        addTodo (Todo.create "Write your app") |> ignore
        addTodo (Todo.create "Ship it !!!") |> ignore

let todosApi =
    { getTodos = fun () -> async { return Storage.todos |> List.ofSeq }
      addTodo =
        fun todo ->
            async {
                return
                    match Storage.addTodo todo with
                    | Ok () -> todo
                    | Error e -> failwith e
            } }

let cocoTenderApi =
    {
        getBoQItems = fun () -> async {
            let item : BoQItemDto = {
                Description = "Pool Tile"
                Quantity = 10.0
                Unit = "m^2"
                Material = "Big Tile"
                MaterialUnit = "m^2"
                MaterialUnitCost = 100.0
                Labor = "Do Tiling"
                LaborUnit = "m^2"
                LaborUnitCost = 50.0
                TotalCost = 1500.0
            }

            return [
                item
                { item with Quantity = item.Quantity * 2.0 }
                { item with Quantity = item.Quantity * 3.0 }
            ]
        }
    }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    //|> Remoting.fromValue todosApi
    |> Remoting.fromValue cocoTenderApi
    |> Remoting.buildHttpHandler

let app =
    application {
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

[<EntryPoint>]
let main _ =
    run app
    0