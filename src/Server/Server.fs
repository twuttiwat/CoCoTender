module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

// module Storage =
//     let todos = ResizeArray()

//     let addTodo (todo: Todo) =
//         if Todo.isValid todo.Description then
//             todos.Add todo
//             Ok()
//         else
//             Error "Invalid todo"

//     do
//         addTodo (Todo.create "Create new SAFE project")
//         |> ignore

//         addTodo (Todo.create "Write your app") |> ignore
//         addTodo (Todo.create "Ship it !!!") |> ignore

// let todosApi =
//     { getTodos = fun () -> async { return Storage.todos |> List.ofSeq }
//       addTodo =
//         fun todo ->
//             async {
//                 return
//                     match Storage.addTodo todo with
//                     | Ok () -> todo
//                     | Error e -> failwith e
//             } }

module Storage =
    let boqItems = ResizeArray<BoQItemDto>()

    let addBoQItem (boqItem: BoQItemDto) =
        if (BoQItemDto.isValid boqItem) then
            boqItems.Add boqItem
            Ok ()
        else
            Error "Invalid boq item"

    do
        let defaultItem =
            BoQItemDto.create "Pool Tile" 10.0 "m^2" "Big Tile" 100.0
                            "Do Tiling" 50.0 1500.0
        addBoQItem defaultItem |> ignore
        addBoQItem { defaultItem with Quantity = 20.0 } |> ignore
        addBoQItem { defaultItem with Quantity = 30.0 } |> ignore

let cocoTenderApi =
    {
        getBoQItems = fun () -> async { return Storage.boqItems |> List.ofSeq }
        addBoQItem =
            fun boqItem ->
                async {
                    return
                        match Storage.addBoQItem boqItem with
                        | Ok () -> boqItem
                        | Error e -> failwith e
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