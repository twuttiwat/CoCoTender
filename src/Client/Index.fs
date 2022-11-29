module Index

open Elmish
open Fable.Remoting.Client
open Shared

//type Model = { Todos: Todo list; Input: string }
type Model = { BoQItems: BoQItemDto list; Input: string }

type Msg =
    | GotBoQItems of BoQItemDto list
    | SetInput of string
    //| AddTodo
    //| AddedTodo of Todo

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let cocoTenderApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ICoCoTenderApi>

let init () : Model * Cmd<Msg> =
    let model = { BoQItems = []; Input = "" }

    let cmd = Cmd.OfAsync.perform cocoTenderApi.getBoQItems () GotBoQItems

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotBoQItems items -> { model with BoQItems = items }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    // | AddTodo ->
    //     let todo = Todo.create model.Input

    //     let cmd = Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

    //     { model with Input = "" }, cmd
    // | AddedTodo todo -> { model with Todos = model.Todos @ [ todo ] }, Cmd.none

open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for boqItem in model.BoQItems do
                    let itemText = $"{boqItem.Description} {boqItem.Quantity} {boqItem.Unit}"
                    Html.li [ prop.text itemText ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.Input
                            prop.placeholder "What needs to be done?"
                            prop.onChange (fun x -> SetInput x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Todo.isValid model.Input |> not)
                        //prop.onClick (fun _ -> dispatch AddTodo)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "CoCoTender"
                            ]
                            containerBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]