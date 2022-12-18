module Pages.Template

open System
open Elmish
open Fable.Remoting.Client
open Elmish.Toastr
open Shared

type Model =
    {
        PageName : string
    }

type Msg =
    | DoNothing


let init () : Model * Cmd<Msg> =
    { PageName = "@Template" }, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | DoNothing ->
        model, Cmd.none

open Feliz
open Feliz.Bulma

let view = React.functionComponent(fun () ->
    let (model: Model), dispatch = React.useElmish(init, update, [| |])

    Bulma.title [
        text.hasTextCentered
        prop.text model.PageName
    ]
)
