module Index

open System
open Elmish
open Fable.Remoting.Client
open Elmish.Toastr
open Feliz.Router
open MyRouter
open Shared

type Model =
    {
        PageName : string
        CurrentPage : Page
        CurrentUrl : string list
    }

type Msg =
    | UrlChanged of string list

let init () : Model * Cmd<Msg> =
    let model =
        {
            PageName = "Index"
            CurrentPage = Page.Home
            CurrentUrl = Router.currentUrl()
        }

    let currentPage =
        match model.CurrentUrl with
        | [] -> Home
        | [ "boq" ] -> BoQ
        | _ -> Home

    { model with CurrentPage = currentPage }, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UrlChanged segments ->
        printfn "UrlChanged %A" segments
        let currentPage =
            match segments with
            | [] -> Home
            | [ "boq" ] -> BoQ
            | _ -> Home
        { model with CurrentPage = currentPage } , Cmd.none

open Feliz
open Feliz.Router
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

let viewPage (model: Model) (dispatch: Msg -> unit) (pageContent:ReactElement) =
    Bulma.hero [
        hero.isFullHeight
        color.isDark
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    pageContent
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    let activePage () =
        match model.CurrentPage with
        | Home -> Pages.Home.view()
        | BoQ -> Pages.BoQ.view()

        |> viewPage model dispatch

    React.router [
        router.onUrlChanged (UrlChanged >> dispatch)
        router.children [ activePage() ]
    ]