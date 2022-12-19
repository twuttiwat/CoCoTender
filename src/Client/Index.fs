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
    }

type Msg =
    | UrlChanged of Page

let init () : Model * Cmd<Msg> =
    let currentPage = Router.currentUrl() |> parseUrl
    let model =
        {
            PageName = "Index"
            CurrentPage = currentPage
        }

    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | UrlChanged page ->
        printfn "UrlChanged %A" page
        { model with CurrentPage = page } , Cmd.none

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
                    Bulma.title [
                        text.hasTextCentered
                        prop.text "CoCoTender"
                    ]
                    pageContent
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    let activePage =
        match model.CurrentPage with
        | Home -> Pages.Home.view()
        | BoQ -> Pages.BoQ.view()

        |> viewPage model dispatch

    React.router [
        router.onUrlChanged (parseUrl >> UrlChanged >> dispatch)
        router.children [ activePage ]
    ]