module Pages.Login

open Elmish
open Fable.Remoting.Client
open Shared
open Feliz.Router

type Model =
    {
        Email : string
        Password : string
    }

type Msg =
    | UpdateEmail of email:string
    | UpdatePassword of password:string
    | Login
    | LoggedIn of Result<Token, string>
    | GotError of exn

let authApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IAuthApi>

let init () : Model * Cmd<Msg> =
    let model =
        {
            Email = "foo@bar.com"
            Password = "ok"
        }

    model, Cmd.none

open Fable.SimpleJson


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    let showError' msg = printf "%s" msg

    match msg with
    | UpdateEmail email ->
        { model with Email = email }, Cmd.none

    | UpdatePassword pwd ->
        { model with Password = pwd }, Cmd.none

    | Login ->
        printf "Loggin In"
        let cmd = Cmd.OfAsync.either authApi.login (model.Email, model.Password) LoggedIn GotError
        model, cmd

    | LoggedIn (Ok (Token token)) ->
        printf $"Token {token}"
        TokenStorage.setToken token
        let cmd = Cmd.navigatePath( [|"secured"|])
        model, cmd

    | LoggedIn (Error msg) ->
        showError' msg
        model, Cmd.none

    | GotError ex ->
        printf $"LoggedIn Error {ex.Message}"
        // showError' msg
        model, Cmd.none

open Feliz
open Feliz.Bulma

open Feliz.Router

let view' (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.children [
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.field.div [
                                Bulma.label "Email"
                                Bulma.control.div [
                                    Bulma.input.text [
                                        prop.value model.Email
                                        prop.placeholder "a@b.com"
                                        prop.onChange (fun (email:string) ->  email |> UpdateEmail |> dispatch)
                                    ]
                                ]
                            ]
                            Bulma.field.div [
                                Bulma.label "Password"
                                Bulma.control.div [
                                    Bulma.input.password [
                                        prop.value model.Password
                                        prop.placeholder "*****"
                                        prop.onChange (fun (pwd:string) ->  pwd |> UpdatePassword |> dispatch)
                                    ]
                                ]
                            ]
                            Bulma.field.div [
                                Bulma.field.isGrouped
                                Bulma.field.isGroupedCentered
                                prop.children [
                                    Bulma.control.div [
                                        Bulma.button.button [
                                            Bulma.color.isLink
                                            prop.text "Login"
                                            prop.onClick (fun _ -> dispatch Login)
                                        ]
                                    ]
                                ]
                            ]

                        ]
                    ]
                ]
            ]
        ]
    ]


open Feliz.UseElmish

let view = React.functionComponent(fun () ->
    let (model: Model), dispatch = React.useElmish(init, update, [| |])

    view' model dispatch
)
