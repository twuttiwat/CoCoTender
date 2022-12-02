module Index

open System
open Elmish
open Fable.Remoting.Client
open Shared

type Model = { BoQItems: BoQItemDto list; Input: string }

type Msg =
    | GotBoQItems of BoQItemDto list
    | SetInput of string
    | AddBoQItem
    | AddedBoQItem of BoQItemDto

let cocoTenderApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ICoCoTenderApi>

let defaultNewItem =
    BoQItemDto.create "New Item" 0.0 "m" "Material 1"
        0.0 "Labor 1" 0.0 0.0

let init () : Model * Cmd<Msg> =
    let model = { BoQItems = []; Input = "" }

    let cmd = Cmd.OfAsync.perform cocoTenderApi.getBoQItems () GotBoQItems

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotBoQItems items -> { model with BoQItems = items }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | AddBoQItem ->
        let cmd = Cmd.OfAsync.perform cocoTenderApi.addBoQItem defaultNewItem AddedBoQItem

        { model with Input = "" }, cmd
    | AddedBoQItem boqItem -> { model with BoQItems = model.BoQItems @ [ boqItem ] }, Cmd.none

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

open Feliz.AgGrid

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Html.div [
        prop.className ThemeClass.Balham
        prop.children [
            Bulma.button.button [
                color.isPrimary
                prop.onClick (fun _ -> dispatch AddBoQItem)
                prop.text "Add"
            ]
            AgGrid.grid [
                AgGrid.rowData (model.BoQItems |> Array.ofList)
                AgGrid.defaultColDef [
                    ColumnDef.resizable true
                ]
                AgGrid.domLayout AutoHeight

                AgGrid.onColumnGroupOpened (fun x -> x.AutoSizeGroupColumns())
                AgGrid.onGridReady (fun x -> x.AutoSizeAllColumns())
                AgGrid.ensureDomOrder true
                AgGrid.columnDefs [
                    ColumnDef.create<string> [
                        ColumnDef.headerName "Description"
                        ColumnDef.valueGetter (fun x -> x.Description)
                    ]
                    ColumnDef.create<float> [
                        ColumnDef.headerName "Quantity"
                        ColumnDef.valueGetter (fun x -> x.Quantity)
                        ColumnDef.width 75
                    ]
                    ColumnDef.create<string> [
                        ColumnDef.headerName "Unit"
                        ColumnDef.valueGetter (fun x -> x.Unit)
                        ColumnDef.width 50
                    ]
                    ColumnDef.create<string> [
                        ColumnDef.headerName "Material"
                        ColumnDef.valueGetter (fun x -> x.Material)
                        ColumnDef.width 150
                    ]
                    ColumnDef.create<float> [
                        ColumnDef.headerName "M. UnitCost"
                        ColumnDef.valueGetter (fun x -> x.MaterialUnitCost)
                        ColumnDef.width 75
                    ]
                    ColumnDef.create<string> [
                        ColumnDef.headerName "Labor"
                        ColumnDef.valueGetter (fun x -> x.Labor)
                        ColumnDef.width 150
                    ]
                    ColumnDef.create<float> [
                        ColumnDef.headerName "L. UnitCost"
                        ColumnDef.valueGetter (fun x -> x.LaborUnitCost)
                        ColumnDef.width 75
                    ]
                    ColumnDef.create<float> [
                        ColumnDef.headerName "Total Cost"
                        ColumnDef.valueGetter (fun x -> x.TotalCost)
                        ColumnDef.width 75
                    ]
                ]
            ]
        ]
    ]
    // Bulma.box [
    //     Bulma.content [
    //         Html.ol [
    //             for boqItem in model.BoQItems do
    //                 let itemText = $"{boqItem.Description} {boqItem.Quantity} {boqItem.Unit}"
    //                 Html.li [ prop.text itemText ]
    //         ]
    //     ]
    //     Html.div [
    //         AgGrid.grid [
    //             AgGrid.rowData (model.BoQItems |> Array.ofList)
    //             AgGrid.columnDefs [
    //                 ColumnDef.create<string> [
    //                     ColumnDef.headerName "Description"
    //                 ]
    //             ]
    //         ]
    //     ]
    //     Bulma.field.div [
    //         field.isGrouped
    //         prop.children [
    //             Bulma.control.p [
    //                 control.isExpanded
    //                 prop.children [
    //                     Bulma.input.text [
    //                         prop.value model.Input
    //                         prop.placeholder "What needs to be done?"
    //                         prop.onChange (fun x -> SetInput x |> dispatch)
    //                     ]
    //                 ]
    //             ]
    //             Bulma.control.p [
    //                 Bulma.button.button [
    //                     color.isPrimary
    //                     prop.disabled (String.IsNullOrWhiteSpace model.Input)
    //                     prop.onClick (fun _ -> dispatch AddBoQItem)
    //                     prop.text "Add"
    //                 ]
    //             ]
    //         ]
    //     ]
    // ]

let view (model: Model) (dispatch: Msg -> unit) =
    // Bulma.container [
    //     containerBox model dispatch
    // ]
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
                    Bulma.title [
                        text.hasTextCentered
                        prop.text "CoCoTender"
                    ]
                    containerBox model dispatch
                ]
            ]
        ]
    ]