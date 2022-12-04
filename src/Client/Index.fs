module Index

open System
open Elmish
open Fable.Remoting.Client
open Shared

type Model =
    {
        BoQItems: BoQItemDto list
        AllCost: AllCost
        ShowFactorFView: bool
        FactorFInfo: FactorFInfo
    }

type Msg =
    | GetBoQItems
    | GotBoQItems of BoQItemDto list*AllCost
    | AddBoQItem
    | AddedBoQItem of BoQItemDto*AllCost
    | UpdateBoQItem of BoQItemDto
    | UpdatedBoQItem of BoQItemDto*AllCost
    | DeleteBoQItem of itemId:Guid
    | ToggleFactorFView
    | GotFactorFInfo of FactorFInfo

let cocoTenderApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ICoCoTenderApi>

let defaultNewItem () =
    BoQItemDto.create (Guid.NewGuid()) "New Item" 0.0 "m" "Material 1"
        0.0 "Labor 1" 0.0 0.0

let init () : Model * Cmd<Msg> =
    let model =
        {
            BoQItems = []
            AllCost = {DirectCost = 0.0; FactorF = 0.0; EstimateCost = 0.0}
            ShowFactorFView = false
            FactorFInfo = []
        }

    let cmdGetBoQItems = Cmd.OfAsync.perform cocoTenderApi.getBoQItems () GotBoQItems
    let cmdGetFactorFInfo = Cmd.OfAsync.perform cocoTenderApi.getFactorFInfo () GotFactorFInfo

    model, Cmd.batch [ cmdGetBoQItems; cmdGetFactorFInfo ]

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GetBoQItems ->
        let cmd = Cmd.OfAsync.perform cocoTenderApi.getBoQItems () GotBoQItems
        model, cmd
    | GotBoQItems (items,allCost) -> { model with BoQItems = items; AllCost = allCost }, Cmd.none

    | AddBoQItem ->
        let cmd = Cmd.OfAsync.perform cocoTenderApi.addBoQItem (defaultNewItem()) AddedBoQItem
        model, cmd
    | AddedBoQItem (boqItem, allCost) ->
        { model with BoQItems = model.BoQItems @ [ boqItem ]; AllCost = allCost }, Cmd.none

    | UpdateBoQItem boqItem ->
        let cmd = Cmd.OfAsync.perform cocoTenderApi.updateBoQItem boqItem UpdatedBoQItem
        model, cmd
    | UpdatedBoQItem (updatedItem,allCost) ->
        let updatedItems = model.BoQItems |> List.map (fun x -> if x.Id = updatedItem.Id then updatedItem else x)
        { model with BoQItems = updatedItems; AllCost = allCost }, Cmd.none

    | DeleteBoQItem itemId ->
        let cmd = Cmd.OfAsync.perform cocoTenderApi.deleteBoQItem itemId (fun _ -> GetBoQItems)
        model, cmd

    | GotFactorFInfo info -> { model with FactorFInfo = info }, Cmd.none

    | ToggleFactorFView ->
        { model with ShowFactorFView = model.ShowFactorFView |> not }, Cmd.none

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
                AgGrid.onGridReady (fun x -> x.AutoSizeAllColumns())
                AgGrid.singleClickEdit true
                AgGrid.enableCellTextSelection true
                AgGrid.ensureDomOrder true
                AgGrid.columnDefs [
                    ColumnDef.create<string> [
                        ColumnDef.headerName "Description"
                        ColumnDef.valueGetter (fun x -> x.Description)
                        ColumnDef.valueSetter (fun newValue _ row  -> { row with Description = newValue } |> UpdateBoQItem |> dispatch)
                        ColumnDef.editable (fun _ _ -> true)
                    ]
                    ColumnDef.create<float> [
                        ColumnDef.headerName "Quantity"
                        ColumnDef.valueGetter (fun x -> x.Quantity)
                        ColumnDef.valueSetter (fun newValue _ row  -> { row with Quantity = newValue |> float  } |> UpdateBoQItem |> dispatch)
                        ColumnDef.editable (fun _ _ -> true)
                        ColumnDef.width 75
                    ]
                    ColumnDef.create<string> [
                        ColumnDef.headerName "Unit"
                        ColumnDef.valueGetter (fun x -> x.Unit)
                        ColumnDef.valueSetter (fun newValue _ row  -> { row with Unit = newValue } |> UpdateBoQItem |> dispatch)
                        ColumnDef.editable (fun _ _ -> true)
                        ColumnDef.width 50
                    ]
                    ColumnDef.create<string> [
                        ColumnDef.headerName "Material"
                        ColumnDef.valueGetter (fun x -> x.Material)
                        ColumnDef.valueSetter (fun newValue _ row  -> { row with Material = newValue } |> UpdateBoQItem |> dispatch)
                        ColumnDef.editable (fun _ _ -> true)
                        ColumnDef.width 150
                    ]
                    ColumnDef.create<float> [
                        ColumnDef.headerName "M. UnitCost"
                        ColumnDef.valueGetter (fun x -> x.MaterialUnitCost)
                        ColumnDef.valueSetter (fun newValue _ row  -> { row with MaterialUnitCost = newValue |> float  } |> UpdateBoQItem |> dispatch)
                        ColumnDef.editable (fun _ _ -> true)
                        ColumnDef.width 75
                    ]
                    ColumnDef.create<string> [
                        ColumnDef.headerName "Labor"
                        ColumnDef.valueGetter (fun x -> x.Labor)
                        ColumnDef.valueSetter (fun newValue _ row  -> { row with Labor = newValue } |> UpdateBoQItem |> dispatch)
                        ColumnDef.editable (fun _ _ -> true)
                        ColumnDef.width 150
                    ]
                    ColumnDef.create<float> [
                        ColumnDef.headerName "L. UnitCost"
                        ColumnDef.valueGetter (fun x -> x.LaborUnitCost)
                        ColumnDef.valueSetter (fun newValue _ row  -> { row with LaborUnitCost = newValue |> float  } |> UpdateBoQItem |> dispatch)
                        ColumnDef.editable (fun _ _ -> true)
                        ColumnDef.width 75
                    ]
                    ColumnDef.create<float> [
                        ColumnDef.headerName "Total Cost"
                        ColumnDef.valueGetter (fun x -> x.TotalCost)
                        ColumnDef.width 75
                    ]
                    ColumnDef.create<Guid> [
                        ColumnDef.valueGetter (fun x -> x.Id)
                        ColumnDef.cellRendererFramework (fun itemId _ ->
                            Html.button [
                                prop.text "🗑️"
                                prop.onClick (fun _ -> dispatch (DeleteBoQItem itemId))
                            ]
                        )
                    ]
                ]
            ]
            Bulma.level [
                prop.style [
                    style.paddingRight 300
                    style.fontSize 14
                    style.fontWeight.bold
                ]
                color.hasBackgroundLight
                prop.children [
                    Bulma.levelLeft []
                    Bulma.levelRight [
                        Bulma.levelItem (Bulma.label $"Direct Cost")
                        Bulma.levelItem (Bulma.label $"{model.AllCost.DirectCost}")
                        Bulma.levelItem (Bulma.label $"Estimate Cost")
                        Bulma.levelItem (Bulma.label $"{model.AllCost.EstimateCost}")
                        Bulma.levelItem (
                            Bulma.button.button [
                                prop.text "Show Factor F"
                                prop.onClick (fun _ -> ToggleFactorFView |> dispatch)
                            ]
                        )
                    ]
                ]
            ]
        ]
    ]

let factorFView (model: Model) dispatch =
    printfn "Info %A Factor F %A" model.FactorFInfo model.AllCost.FactorF
    let lowerBoundIndex = model.FactorFInfo |> List.tryFindIndexBack (fun x -> x |> snd |> (>=) model.AllCost.FactorF)
    printfn "LowerBoundIndex %A" lowerBoundIndex
    let factorFTr i (condition: string,factorF) =
        let isSelected =
            match lowerBoundIndex with
            | Some index -> i = index || i = (index + 1)
            | _ -> false

        Html.tr [
            if isSelected then yield prop.className "is-selected"
            yield prop.children [Html.td condition; Html.td (factorF |> string)]
        ]

    QuickView.quickview [
        if model.ShowFactorFView then yield quickview.isActive
        yield prop.children [
            QuickView.header [
                Html.div [
                    prop.style [ style.color.black ]
                    prop.text "Factor F"
                ]
                Bulma.delete [ prop.onClick (fun _ -> ToggleFactorFView |> dispatch) ]
            ]
            QuickView.body [
                QuickView.block [
                    Bulma.table [
                        Html.thead [
                            Html.tr [ Html.th "Direct Cost Condition"; Html.th "Factor F" ]
                        ]
                        Html.tbody (model.FactorFInfo |> List.mapi factorFTr)
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
                    Bulma.title [
                        text.hasTextCentered
                        prop.text "CoCoTender"
                    ]
                    containerBox model dispatch
                    factorFView model dispatch
                ]
            ]
        ]
    ]