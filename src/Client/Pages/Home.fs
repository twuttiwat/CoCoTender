module Pages.Home

open System
open Elmish
open Fable.Remoting.Client
open Elmish.Toastr
open Shared

type ProjectDto = {
    Id : Guid
    Name : string
    EstimateCost : float
}

type Model =
    {
        PageName : string
        Projects : ProjectDto list
    }

type Msg =
    | DoNothing

let init () : Model * Cmd<Msg> =
    let model =
        {
            PageName = "Home"
            Projects = [ for _ in 1..3 do {Id = Guid.NewGuid(); Name = "Swimming Pool"; EstimateCost = 1500.0} ]
        }

    model, Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | DoNothing ->
        model, Cmd.none

open Feliz
open Feliz.Router
open Feliz.UseElmish
open Feliz.Bulma
open Feliz.AgGrid


let projectsGrid model dispatch =

    Html.div [
        prop.className ThemeClass.Balham
        prop.children [
            Bulma.button.button [
                color.isInfo
                prop.text "Add"
            ]
            AgGrid.grid [
                AgGrid.rowData (model.Projects |> Array.ofList)
                AgGrid.defaultColDef [
                    ColumnDef.resizable true
                ]
                AgGrid.domLayout AutoHeight
                AgGrid.onGridReady (fun x -> x.AutoSizeAllColumns())
                AgGrid.enableCellTextSelection true
                AgGrid.ensureDomOrder true
                AgGrid.columnDefs [
                    ColumnDef.create<string> [
                        ColumnDef.headerName "Name"
                        ColumnDef.valueGetter (fun x -> x.Name)
                    ]
                    ColumnDef.create<float> [
                        ColumnDef.headerName "Estimate Cost"
                        ColumnDef.valueGetter (fun x -> x.EstimateCost)
                        ColumnDef.width 75
                    ]
                    ColumnDef.create<Guid> [
                        ColumnDef.valueGetter (fun x -> x.Id)
                        ColumnDef.cellRendererFramework (fun id _ ->
                            Html.a [
                                prop.href (Router.format("boq"))
                                prop.text "Edit"
                                // prop.onClick (fun _ -> Router.navigatePath [| "boq"|] )
                            ]
                        )
                    ]
                ]
            ]
        ]
    ]

let view = React.functionComponent(fun () ->
    let (model: Model), dispatch = React.useElmish(init, update, [| |])

    projectsGrid model dispatch
)
