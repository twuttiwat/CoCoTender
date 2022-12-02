module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared
open CoCoTender.Domain

module Storage =
    let boqItems = ResizeArray<BoQItem>()

    let addBoQItem (boqItem: BoQItem) =
        boqItems.Add boqItem
        Ok ()

    do
        let qty = Quantity (10.0, "m^2")
        let material = Material { Name = "Pool Tile"; Unit = "m^2"; UnitCost = 100.0 }
        let labor = Labor { Name = "Do Tiling"; Unit = "m^2"; UnitCost = 50.0 }
        let defaultItem = BoQItem.tryCreate "Pool Tile" qty material labor
        match defaultItem with
        | Ok defaultItem' ->
            addBoQItem defaultItem' |> ignore
            addBoQItem defaultItem' |> ignore
            addBoQItem defaultItem' |> ignore
        | _ -> ()

module Dto =
    let toBoQItemDto (domain:BoQItem) =
        let domainVal =  domain |> BoQItem.value
        let (Quantity (qty, qtyUnit)) = domainVal.Quantity
        let (Material material) = domainVal.MaterialUnitCost
        let (Labor labor) = domainVal.LaborUnitCost
        BoQItemDto.create domainVal.Description qty qtyUnit
                          material.Name material.UnitCost labor.Name labor.UnitCost
                          domainVal.TotalCost

    let toBoQItemDomain (dto:BoQItemDto) =
        let qty = Quantity (dto.Quantity, dto.Unit)
        let material = Material { Name = dto.Material; Unit = dto.Unit; UnitCost = dto.MaterialUnitCost }
        let labor = Labor { Name = dto.Labor; Unit = dto.Unit; UnitCost = dto.LaborUnitCost }
        BoQItem.tryCreate dto.Description qty material labor

let cocoTenderApi =
    {
        getBoQItems = fun () -> async { return Storage.boqItems |> List.ofSeq |> List.map Dto.toBoQItemDto }
        addBoQItem =
            fun boqItemDto ->
                async {
                    return
                        match  boqItemDto |> Dto.toBoQItemDomain with
                        | Ok boqItem ->
                            match Storage.addBoQItem boqItem with
                            | Ok () -> boqItemDto
                            | Error e -> failwith e
                        | Error e -> failwith e
                }
    }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
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