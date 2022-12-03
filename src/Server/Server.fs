module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open System
open Shared
open CoCoTender.Domain
open Project

module Storage =
    let boqItems = ResizeArray<BoQItem>()

    let addBoQItem (boqItem: BoQItem) =
        boqItems.Add boqItem
        Ok ()

    let updateBoQItem boqItem =
        let index = boqItems.FindIndex( fun x -> x = boqItem)
        boqItems[index] <- boqItem
        Ok ()

    let deleteBoQItem itemId =
        let index = boqItems.FindIndex( fun x -> x |> BoQItem.value |> fun y -> y.Id = itemId)
        boqItems.RemoveAt(index)
        Ok ()

    let loadFactorFTable () =
        FactorFTable [(10,1.1); (100,1.5); (1000, 1.9)]

    do
        let qty = Quantity (10.0, "m^2")
        let material = Material { Name = "Pool Tile"; Unit = "m^2"; UnitCost = 100.0 }
        let labor = Labor { Name = "Do Tiling"; Unit = "m^2"; UnitCost = 50.0 }
        let defaultItem = BoQItem.tryCreate (Guid.NewGuid()) "Pool Tile" qty material labor
        match defaultItem with
        | Ok defaultItem' ->
            addBoQItem defaultItem' |> ignore
        | _ -> ()

module Dto =
    let toBoQItemDto (domain:BoQItem) =
        let domainVal =  domain |> BoQItem.value
        let (Quantity (qty, qtyUnit)) = domainVal.Quantity
        let (Material material) = domainVal.MaterialUnitCost
        let (Labor labor) = domainVal.LaborUnitCost
        BoQItemDto.create domainVal.Id domainVal.Description qty qtyUnit
                          material.Name material.UnitCost labor.Name labor.UnitCost
                          domainVal.TotalCost

    let toBoQItemDomain (dto:BoQItemDto) =
        let qty = Quantity (dto.Quantity, dto.Unit)
        let material = Material { Name = dto.Material; Unit = dto.Unit; UnitCost = dto.MaterialUnitCost }
        let labor = Labor { Name = dto.Labor; Unit = dto.Unit; UnitCost = dto.LaborUnitCost }
        BoQItem.tryCreate dto.Id dto.Description qty material labor

let getAllCost () =
    match Project.tryGetAllCost Storage.loadFactorFTable (Storage.boqItems |> List.ofSeq) with
    | Ok (DirectCost directCost, EstimateCost estimateCost) ->
        { DirectCost = directCost; EstimateCost = estimateCost}
    | Error e ->
        failwith e

let cocoTenderApi =
    {
        getBoQItems = fun () -> async {
            let boqItems = Storage.boqItems |> List.ofSeq
            return (boqItems |> List.map Dto.toBoQItemDto), getAllCost()
        }
        addBoQItem =
            fun boqItemDto ->
                async {
                    return
                        match  boqItemDto |> Dto.toBoQItemDomain with
                        | Ok boqItem ->
                            match Storage.addBoQItem boqItem with
                            | Ok () -> boqItemDto, getAllCost()
                            | Error e -> failwith e
                        | Error e -> failwith e
                }
        updateBoQItem =
            fun boqItemDto ->
                async {
                    return
                        match  boqItemDto |> Dto.toBoQItemDomain with
                        | Ok boqItem ->
                            match boqItem |> BoQItem.tryUpdate with
                            | Ok updatedBoQItem ->
                                match Storage.updateBoQItem updatedBoQItem with
                                | Ok () -> (updatedBoQItem |> Dto.toBoQItemDto), getAllCost()
                                | Error e -> failwith e
                            | Error e -> failwith e
                        | Error e -> failwith e
                }
        deleteBoQItem =
            fun itemId ->
                async {
                    return
                        match Storage.deleteBoQItem itemId with
                        | Ok () -> ()
                        | Error e -> failwith e
                }
        getAllCost =
            fun () ->
                async {
                    let directCost = Storage.boqItems |> Seq.sumBy (fun x -> x |> BoQItem.value |> fun y -> y.TotalCost)
                    return
                        match Project.tryEstimateCost Storage.loadFactorFTable (Storage.boqItems |> List.ofSeq) with
                        | Ok estimateCost -> { DirectCost = directCost; EstimateCost = estimateCost}
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