module Server

open FsToolkit.ErrorHandling
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open System
open LiteDB.FSharp
open LiteDB

open Shared
open CoCoTender.Domain
open Project

type IStorageApi =
    abstract member getBoQItems : unit -> Result<BoQItem list,string>
    abstract member addBoQItem : boqItem:BoQItem -> Result<unit, string>
    abstract member updateBoQItem : boqItem:BoQItem -> Result<unit, string>
    abstract member deleteBoQItem : itemId:Guid -> Result<Unit, string>
    abstract member loadFactorFTable : unit -> FactorFTable

type ResizeArrayStorage() =
    let boqItems = ResizeArray<BoQItem>()
    do
        let qty = Quantity (10.0, "m^2")
        let material = Material { Name = "Pool Tile"; Unit = "m^2"; UnitCost = 100.0 }
        let labor = Labor { Name = "Do Tiling"; Unit = "m^2"; UnitCost = 50.0 }
        let defaultItem = BoQItem.tryCreate (Guid.NewGuid()) "Pool Tile" qty material labor
        match defaultItem with
        | Ok defaultItem' ->
            boqItems.Add defaultItem' |> ignore
        | _ -> ()

    interface IStorageApi with
        member _.getBoQItems() =
            boqItems |> List.ofSeq |> Ok

        member _.addBoQItem(boqItem) =
            boqItems.Add boqItem
            Ok ()

        member _.updateBoQItem(boqItem) =
            let index = boqItems.FindIndex( fun x -> x = boqItem)
            boqItems[index] <- boqItem
            Ok ()

        member _.deleteBoQItem(itemId) =
            let index = boqItems.FindIndex( fun x -> x |> BoQItem.value |> fun y -> y.Id = itemId)
            boqItems.RemoveAt(index)
            Ok ()

        member _.loadFactorFTable() =
            FactorFTable [(10,1.1); (100,1.5); (1000, 1.9)]

type FactorFRec = {
    Id: int
    DirectCost: float
    FactorF: float
}
type LiteDBStorage () =

    let database =
        let mapper = FSharpBsonMapper()
        let connStr = $"Filename=CoCoTender.db;mode=Exclusive"
        new LiteDatabase (connStr, mapper)

    let boqItems = database.GetCollection<BoQItem> "boqItems"
    let factorFs = database.GetCollection<FactorFRec> "factorFs"

    do
        if factorFs.Count() = 0 then
            factorFs.Insert { Id = 1; DirectCost = 10.0; FactorF = 1.1 } |> ignore
            factorFs.Insert { Id = 2; DirectCost = 100.0; FactorF = 1.5 } |> ignore
            factorFs.Insert { Id = 3; DirectCost = 1000.0; FactorF = 1.9 } |> ignore


    interface IStorageApi with
        member _.getBoQItems () =
            boqItems.FindAll() |> List.ofSeq |> Ok

        member _.addBoQItem(boqItem) =
            boqItems.Insert boqItem |> ignore
            Ok ()

        member _.updateBoQItem(boqItem) =
            if boqItems.Update boqItem then Ok ()
            else Error "LiteDbStorage:Could not update boq item"

        member _.deleteBoQItem(itemId) =
            if boqItems.Delete itemId then Ok ()
            else Error "LiteDbStorage:Could not delete boq item"

        member _.loadFactorFTable () =
            factorFs.FindAll() |> List.ofSeq |> List.map (fun x -> x.DirectCost,x.FactorF) |> FactorFTable

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

let getAllCost (storage:IStorageApi) () = result {
    let! boqItems = storage.getBoQItems()
    let! (DirectCost directCost, FactorF factorF, EstimateCost estimateCost) =
        Project.tryGetAllCost storage.loadFactorFTable boqItems

    return
        {
            DirectCost = directCost
            FactorF = factorF
            EstimateCost = estimateCost
        }
}

let cocoTenderApi (storage:IStorageApi) =
    {
        getBoQItems = fun () -> asyncResult {
            let! boqItems = storage.getBoQItems()
            let! allCost = getAllCost storage ()

            return (boqItems |> List.map Dto.toBoQItemDto), allCost
        }
        addBoQItem =
            fun boqItemDto -> asyncResult {
                let! boqItem =  boqItemDto |> Dto.toBoQItemDomain
                do! storage.addBoQItem boqItem
                let boqItemDto' = boqItem |> Dto.toBoQItemDto
                let! allCost = getAllCost storage ()

                return boqItemDto', allCost
            }
        updateBoQItem =
            fun boqItemDto -> asyncResult {
                let! boqItem =  boqItemDto |> Dto.toBoQItemDomain
                let! boqItem =  boqItem |> BoQItem.tryUpdate
                do! storage.updateBoQItem boqItem

                let boqItemDto' = boqItem |> Dto.toBoQItemDto
                let! allCost = getAllCost storage ()

                return boqItemDto', allCost
            }
        deleteBoQItem =
            fun itemId -> asyncResult {
                return! storage.deleteBoQItem itemId
            }
        getAllCost = fun () -> asyncResult { return! getAllCost storage () }
        getFactorFInfo = fun () -> asyncResult { return Project.getFactorFInfo storage.loadFactorFTable }
    }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue (cocoTenderApi (LiteDBStorage()))
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