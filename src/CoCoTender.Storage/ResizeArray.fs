module CoCoTender.Storage.ResizeArray

open System

open CoCoTender.Domain
open CoCoTender.Domain.Project

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
