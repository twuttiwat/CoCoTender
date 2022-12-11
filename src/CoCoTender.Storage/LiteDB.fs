module CoCoTender.Storage.LiteDB

open System
open LiteDB
open LiteDB.FSharp

open CoCoTender.Domain
open CoCoTender.Domain.Project

type  FactorFRec = {
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
