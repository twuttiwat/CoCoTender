#r "nuget:LiteDB.FSharp"
#r "nuget:FsToolkit.ErrorHandling"
#r @"C:\projs\CoCoTender\src\CoCoTender.Domain\bin\Debug\net6.0\CoCoTender.Domain.dll"

open LiteDB.FSharp
open LiteDB
open LiteDB.FSharp.Extensions
open CoCoTender.Domain

let database =
      let mapper = FSharpBsonMapper()
      let connStr = $"Filename={__SOURCE_DIRECTORY__}\\CoCoTender.db;mode=Exclusive"
      new LiteDatabase (connStr, mapper)

let boqitems = database.GetCollection<BoQItem> "boqitems"

open System

let itemId = Guid.NewGuid()
let desc = "Pool Item"
let qty = Quantity (10.0, "m^2")
let material = Material { Name = "Big Tile"; UnitCost = 100.0; Unit = "m^2" }
let labor = Labor { Name = "Do Tiling"; UnitCost = 50.0; Unit = "m^2" }
let boqItem:BoQItem =
      match BoQItem.tryCreate itemId desc qty material labor with
      | Ok value -> value
      | Error e -> failwith e


boqitems.Insert boqItem
boqitems.FindAll () |> List.ofSeq

//boqitems.FindById()

// OR
let id = BsonValue(Guid("92975f65-52fe-4089-b11e-524eb141577f"))
// result : Album
let result = boqitems.FindById(id)


let result' = { result with Description = "Foo"}
boqitems.Update(result')

boqitems.Delete(id)
