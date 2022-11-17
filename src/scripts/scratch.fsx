#r "nuget: FSharp.Data, 5.0.2"
#r @"C:\projs\CoCoTender\src\CoCoTender.Domain\bin\Debug\net6.0\CoCoTender.Domain.dll"

open FSharp.Data
open CoCoTender.Domain
open BoQItem
open Project

[<Literal>]
let ResolutionFolder = __SOURCE_DIRECTORY__

type BoQItems = CsvProvider<"boq.csv", ResolutionFolder=ResolutionFolder>
let testBoQ = BoQItems.Load(__SOURCE_DIRECTORY__ + "/boq.csv")

let q = Quantity (1.0, "u")
let u = { Name = "n"; Unit = "u"; UnitCost = 1.0}
let m = Material u
let l = Labor u

BoQItem.tryCreate "a" q m l

let (firstRow:BoQItems.Row) = testBoQ.Rows |> Seq.head

let toBoQItem (row:BoQItems.Row) =
  let qty = Quantity (row.Quantity, row.Unit.Trim())
  let material = 
    {
        Name = row.Material
        Unit = row.MaterialUnit.Trim()
        UnitCost = row.MaterialUnitCost
    }
    |> Material

  let labor = 
    {
      Name = row.Labor
      Unit = row.LaborUnit.Trim()
      UnitCost = row.LaborUnitCost
    }
    |> Labor

  BoQItem.tryCreate row.Description qty material labor

let directCost =
  testBoQ.Rows 
  |> Seq.choose toBoQItem 
  |> Seq.sumBy (fun x -> x |> value |> fun y -> y.TotalCost)
  |> DirectCost

open Project

type FactorFFile = CsvProvider<"FactorF.csv", ResolutionFolder=ResolutionFolder>
let testFactorF = FactorFFile.Load(__SOURCE_DIRECTORY__ + "/FactorF.csv")
let fRows = testFactorF.Rows
let fTable = fRows |> Seq.map (fun x -> x.DirectCost,x.FactorF) |> List.ofSeq |> FactorFTable

let loadBoQFile (filePath:string) =
  BoQItems
    .Load(__SOURCE_DIRECTORY__ + "/boq.csv")
    .Rows
  |> Seq.choose toBoQItem 
  |> List.ofSeq

let loadFactorFFile (filePath:string) () = 
  FactorFFile
    .Load(filePath)
    .Rows
  |> Seq.map (fun x -> x.DirectCost,x.FactorF) 
  |> List.ofSeq 
  |> FactorFTable

let boqFile = __SOURCE_DIRECTORY__ + "/boq.csv"
let factorFFile = __SOURCE_DIRECTORY__ + "/FactorF.csv"
let cost = estimateCost (loadFactorFFile factorFFile) (loadBoQFile boqFile) 

printfn "%A" fsi.CommandLineArgs
match fsi.CommandLineArgs |> List.ofArray with 
| _::boqFile::factorFFile::_ -> 
  let cost = estimateCost (loadFactorFFile factorFFile) (loadBoQFile boqFile) 
  printfn $"Estimate Cost is {cost}"
| _::_ -> 
  let boqFile = __SOURCE_DIRECTORY__ + @"\boq.csv"
  let factorFFile = __SOURCE_DIRECTORY__ + @"\FactorF.csv"
  printfn "%s" boqFile
  printfn "%s" factorFFile
  let cost = estimateCost (loadFactorFFile factorFFile) (loadBoQFile boqFile) 
  printfn $"Estimate Cost is {cost}"
| _ -> 
  printfn "Should not be here"

open System.IO
Directory.GetCurrentDirectory()