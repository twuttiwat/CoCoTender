open System.IO
open FSharp.Data
open CoCoTender.Domain
open BoQItem
open Project

[<Literal>]
// let ResolutionFolder = @"c:/projs/CoCoTender/src/scripts/"// __SOURCE_DIRECTORY__ 
let ResolutionFolder =  __SOURCE_DIRECTORY__ 
module BoQFile =
    type BoQItems = CsvProvider<"boq.csv", ResolutionFolder=ResolutionFolder>
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

    let load (filePath:string) =
        BoQItems
            .Load(filePath)
            .Rows
        |> Seq.choose toBoQItem 
        |> List.ofSeq

module FactorFFile =
    type FactorFs = CsvProvider<"FactorF.csv", ResolutionFolder=ResolutionFolder>

    let load (filePath:string) () = 
        FactorFs
            .Load(filePath)
            .Rows
        |> Seq.map (fun x -> x.DirectCost,x.FactorF) 
        |> List.ofSeq 
        |> FactorFTable

[<EntryPoint>]
let main args =
    printfn "%A" args
    match args |> List.ofArray with 
    | boqFile::factorFFile::_ -> 
        let cost = estimateCost (FactorFFile.load factorFFile) (BoQFile.load boqFile) 
        printfn $"Estimate Cost is {cost}"
    | [] -> 
        let boqFile = "boq.csv"
        let factorFFile =  "factorf.csv"
        printfn "%s" boqFile
        printfn "%s" factorFFile
        let cost = estimateCost (FactorFFile.load factorFFile) (BoQFile.load boqFile) 
        printfn $"Estimate Cost is {cost}"
    | _ -> 
        printfn "Should not be here"

    0