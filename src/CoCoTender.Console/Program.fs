open System.IO
open FSharp.Data
open FsToolkit.ErrorHandling
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
        |> Seq.map toBoQItem 
        |> List.ofSeq
        |> List.sequenceResultA
        |> function
            | Ok items -> items
            | Error msgs -> failwith $"{msgs}"

module FactorFFile =
    type FactorFs = CsvProvider<"FactorF.csv", ResolutionFolder=ResolutionFolder>

    let load (filePath:string) () = 
        FactorFs
            .Load(filePath)
            .Rows
        |> Seq.map (fun x -> x.DirectCost,x.FactorF) 
        |> List.ofSeq 
        |> FactorFTable

let printCost cost =
    cost 
    |> Result.map (fun cost' -> printfn $"Estimate Cost is {cost'}") 
    |> ignore

[<EntryPoint>]
let main args =
    printfn "%A" args
    match args |> List.ofArray with 
    | boqFile::factorFFile::_ -> 
        let cost = tryEstimateCost (FactorFFile.load factorFFile) (BoQFile.load boqFile) 
        printCost cost 
    | [] -> 
        let boqFile = "boq.csv"
        let factorFFile =  "factorf.csv"
        let cost = tryEstimateCost (FactorFFile.load factorFFile) (BoQFile.load boqFile)         
        printCost cost
    | _ -> 
        printfn "Should not be here"

    0