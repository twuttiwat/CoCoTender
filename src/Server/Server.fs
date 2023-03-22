module Server

open FsToolkit.ErrorHandling
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn
open Giraffe
open System
open System.Security.Claims
open System.IdentityModel.Tokens.Jwt
open Microsoft.IdentityModel.Tokens
open Microsoft.AspNetCore.Http
open Saturn

open System

open Shared
open CoCoTender.Domain
open CoCoTender.Domain.Project
open CoCoTender.Storage
open CoCoTender.Storage.LiteDB

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
            printfn "Server boqitems %A" (boqItems |> List.length)
            let! allCost = getAllCost storage ()
            printfn "Server allCost %A" allCost

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

let secret = "my-top-secret-no-one-knows"
let issuer = "my-domain-issuer.com"

let generateToken email =
    let claims = [|
        Claim(JwtRegisteredClaimNames.Sub, email);
        Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) |]

    claims
    |> Auth.generateJWT (secret, SecurityAlgorithms.HmacSha256) issuer (DateTime.UtcNow.AddHours(1.0))
    |> Token
    
let authApi = {
    login =
        fun (email, password) ->
            async {
                if (password = "ok") then
                    return (email |> generateToken |> Ok)
                else 
                    return (Error "Login Failed")
            }
}

// let cocoTenderApi' context = cocoTenderApi context (LiteDBStorage())

let anonymousApi : HttpHandler =
    Remoting.createApi ()
    |> Remoting.fromValue authApi
    |> Remoting.buildHttpHandler

let securedApi  =
    Remoting.createApi ()
    |> Remoting.fromValue (cocoTenderApi (LiteDBStorage()))
    |> Remoting.buildHttpHandler
      
let completeApi : HttpHandler = choose [
    anonymousApi
    pipeline {
        requires_authentication (Giraffe.Auth.challenge "JWT")
        plug securedApi
    }
]
    
let topRouter = router {
    get "/" (htmlFile "public/app.html")
    forward "/api" completeApi
}


let app =
    application {
        use_jwt_authentication secret issuer
        use_router topRouter 
        memory_cache
        use_static "public"
        use_gzip
    }

[<EntryPoint>]
let main _ =
    run app
    0