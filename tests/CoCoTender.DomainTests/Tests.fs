module Tests

open System
open Xunit
open FsUnit
open CoCoTender
open CoCoTender.Domain

[<Fact>]
let ``Hello test`` () =
    Say.hello "world" |> should equal "Hello, world"
