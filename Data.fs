module Data

open System.IO
open Thoth.Json.Net
open Types

let config =
    match File.ReadAllText("config.json")
          |> Decode.fromString Config.Decoder with
    | Ok config -> config
    | Error e -> failwith e

let mutable db =
    let defaultDb = { Responses = [ "hi" ]; Status = None }

    try
        match File.ReadAllText(config.DbFile)
              |> Decode.Auto.fromString with
        | Ok db -> db
        | Error e ->
            printfn "%s" e
            defaultDb
    with e ->
        printfn "%s" e.Message
        defaultDb
