module Data

open System
open System.IO
open Thoth.Json.Net
open Types

let config =
    let configPath =
        match Environment.GetCommandLineArgs() with
        | [| _ |] -> "config.json"
        | [| _; configPath |] -> configPath
        | _ -> failwith "too many arguments, expected at most one (config path)"

    match
        File.ReadAllText(configPath)
        |> Decode.fromString Config.Decoder
        with
    | Ok config -> config
    | Error e -> failwith e

let mutable db =
    let defaultDb = { Responses = [ "hi" ]; Status = None }

    try
        match
            File.ReadAllText(config.DbFile)
            |> Decode.fromString Db.Decoder
            with
        | Ok db -> db
        | Error e ->
            printfn $"%s{e}"
            defaultDb
    with
    | e ->
        printfn $"%s{e.Message}"
        defaultDb
