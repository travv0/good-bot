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

    match File.ReadAllText(configPath) |> Decode.fromString Config.Decoder with
    | Ok config -> config
    | Error e -> failwith e

let mutable private db =
    let defaultDb =
        { Responses = [ "hi" ]
          Status = None
          Meanness = 5
          AutoReplies = Map.empty
          AutoReplyRates = Map.empty
          LastResponse = None
          YoutubeChannel = None
          YoutubeChannels = Set.empty
          LastYoutubeFetch = Map.empty }

    try
        match
            File.ReadAllText(config.DbFile) |> Decode.fromString Db.Decoder
        with
        | Ok db -> db
        | Error e ->
            printfn $"%s{e}"
            defaultDb
    with e ->
        printfn $"%s{e.Message}"
        defaultDb

let updateDb newDb =
    lock db (fun () ->
        db <- newDb

        File.WriteAllText(
            config.DbFile,
            Encode.Auto.toString (
                4,
                newDb,
                extra = (Extra.empty |> Extra.withUInt64 |> Extra.withDecimal)
            )
        ))

let getDb () = lock db (fun () -> db)
