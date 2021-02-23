module Types

open Thoth.Json.Net
open DSharpPlus.Entities

type Db =
    { Responses: string list
      Status: (string * ActivityType) option }

type Config =
    { DiscordToken: string
      DictKey: string option
      UrbanKey: string option
      CommandPrefix: string
      DbFile: string }

    static member Decoder =
        Decode.object
            (fun get ->
                { DiscordToken = get.Required.Field "token" Decode.string
                  DictKey = get.Optional.Field "dictKey" Decode.string
                  UrbanKey = get.Optional.Field "urbanKey" Decode.string
                  CommandPrefix =
                      get.Optional.Field "prefix" Decode.string
                      |> Option.defaultValue "!"
                  DbFile =
                      get.Optional.Field "dbFile" Decode.string
                      |> Option.defaultValue "db.json" })
