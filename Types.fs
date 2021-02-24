module Types

open DSharpPlus.Entities
open FSharpPlus
open Thoth.Json.Net

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

type Definition =
    { PartOfSpeech: string option
      Definitions: string [] }

    static member Decoder =
        Decode.object
            (fun get ->
                { PartOfSpeech = get.Optional.Field "fl" Decode.string
                  Definitions = get.Required.Field "shortdef" (Decode.array Decode.string) })

    static member UrbanDecoder =
        Decode.object
            (fun get ->
                let defDecoder =
                    Decode.object (fun get -> get.Required.Field "definition" Decode.string)

                { PartOfSpeech = None
                  Definitions = get.Required.Field "list" (Decode.array defDecoder) })
