module Types

open DSharpPlus.Entities
open Thoth.Json.Net
open System

type Db =
    { Responses: string list
      Status: (string * ActivityType) option
      Meanness: int
      AutoReplies: Map<uint64, string>
      AutoReplyRates: Map<uint64, decimal>
      LastResponse: string option
      YoutubeChannel: uint64 option
      YoutubeChannels: Set<string>
      LastYoutubeFetch: Map<string, DateTime> }

    static member Decoder =
        Decode.object (fun get ->
            { Responses =
                Decode.list Decode.string
                |> get.Required.Field "Responses"
              Status =
                Decode.tuple2 Decode.string Decode.Enum.int
                |> get.Optional.Field "Status"
              Meanness =
                Decode.int
                |> get.Optional.Field "Meanness"
                |> Option.defaultValue 5
              AutoReplies =
                Decode.list (Decode.tuple2 Decode.string Decode.string)
                |> get.Optional.Field "AutoReplies"
                |> Option.defaultValue []
                |> List.map (fun (k, v) -> uint64 k, v)
                |> Map.ofList
              AutoReplyRates =
                Decode.list (Decode.tuple2 Decode.string Decode.decimal)
                |> get.Optional.Field "AutoReplyRates"
                |> Option.defaultValue []
                |> List.map (fun (k, v) -> uint64 k, v)
                |> Map.ofList
              LastResponse = Decode.string |> get.Optional.Field "LastResponse"
              YoutubeChannel =
                Decode.string
                |> get.Optional.Field "YoutubeChannel"
                |> Option.map uint64
              YoutubeChannels =
                Decode.list Decode.string
                |> get.Optional.Field "YoutubeChannels"
                |> Option.defaultValue []
                |> Set.ofList
              LastYoutubeFetch =
                Decode.dict Decode.datetimeUtc
                |> get.Optional.Field "LastYoutubeFetch"
                |> Option.defaultValue Map.empty })


type Config =
    { DiscordToken: string
      DictKey: string option
      UrbanKey: string option
      CommandPrefix: string
      DbFile: string
      YoutubeKey: string option }

    static member Decoder =
        Decode.object (fun get ->
            { DiscordToken = get.Required.Field "token" Decode.string
              DictKey = get.Optional.Field "dictKey" Decode.string
              UrbanKey = get.Optional.Field "urbanKey" Decode.string
              CommandPrefix =
                get.Optional.Field "prefix" Decode.string
                |> Option.defaultValue "!"
              DbFile =
                get.Optional.Field "dbFile" Decode.string
                |> Option.defaultValue "db.json"
              YoutubeKey = get.Optional.Field "youtubeKey" Decode.string })

type Definition =
    { PartOfSpeech: string option
      Definitions: string [] }

    static member DictDecoder =
        Decode.object (fun get ->
            { PartOfSpeech = get.Optional.Field "fl" Decode.string
              Definitions =
                get.Required.Field "shortdef" (Decode.array Decode.string) })
        |> Decode.array

    static member UrbanDecoder =
        Decode.object (fun get ->
            let defDecoder =
                Decode.object (fun get ->
                    get.Required.Field "definition" Decode.string)

            { PartOfSpeech = None
              Definitions = get.Required.Field "list" (Decode.array defDecoder) })
