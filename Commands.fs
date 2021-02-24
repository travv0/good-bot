namespace GoodBot

open DSharpPlus.CommandsNext
open DSharpPlus.CommandsNext.Attributes
open DSharpPlus.Entities
open Data
open Extensions
open FSharpPlus
open FsHttp
open FsHttp.DslCE
open Microsoft.Extensions.Logging
open System
open System.IO
open System.Threading.Tasks
open Thoth.Json.Net
open Types

type Commands() =
    inherit BaseCommandModule()

    let updateStatus (ctx: CommandContext) name activityType =
        async {
            let activity =
                if name = "" then
                    DiscordActivity()
                else
                    DiscordActivity(name, activityType)

            do!
                ctx.Client.UpdateStatusAsync(activity = activity)
                |> Async.AwaitTask

            lock
                db
                (fun () ->
                    db <-
                        { db with
                              Status =
                                  if name = "" then
                                      None
                                  else
                                      Some(name, activityType) }

                    File.WriteAllText(config.DbFile, Encode.Auto.toString (4, db)))

            if name = "" then
                do! ctx.RespondChunked("Removed status")
            else
                let activityTypeText =
                    match activityType with
                    | ActivityType.Playing -> "Playing"
                    | ActivityType.Watching -> "Watching"
                    | ActivityType.ListeningTo -> "Listening to"
                    | ActivityType.Streaming -> "Streaming"
                    | t -> failwithf "not implemented for %s" (t.ToString())

                do! ctx.RespondChunked(sprintf "Updated status to **%s %s**" activityTypeText name)
        }

    let russianRoulette (ctx: CommandContext) =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask
            let rand = Random()

            if rand.Next(6) = 0 then
                do! ctx.RespondChunked("Bang!")

                do!
                    ctx.Guild.BanMemberAsync(ctx.Member, 0, "Bang!")
                    |> Async.AwaitTask
            else
                do! ctx.RespondChunked("Click.")
        }

    let buildDefineOutput term (definition: Definition) =
        let definitions =
            match definition.Definitions with
            | [| def |] -> def
            | defs ->
                mapi (fun i def -> (i + 1).ToString() ++ ". " ++ def) defs
                |> intercalate "\n\n"

        "**"
        ++ term
        ++ "**"
        ++ (match definition.PartOfSpeech with
            | Some partOfSpeech -> " *" ++ partOfSpeech ++ "*"
            | None -> "")
        ++ "\n"
        ++ definitions

    let getDictionaryResponse term =
        match config.DictKey with
        | None -> Error "no dictionary.com api key set"
        | Some apiKey ->
            http {
                GET(
                    "https://dictionaryapi.com/api/v3/references/collegiate/json/"
                    ++ term
                    ++ "?key="
                    ++ apiKey
                )
            }
            |> Response.toText
            |> Ok

    let getUrbanResponse term =
        match config.UrbanKey with
        | None -> Error "no urban dictionary api key set"
        | Some apiKey ->
            http {
                GET(
                    "https://mashape-community-urban-dictionary.p.rapidapi.com/define?term="
                    ++ term
                )

                Header "x-rapidapi-key" apiKey
                Header "x-rapidapi-host" "mashape-community-urban-dictionary.p.rapidapi.com"
                Header "useQueryString" "true"
            }
            |> Response.toText
            |> Ok

    let getDefineOutput (logger: ILogger) term: string option =
        let response =
            getDictionaryResponse term
            >>= Decode.fromString (Decode.array Definition.Decoder)

        let getUrbanOutput () =
            let urbanResponse =
                getUrbanResponse term
                >>= Decode.fromString Definition.UrbanDecoder

            match urbanResponse with
            | Ok defs when defs.Definitions.Length > 0 -> buildDefineOutput term defs |> Some
            | Ok _ -> None
            | Error e ->
                logger.LogDebug(sprintf "%s" e)
                None

        match response with
        | Ok defs when defs.Length > 0 ->
            map (buildDefineOutput term) defs
            |> intercalate "\n\n"
            |> Some
        | Error e ->
            logger.LogDebug(sprintf "%s" e)
            getUrbanOutput ()
        | _ -> getUrbanOutput ()

    let define (ctx: CommandContext) (term: string) =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask

            match getDefineOutput ctx.Client.Logger term with
            | Some output -> do! ctx.RespondChunked(output)
            | None -> do! ctx.RespondChunked("No definition found for **" ++ term ++ "**")
        }

    let addResponse (ctx: CommandContext) response =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask

            if response = "" then
                do! ctx.RespondChunked("Missing response to add")
            else
                lock
                    db
                    (fun () ->
                        db <-
                            { db with
                                  Responses = response :: db.Responses |> distinct }

                        File.WriteAllText(config.DbFile, Encode.Auto.toString (4, db)))

                do! ctx.RespondChunked(sprintf "Added **%s** to responses" response)
        }

    let removeResponse (ctx: CommandContext) response =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask

            if response = "" then
                do! ctx.RespondChunked("Missing response to remove")
            elif not (exists ((=) response) db.Responses) then
                do! ctx.RespondChunked(sprintf "Response **%s** not found" response)
            else
                lock
                    db
                    (fun () ->
                        db <-
                            { db with
                                  Responses = db.Responses |> filter ((<>) response) }

                        File.WriteAllText(config.DbFile, Encode.Auto.toString (4, db)))

                do! ctx.RespondChunked(sprintf "Removed **%s** from responses" response)
        }

    let listResponses (ctx: CommandContext) =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask
            do! ctx.RespondChunked(String.Join("\n", db.Responses))
        }

    let playing (ctx: CommandContext) name =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask
            do! updateStatus ctx name ActivityType.Playing
        }

    let watching (ctx: CommandContext) name =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask
            do! updateStatus ctx name ActivityType.Watching
        }

    let listeningTo (ctx: CommandContext) name =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask
            do! updateStatus ctx name ActivityType.ListeningTo
        }

    [<Command("rr")>]
    member __.RussianRouletteAsync(ctx) =
        russianRoulette ctx |> Async.StartAsTask :> Task

    [<Command("define")>]
    member __.DefineAsync(ctx, term) =
        define ctx term |> Async.StartAsTask :> Task

    [<Command("add")>]
    member __.AddResponseAsync(ctx, response) =
        addResponse ctx response |> Async.StartAsTask :> Task

    [<Command("remove")>]
    member __.RemoveResponseAsync(ctx, response) =
        removeResponse ctx response |> Async.StartAsTask :> Task

    [<Command("list")>]
    member __.ListResponsesAsync(ctx) =
        listResponses ctx |> Async.StartAsTask :> Task

    [<Command("playing")>]
    member __.PlayingAsync(ctx, name) =
        playing ctx name |> Async.StartAsTask :> Task

    [<Command("watching")>]
    member __.WatchingAsync(ctx, name) =
        watching ctx name |> Async.StartAsTask :> Task

    [<Command("listeningto")>]
    member __.ListeningToAsync(ctx, name) =
        listeningTo ctx name |> Async.StartAsTask :> Task

    [<Command("playing")>]
    member __.PlayingAsync(ctx) =
        playing ctx "" |> Async.StartAsTask :> Task

    [<Command("watching")>]
    member __.WatchingAsync(ctx) =
        watching ctx "" |> Async.StartAsTask :> Task

    [<Command("listeningto")>]
    member __.ListeningToAsync(ctx) =
        listeningTo ctx "" |> Async.StartAsTask :> Task
