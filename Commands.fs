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

    let updateDb newDb =
        lock db (fun () ->
            db <- newDb
            File.WriteAllText(config.DbFile, Encode.Auto.toString (4, newDb)))

    let updateStatus (ctx: CommandContext) name activityType =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask

            let activity =
                if name = "" then
                    DiscordActivity()
                else
                    DiscordActivity(name, activityType)

            do!
                ctx.Client.UpdateStatusAsync(activity = activity)
                |> Async.AwaitTask

            updateDb
                { db with
                    Status =
                        if name = "" then
                            None
                        else
                            Some(name, activityType) }

            if name = "" then
                do! ctx.RespondChunked("Removed status")
            else
                let activityTypeText =
                    match activityType with
                    | ActivityType.Playing -> "Playing"
                    | ActivityType.Watching -> "Watching"
                    | ActivityType.ListeningTo -> "Listening to"
                    | ActivityType.Streaming -> "Streaming"
                    | t -> failwithf $"not implemented for %s{string t}"

                do! ctx.RespondChunked $"Updated status to **%s{activityTypeText} %s{name}**"
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

    let getUrbanOutput (logger: ILogger) term =
        let urbanResponse =
            getUrbanResponse term
            >>= Decode.fromString Definition.UrbanDecoder

        match urbanResponse with
        | Ok defs when defs.Definitions.Length > 0 -> buildDefineOutput term defs |> Some
        | Ok _ -> None
        | Error e ->
            logger.LogDebug $"%s{e}"
            None

    let getDefineOutput (logger: ILogger) term : string option =
        let response =
            getDictionaryResponse term
            >>= Decode.fromString Definition.DictDecoder

        match response with
        | Ok defs when defs.Length > 0 ->
            map (buildDefineOutput term) defs
            |> intercalate "\n\n"
            |> Some
        | Error e ->
            logger.LogDebug $"%s{e}"
            getUrbanOutput logger term
        | _ -> getUrbanOutput logger term

    let define (ctx: CommandContext) (term: string) =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask

            match getDefineOutput ctx.Client.Logger term with
            | Some output -> do! ctx.RespondChunked(output)
            | None -> do! ctx.RespondChunked("No definition found for **" ++ term ++ "**")
        }

    let urban (ctx: CommandContext) (term: string) =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask

            match getUrbanOutput ctx.Client.Logger term with
            | Some output -> do! ctx.RespondChunked(output)
            | None -> do! ctx.RespondChunked("No definition found for **" ++ term ++ "**")
        }

    let addResponse (ctx: CommandContext) response =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask

            if response = "" then
                do! ctx.RespondChunked("Missing response to add")
            else
                updateDb { db with Responses = response :: db.Responses |> distinct }
                do! ctx.RespondChunked $"Added **%s{response}** to responses"
        }

    let removeResponse (ctx: CommandContext) response =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask

            if response = "" then
                do! ctx.RespondChunked("Missing response to remove")
            elif not (exists ((=) response) db.Responses) then
                do! ctx.RespondChunked $"Response **%s{response}** not found"
            else
                updateDb { db with Responses = db.Responses |> filter ((<>) response) }
                do! ctx.RespondChunked $"Removed **%s{response}** from responses"
        }

    let listResponses (ctx: CommandContext) =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask
            do! ctx.RespondChunked(String.Join("\n", db.Responses))
        }

    let playing (ctx: CommandContext) name =
        async { do! updateStatus ctx name ActivityType.Playing }

    let watching (ctx: CommandContext) name =
        async { do! updateStatus ctx name ActivityType.Watching }

    let listeningTo (ctx: CommandContext) name =
        async { do! updateStatus ctx name ActivityType.ListeningTo }

    let competingIn (ctx: CommandContext) name =
        async { do! updateStatus ctx name ActivityType.Competing }

    let setMeanness (ctx: CommandContext) (meanness: int) =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask
            let meanness = meanness |> max 0 |> min 10
            updateDb { db with Meanness = meanness }
            do! ctx.RespondChunked $"Set meanness to **%d{meanness}**"
        }

    [<Command("rr"); Description("Play Russian Roulette!")>]
    member _.RussianRouletteAsync(ctx) =
        russianRoulette ctx |> Async.StartAsTask :> Task

    [<Command("define");
      Description("Look up the definition of a word or phrase, using Urban Dictionary as a backup.")>]
    member _.DefineAsync(ctx, [<Description("The word or phrase to look up."); RemainingText>] term) =
        define ctx term |> Async.StartAsTask :> Task

    [<Command("urban"); Description("Look up the definition of a word or phrase on Urban Dictionary.")>]
    member _.UrbanAsync(ctx, [<Description("The word or phrase to look up."); RemainingText>] term) =
        urban ctx term |> Async.StartAsTask :> Task

    [<Command("add"); Description("Add a response to be randomly selected when the bot replies after being pinged.")>]
    member _.AddResponseAsync(ctx, [<Description("The response to add."); RemainingText>] response) =
        addResponse ctx response |> Async.StartAsTask :> Task

    [<Command("remove"); Description("Remove a response from the bot's response pool.")>]
    member _.RemoveResponseAsync(ctx, [<Description("The response to remove."); RemainingText>] response) =
        removeResponse ctx response |> Async.StartAsTask :> Task

    [<Command("list"); Description("List all responses in the response pool.")>]
    member _.ListResponsesAsync(ctx) =
        listResponses ctx |> Async.StartAsTask :> Task

    [<Command("playing"); Description("Set bot's activity to Playing.")>]
    member _.PlayingAsync(ctx, [<Description("What's the bot playing?"); RemainingText>] name) =
        playing ctx name |> Async.StartAsTask :> Task

    [<Command("watching"); Description("Set bot's activity to Watching.")>]
    member _.WatchingAsync(ctx, [<Description("What's the bot watching?"); RemainingText>] name) =
        watching ctx name |> Async.StartAsTask :> Task

    [<Command("listeningto"); Description("Set bot's activity to Listening To.")>]
    member _.ListeningToAsync(ctx, [<Description("What's the bot listening to?"); RemainingText>] name) =
        listeningTo ctx name |> Async.StartAsTask :> Task

    [<Command("competingin"); Description("Set bot's activity to Competing In.")>]
    member _.CompetingInAsync(ctx, [<Description("What's the bot competing in?"); RemainingText>] name) =
        competingIn ctx name |> Async.StartAsTask :> Task

    [<Command("playing")>]
    member _.PlayingAsync(ctx) =
        playing ctx "" |> Async.StartAsTask :> Task

    [<Command("watching")>]
    member _.WatchingAsync(ctx) =
        watching ctx "" |> Async.StartAsTask :> Task

    [<Command("listeningto")>]
    member _.ListeningToAsync(ctx) =
        listeningTo ctx "" |> Async.StartAsTask :> Task

    [<Command("competingin")>]
    member _.CompetingInAsync(ctx) =
        competingIn ctx "" |> Async.StartAsTask :> Task

    [<Command("meanness"); Description("Set bot's meanness level from 0 to 10.")>]
    member _.MeannessAsync
        (
            ctx,
            [<Description("The number between 0 and 10 to set the bot's meanness to. Higher is meaner.")>] meanness
        ) =
        setMeanness ctx meanness |> Async.StartAsTask :> Task
