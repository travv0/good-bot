namespace GoodBot

open DSharpPlus.CommandsNext
open DSharpPlus.CommandsNext.Attributes
open DSharpPlus.Entities
open Data
open Extensions
open Fake.Core
open FsHttp
open FsHttp.DslCE
open Microsoft.Extensions.Logging
open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Thoth.Json.Net
open Types
open Util

type Commands() =
    inherit BaseCommandModule()

    let trimOutput n s =
        if String.length s > n then
            s.Substring(0, n - 3) + "..."
        else
            s

    let updateStatus (ctx: CommandContext) name activityType =
        task {
            do! ctx.TriggerTypingAsync()

            let activity =
                if String.IsNullOrWhiteSpace(name) then
                    DiscordActivity()
                else
                    DiscordActivity(name, activityType)

            do! ctx.Client.UpdateStatusAsync(activity = activity)

            updateDb
                { db with
                    Status =
                        if String.IsNullOrWhiteSpace(name) then
                            None
                        else
                            Some(name, activityType) }

            if String.IsNullOrWhiteSpace(name) then
                ctx.RespondChunked("Removed status")
            else
                let activityTypeText =
                    match activityType with
                    | ActivityType.Playing -> "Playing"
                    | ActivityType.Watching -> "Watching"
                    | ActivityType.ListeningTo -> "Listening to"
                    | ActivityType.Streaming -> "Streaming"
                    | ActivityType.Competing -> "Competing in"
                    | t -> failwithf $"not implemented for %s{string t}"

                ctx.RespondChunked(
                    $"Updated status to **%s{activityTypeText} %s{name}**"
                )
        }

    let addAutoReply (ctx: CommandContext) (user: DiscordUser) reply =
        updateDb
            { db with AutoReplies = db.AutoReplies |> Map.add user.Id reply }

        ctx.RespondChunked(
            $"Will now reply to **%s{user.Username}** with \"%s{reply}\""
        )

    let removeAutoReply (ctx: CommandContext) (user: DiscordUser) =
        updateDb { db with AutoReplies = db.AutoReplies |> Map.remove user.Id }
        ctx.RespondChunked($"Removed auto-reply for user **%s{user.Username}**")

    let buildDefineOutput term (definition: Definition) =
        let definitions =
            match definition.Definitions with
            | [| def |] -> def
            | defs ->
                Array.mapi (fun i def -> (i + 1).ToString() + ". " + def) defs
                |> String.join "\n\n"

        "**"
        + term
        + "**"
        + (match definition.PartOfSpeech with
           | Some partOfSpeech -> " *" + partOfSpeech + "*"
           | None -> "")
        + "\n"
        + definitions

    let getDictionaryResponse term =
        match config.DictKey with
        | None -> Error "no dictionary.com api key set"
        | Some apiKey ->
            http {
                GET(
                    "https://dictionaryapi.com/api/v3/references/collegiate/json/"
                    + term
                    + "?key="
                    + apiKey
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
                    + term
                )

                Header "x-rapidapi-key" apiKey

                Header
                    "x-rapidapi-host"
                    "mashape-community-urban-dictionary.p.rapidapi.com"

                Header "useQueryString" "true"
            }
            |> Response.toText
            |> Ok

    let getUrbanOutput (logger: ILogger) term =
        let urbanResponse =
            getUrbanResponse term
            |> Result.bind (Decode.fromString Definition.UrbanDecoder)

        match urbanResponse with
        | Ok defs when defs.Definitions.Length > 0 ->
            buildDefineOutput term defs |> Some
        | Ok _ -> None
        | Error e ->
            logger.LogDebug $"%s{e}"
            None

    let getDefineOutput (logger: ILogger) term : string option =
        let response =
            getDictionaryResponse term
            |> Result.bind (Decode.fromString Definition.DictDecoder)

        match response with
        | Ok defs when defs.Length > 0 ->
            Array.map (buildDefineOutput term) defs
            |> String.join "\n\n"
            |> Some
        | Error e ->
            logger.LogDebug $"%s{e}"
            getUrbanOutput logger term
        | _ -> getUrbanOutput logger term

    let define (ctx: CommandContext) getOutput (term: string) =
        task {
            do! ctx.TriggerTypingAsync()

            if String.IsNullOrWhiteSpace(term) then
                ctx.RespondChunked("Missing term to define")
            else
                match getOutput ctx.Client.Logger term with
                | Some output -> ctx.RespondChunked(output)
                | None ->
                    ctx.RespondChunked(
                        "No definition found for **" + term + "**"
                    )
        }

    let lox (ctx: CommandContext) dumpAst program =
        let loxOutput
            { Result = { Output = ok; Error = error }
              ExitCode = exitCode }
            =
            let trimmedError = trimOutput 1992 error

            if not (String.IsNullOrWhiteSpace trimmedError) then
                ctx.RespondChunked($"```\n%s{trimmedError}\n```")

            elif exitCode = 0 then
                let trimmedOk = trimOutput 1992 ok

                if String.IsNullOrWhiteSpace(trimmedOk) then
                    ctx.RespondChunked($"```\n<No output>\n```")
                else
                    ctx.RespondChunked($"```\n%s{trimmedOk}\n```")

            else
                ctx.RespondChunked($"```\n<Unknown error>\n```")

        task {
            do! ctx.TriggerTypingAsync()

            let code =
                parseCodeBlockFromMessage program
                |> Result.ok program

            let path = Path.GetTempFileName()
            File.WriteAllText(path, code)

            Arguments.Empty
            |> Arguments.appendIf dumpAst "--ast"
            |> Arguments.append [ path ]
            |> Arguments.toList
            |> CreateProcess.fromRawCommand "flox"
            |> CreateProcess.redirectOutput
            |> CreateProcess.addOnStartedEx (fun p ->
                Thread.Sleep(TimeSpan.FromSeconds(3))

                if not p.Process.HasExited then
                    p.Process.Kill()
                    ctx.RespondChunked("```\n<Timeout>\n```"))
            |> Proc.run
            |> loxOutput
        }

    [<Command("rr"); Description("Play Russian Roulette!")>]
    member _.RussianRouletteAsync(ctx: CommandContext) : Task =
        task {
            do! ctx.TriggerTypingAsync()
            let rand = Random()

            if rand.Next(6) = 0 then
                ctx.RespondChunked("Bang!")
                do! ctx.Guild.BanMemberAsync(ctx.Member, 0, "Bang!")
            else
                ctx.RespondChunked("Click.")
        }

    [<Command("define");
      Description("Look up the definition of a word or phrase, using Urban Dictionary as a backup.")>]
    member _.DefineAsync
        (
            ctx,
            [<Description("The word or phrase to look up."); RemainingText>] term
        ) : Task =
        define ctx getDefineOutput term

    [<Command("urban");
      Description("Look up the definition of a word or phrase on Urban Dictionary.")>]
    member _.UrbanAsync
        (
            ctx,
            [<Description("The word or phrase to look up."); RemainingText>] term
        ) : Task =
        define ctx getUrbanOutput term

    [<Command("add");
      Description("Add a response to be randomly selected when the bot replies after being pinged.")>]
    member _.AddResponseAsync
        (
            ctx: CommandContext,
            [<Description("The response to add."); RemainingText>] response
        ) : Task =
        task {
            do! ctx.TriggerTypingAsync()

            if String.IsNullOrWhiteSpace(response) then
                ctx.RespondChunked("Missing response to add")
            else
                updateDb
                    { db with
                        Responses = response :: db.Responses |> List.distinct }

                ctx.RespondChunked($"Added **%s{response}** to responses")
        }

    [<Command("remove");
      Description("Remove a response from the bot's response pool.")>]
    member _.RemoveResponseAsync
        (
            ctx: CommandContext,
            [<Description("The response to remove."); RemainingText>] response
        ) : Task =
        task {
            do! ctx.TriggerTypingAsync()

            if String.IsNullOrWhiteSpace(response) then
                ctx.RespondChunked("Missing response to remove")
            elif not (List.contains response db.Responses) then
                ctx.RespondChunked($"Response **%s{response}** not found")
            else
                updateDb
                    { db with
                        Responses = db.Responses |> List.filter ((<>) response) }

                ctx.RespondChunked($"Removed **%s{response}** from responses")
        }

    [<Command("removelast");
      Description("Remove the bot's last response from the response pool.")>]
    member _.RemoveLastResponseAsync(ctx: CommandContext) : Task =
        task {
            do! ctx.TriggerTypingAsync()

            match db.LastResponse with
            | None -> ctx.RespondChunked("No response to remove")
            | Some response ->
                updateDb
                    { db with
                        Responses = db.Responses |> List.filter ((<>) response) }

                ctx.RespondChunked($"Removed **%s{response}** from responses")
        }

    [<Command("list"); Description("List all responses in the response pool.")>]
    member _.ListResponsesAsync(ctx: CommandContext) : Task =
        task {
            do! ctx.TriggerTypingAsync()
            ctx.RespondChunked(String.join "\n" db.Responses)
        }

    [<Command("playing"); Description("Set bot's activity to Playing.")>]
    member _.PlayingAsync
        (
            ctx,
            [<Description("What's the bot playing?"); RemainingText>] name
        ) : Task =
        updateStatus ctx name ActivityType.Playing

    [<Command("watching"); Description("Set bot's activity to Watching.")>]
    member _.WatchingAsync
        (
            ctx,
            [<Description("What's the bot watching?"); RemainingText>] name
        ) : Task =
        updateStatus ctx name ActivityType.Watching

    [<Command("listeningto");
      Description("Set bot's activity to Listening To.")>]
    member _.ListeningToAsync
        (
            ctx,
            [<Description("What's the bot listening to?"); RemainingText>] name
        ) : Task =
        updateStatus ctx name ActivityType.ListeningTo

    [<Command("competingin");
      Description("Set bot's activity to Competing In.")>]
    member _.CompetingInAsync
        (
            ctx,
            [<Description("What's the bot competing in?"); RemainingText>] name
        ) : Task =
        updateStatus ctx name ActivityType.Competing

    [<Command("addautoreply");
      Description("Set a message for the bot to automatically reply with whenever a user sends a message.")>]
    member _.AddAutoReply
        (
            ctx: CommandContext,
            [<Description("The user to reply to.")>] user: DiscordUser,
            reply
        ) : Task =
        task {
            do! ctx.TriggerTypingAsync()
            addAutoReply ctx user reply
        }

    [<Command("removeautoreply");
      Description("Remove auto-reply for given user.")>]
    member _.RemoveAutoReply
        (
            ctx: CommandContext,
            [<Description("The user to remove reply for.")>] user: DiscordUser
        ) : Task =
        task {
            do! ctx.TriggerTypingAsync()
            removeAutoReply ctx user
        }

    [<Command("meanness");
      Description("Set bot's meanness level from 0 to 10 or view current meanness.")>]
    member _.MeannessAsync
        (
            ctx: CommandContext,
            [<Description("The number between 0 and 10 to set the bot's meanness to. Higher is meaner.")>] level
        ) : Task =
        task {
            do! ctx.TriggerTypingAsync()
            let meanness = level |> max 0 |> min 11
            updateDb { db with Meanness = meanness }
            ctx.RespondChunked($"Set meanness to **%d{meanness}**")
        }

    [<Command("meanness")>]
    member _.MeannessAsync(ctx: CommandContext) : Task =
        task {
            do! ctx.TriggerTypingAsync()
            ctx.RespondChunked($"Current meanness is **%d{db.Meanness}**")
        }

    [<Command("calc"); Description("Calculate the result of an expression.")>]
    member _.CalcAsync
        (
            ctx: CommandContext,
            [<RemainingText; Description("The expression to evaluate.")>] expr
        ) : Task =
        task {
            do! ctx.TriggerTypingAsync()

            match Calculator.eval expr with
            | Ok f -> ctx.RespondChunked(sprintf $"%.16g{f}")
            | Error e -> ctx.RespondChunked($"```\n%s{e}\n```")
        }

    [<Command("lox"); Description("View the result of a Lox program.")>]
    member _.LoxAsync
        (
            ctx: CommandContext,
            [<RemainingText; Description("The lox program to run.")>] program
        ) : Task =
        lox ctx false program

    [<Command("loxast"); Description("View the syntax tree of a Lox program.")>]
    member _.LoxAstAsync
        (
            ctx: CommandContext,
            [<RemainingText; Description("The lox program to lex and parse.")>] program
        ) : Task =
        lox ctx true program
