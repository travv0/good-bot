namespace GoodBot

open DSharpPlus.CommandsNext
open DSharpPlus.CommandsNext.Attributes
open DSharpPlus.Entities
open Data
open Extensions
open System
open System.IO
open System.Threading.Tasks
open Thoth.Json.Net

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

            db <-
                { db with
                      Status =
                          if name = "" then
                              None
                          else
                              Some(name, activityType) }

            File.WriteAllText(config.DbFile, Encode.Auto.toString (4, db))

            if name = "" then
                do! ctx.Respond("Removed status")
            else
                let activityTypeText =
                    match activityType with
                    | ActivityType.Playing -> "Playing"
                    | ActivityType.Watching -> "Watching"
                    | ActivityType.ListeningTo -> "Listening to"
                    | ActivityType.Streaming -> "Streaming"
                    | t -> failwithf "not implemented for %s" (t.ToString())

                do! ctx.Respond(sprintf "Updated status to **%s %s**" activityTypeText name)
        }

    let russianRoulette (ctx: CommandContext) =
        async {
            do! ctx.TriggerTypingAsync() |> Async.AwaitTask
            let rand = Random()

            if rand.Next(6) = 0 then
                do! ctx.Respond("Bang!")

                do!
                    ctx.Guild.BanMemberAsync(ctx.Member, 0, "Bang!")
                    |> Async.AwaitTask
            else
                do! ctx.Respond("Click.")
        }

    let addResponse (ctx: CommandContext) response =
        async {
            if response = "" then
                do! ctx.Respond("Missing response to add")
            else
                db <-
                    { db with
                          Responses = response :: db.Responses |> List.distinct }

                File.WriteAllText(config.DbFile, Encode.Auto.toString (4, db))
                do! ctx.Respond(sprintf "Added **%s** to responses" response)
        }

    let removeResponse (ctx: CommandContext) response =
        async {
            if response = "" then
                do! ctx.Respond("Missing response to remove")
            elif not (List.contains response db.Responses) then
                do! ctx.Respond(sprintf "Response **%s** not found" response)
            else
                db <-
                    { db with
                          Responses = db.Responses |> List.filter ((<>) response) }

                File.WriteAllText(config.DbFile, Encode.Auto.toString (4, db))
                do! ctx.Respond(sprintf "Removed **%s** from responses" response)
        }

    let listResponses (ctx: CommandContext) =
        async { do! ctx.Respond(String.Join("\n", db.Responses)) }

    let playing (ctx: CommandContext) name =
        async { do! updateStatus ctx name ActivityType.Playing }

    let watching (ctx: CommandContext) name =
        async { do! updateStatus ctx name ActivityType.Watching }

    let listeningTo (ctx: CommandContext) name =
        async { do! updateStatus ctx name ActivityType.ListeningTo }

    [<Command("rr")>]
    member __.RussianRouletteAsync ctx =
        russianRoulette ctx |> Async.StartAsTask :> Task

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
