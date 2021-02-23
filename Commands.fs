namespace GoodBot

open DSharpPlus.CommandsNext
open DSharpPlus.CommandsNext.Attributes
open Data
open Extensions
open System
open System.IO
open System.Threading.Tasks
open Thoth.Json.Net
open DSharpPlus.Entities

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

    member __.RussianRoulette(ctx: CommandContext) =
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

    [<Command("rr")>]
    member this.RussianRouletteAsync ctx =
        this.RussianRoulette ctx |> Async.StartAsTask :> Task

    member __.AddResponse (ctx: CommandContext) response =
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

    [<Command("add")>]
    member this.AddResponseAsync(ctx, response) =
        this.AddResponse ctx response |> Async.StartAsTask :> Task

    member __.RemoveResponse (ctx: CommandContext) response =
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

    [<Command("remove")>]
    member this.RemoveResponseAsync(ctx, response) =
        this.RemoveResponse ctx response
        |> Async.StartAsTask
        :> Task

    member __.ListResponses(ctx: CommandContext) =
        async { do! ctx.Respond(String.Join("\n", db.Responses)) }

    [<Command("list")>]
    member this.ListResponsesAsync(ctx) =
        this.ListResponses ctx |> Async.StartAsTask :> Task

    member __.Playing (ctx: CommandContext) name =
        async { do! updateStatus ctx name ActivityType.Playing }

    member __.Watching (ctx: CommandContext) name =
        async { do! updateStatus ctx name ActivityType.Watching }

    member __.ListeningTo (ctx: CommandContext) name =
        async { do! updateStatus ctx name ActivityType.ListeningTo }

    [<Command("playing")>]
    member this.PlayingAsync(ctx, name) =
        this.Playing ctx name |> Async.StartAsTask :> Task

    [<Command("watching")>]
    member this.WatchingAsync(ctx, name) =
        this.Watching ctx name |> Async.StartAsTask :> Task

    [<Command("listeningto")>]
    member this.ListeningToAsync(ctx, name) =
        this.ListeningTo ctx name |> Async.StartAsTask :> Task

    [<Command("playing")>]
    member this.PlayingAsync(ctx) =
        this.Playing ctx "" |> Async.StartAsTask :> Task

    [<Command("watching")>]
    member this.WatchingAsync(ctx) =
        this.Watching ctx "" |> Async.StartAsTask :> Task

    [<Command("listeningto")>]
    member this.ListeningToAsync(ctx) =
        this.ListeningTo ctx "" |> Async.StartAsTask :> Task
