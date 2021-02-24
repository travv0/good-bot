open DSharpPlus
open DSharpPlus.CommandsNext
open DSharpPlus.Entities
open DSharpPlus.EventArgs
open Data
open Emzi0767.Utilities
open FSharpPlus
open GoodBot
open Microsoft.Extensions.Logging
open System
open System.Threading.Tasks

module Core =
    let rand = Random()

    let clientReady (dis: DiscordClient) _ =
        dis.Logger.LogInformation(sprintf "Bot ready using configuration %A" config)

        match db.Status with
        | Some (name, activityType) -> dis.UpdateStatusAsync(DiscordActivity(name, activityType))
        | None -> Task.CompletedTask

    let clientErrored (dis: DiscordClient) (e: ClientErrorEventArgs) =
        dis.Logger.LogError(
            sprintf "Client error: %s: %s: %s" e.EventName (e.Exception.GetType().Name) e.Exception.Message
        )

        Task.CompletedTask

    let socketErrored (dis: DiscordClient) (e: SocketErrorEventArgs) =
        dis.Logger.LogError(sprintf "Socket error: %s: %s" (e.Exception.GetType().Name) e.Exception.Message)
        Task.CompletedTask

    let commandErrored (commands: CommandsNextExtension) (e: CommandErrorEventArgs) =
        match e.Exception with
        | :? ArgumentException ->
            e.Context.RespondAsync(sprintf "Invalid arguments for command **%s**" e.Command.Name) :> Task
        | :? Exceptions.CommandNotFoundException ->
            e.Context.RespondAsync(sprintf "No command named **%s**" e.Command.Name) :> Task
        | _ ->
            commands.Client.Logger.LogError(
                sprintf "Command error: %s: %s: %s" e.Command.Name (e.Exception.GetType().Name) e.Exception.Message
            )

            Task.CompletedTask

    let messageCreated (dis: DiscordClient) (e: MessageCreateEventArgs) =
        if exists ((=) dis.CurrentUser) e.MentionedUsers then
            let responseNum = rand.Next(db.Responses.Length)
            dis.SendMessageAsync(e.Channel, db.Responses.[responseNum]) :> Task
        elif e.Author.Id = 235148962103951360UL then
            dis.SendMessageAsync(e.Channel, "Carl is a cuck") :> Task
        else
            Task.CompletedTask

    let typingStart (dis: DiscordClient) (e: TypingStartEventArgs) =
        if rand.Next(1000) = 0 then
            dis.SendMessageAsync(e.Channel, sprintf "shut up <@%u>" e.User.Id) :> Task
        else
            Task.CompletedTask

    let discordConfig =
        DiscordConfiguration(Token = config.DiscordToken, TokenType = TokenType.Bot, AutoReconnect = true)

    let discord = new DiscordClient(discordConfig)

    discord.add_SocketErrored (AsyncEventHandler<_, _>(socketErrored))
    discord.add_ClientErrored (AsyncEventHandler<_, _>(clientErrored))
    discord.add_Ready (AsyncEventHandler<_, _>(clientReady))
    discord.add_MessageCreated (AsyncEventHandler<_, _>(messageCreated))
    discord.add_TypingStarted (AsyncEventHandler<_, _>(typingStart))

    let commandConfig =
        CommandsNextConfiguration(StringPrefixes = [ config.CommandPrefix ])

    let commands = discord.UseCommandsNext(commandConfig)
    commands.add_CommandErrored (AsyncEventHandler<_, _>(commandErrored))
    commands.RegisterCommands<Commands>()

[<EntryPoint>]
let main _ =
    Core.discord.ConnectAsync()
    |> Async.AwaitTask
    |> Async.RunSynchronously

    Task.Delay(-1)
    |> Async.AwaitTask
    |> Async.RunSynchronously

    0 // return an integer exit code
