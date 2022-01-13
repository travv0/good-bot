open DSharpPlus
open DSharpPlus.CommandsNext
open DSharpPlus.Entities
open DSharpPlus.EventArgs
open Data
open Emzi0767.Utilities
open Extensions
open FSharpPlus
open GoodBot
open Microsoft.Extensions.Logging
open System
open System.Threading.Tasks

module Core =
    let rand = Random()

    let clientReady (dis: DiscordClient) _ =
        dis.Logger.LogInformation $"Bot ready using configuration %A{config}"

        match db.Status with
        | Some (name, activityType) -> dis.UpdateStatusAsync(DiscordActivity(name, activityType))
        | None -> Task.CompletedTask

    let clientErrored (dis: DiscordClient) (e: ClientErrorEventArgs) =
        dis.Logger.LogError
            $"Client error: %s{e.EventName}: %s{e.Exception.GetType().Name}:\
            %s{e.Exception.Message}"

        Task.CompletedTask

    let socketErrored (dis: DiscordClient) (e: SocketErrorEventArgs) =
        dis.Logger.LogError
            $"Socket error: %s{e.Exception.GetType().Name}:\
            %s{e.Exception.Message}"

        Task.CompletedTask

    let commandErrored (commands: CommandsNextExtension) (e: CommandErrorEventArgs) =
        match e.Exception with
        | :? ArgumentException -> e.Context.RespondAsync $"Invalid arguments for command **%s{e.Command.Name}**" :> Task
        | :? Exceptions.CommandNotFoundException ->
            e.Context.RespondAsync $"No command named **%s{e.Command.Name}**" :> Task
        | _ ->
            commands.Client.Logger.LogError
                $"Command error: %s{e.Command.Name}:\
                %s{e.Exception.GetType().Name}: %s{e.Exception.Message}"

            Task.CompletedTask

    let messageCreated (dis: DiscordClient) (e: MessageCreateEventArgs) =
        if not e.Author.IsBot
           && (exists ((=) dis.CurrentUser) e.MentionedUsers
               || e.Message.Content.Contains("@everyone")
               || e.Message.Content.Contains("@here")) then
            let responseNum = rand.Next(db.Responses.Length)
            e.Message.RespondAsync(db.Responses.[responseNum]) :> Task
        elif e.Author.IsCarl then
            e.Message.RespondAsync("Carl is a cuck") :> Task
        else
            Task.CompletedTask

    let typingStart (dis: DiscordClient) (e: TypingStartEventArgs) =
        if rand.Next(5000 / db.Meanness) = 0 then
            dis.SendMessageAsync(e.Channel, $"shut up <@%u{e.User.Id}>") :> Task
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
