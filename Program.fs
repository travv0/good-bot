open System.Threading
open DSharpPlus
open DSharpPlus.CommandsNext
open DSharpPlus.Entities
open DSharpPlus.EventArgs
open Data
open Emzi0767.Utilities
open GoodBot
open Microsoft.Extensions.Logging
open System
open System.Threading.Tasks

module Core =
    let rand = Random()

    let rec postYoutubeUpdates (dis: DiscordClient) =
        match (getDb ()).YoutubeUpdatesChannel with
        | Some channel ->
            try
                let updates = Youtube.getYoutubeUpdates ()

                if List.length updates > 0 then
                    dis.Logger.LogInformation
                        $"Found %d{List.length updates} new Youtube updates"

                for update in updates do
                    match update with
                    | Some update ->
                        dis.SendMessageAsync(
                            dis.GetChannelAsync(uint64 channel).Result,
                            update
                        )
                        |> ignore
                    | None -> ()
            with exn ->
                dis.Logger.LogError
                    $"Error getting Youtube updates: %s{exn.Message}"

            try
                let communityUpdates = Youtube.getCommunityUpdates ()

                if List.length communityUpdates > 0 then
                    dis.Logger.LogInformation
                        $"Found %d{List.length communityUpdates} new Youtube community updates"

                for update in communityUpdates do
                    match update with
                    | Ok update ->
                        dis.SendMessageAsync(
                            dis.GetChannelAsync(uint64 channel).Result,
                            update
                        )
                        |> ignore
                    | Error e ->
                        dis.Logger.LogError
                            $"Error getting Youtube community update from channel %d{channel}: %s{e}"
            with exn ->
                dis.Logger.LogError
                    $"Error getting Youtube community updates: %s{exn.Message}"
        | None -> ()

        Thread.Sleep(1000 * 60)
        postYoutubeUpdates dis

    let clientReady (dis: DiscordClient) _ =
        dis.Logger.LogInformation $"Bot ready using configuration %A{config}"

        match (getDb ()).Status with
        | Some(name, activityType) ->
            dis.UpdateStatusAsync(DiscordActivity(name, activityType))
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

    let commandErrored
        (commands: CommandsNextExtension)
        (e: CommandErrorEventArgs)
        =
        match e.Exception with
        | :? ArgumentException ->
            e.Context.RespondAsync
                $"Invalid arguments for command **%s{e.Command.Name}**: %s{e.Exception.Message}"
            :> Task
        | :? Exceptions.CommandNotFoundException -> Task.CompletedTask
        | exn ->
            commands.Client.Logger.LogError
                $"Command error: %s{e.Command.Name}: %s{exn.GetType().Name}: %s{exn.Message}"

            Task.CompletedTask

    let messageCreated (dis: DiscordClient) (e: MessageCreateEventArgs) =
        if
            not e.Author.IsBot
            && (Seq.contains dis.CurrentUser e.MentionedUsers
                || e.Message.Content.Contains("@everyone")
                || e.Message.Content.Contains("@here"))
        then
            let responseNum = rand.Next((getDb ()).Responses.Length)
            let response = (getDb ()).Responses[responseNum]

            updateDb
                { getDb () with
                    LastResponse = Some response }

            e.Message.RespondAsync(response) :> Task

        elif (getDb ()).AutoReplies.ContainsKey(e.Message.Author.Id) then
            let replyRate =
                (getDb ()).AutoReplyRates
                |> Map.tryFind e.Message.Author.Id
                |> Option.defaultValue 100M

            if rand.NextDouble() * 100.0 < double replyRate then
                e.Message.RespondAsync(
                    (getDb ()).AutoReplies[e.Message.Author.Id]
                )
                :> Task
            else
                Task.CompletedTask

        else
            Task.CompletedTask

    let typingStart (dis: DiscordClient) (e: TypingStartEventArgs) =
        let meannessToRatio =
            function
            | 11 -> 1
            | n -> 2000 / n

        if
            not e.User.IsCurrent
            && (getDb ()).Meanness > 0
            && rand.Next(meannessToRatio (getDb ()).Meanness) = 0
        then
            dis.SendMessageAsync(e.Channel, $"shut up <@%u{e.User.Id}>") :> Task
        else
            Task.CompletedTask

    let discordConfig =
        DiscordConfiguration(
            Token = config.DiscordToken,
            TokenType = TokenType.Bot,
            Intents = DiscordIntents.All,
            AutoReconnect = true
        )

    let discord = new DiscordClient(discordConfig)

    discord.add_SocketErrored (AsyncEventHandler<_, _>(socketErrored))
    discord.add_ClientErrored (AsyncEventHandler<_, _>(clientErrored))
    discord.add_Ready (AsyncEventHandler<_, _>(clientReady))
    discord.add_MessageCreated (AsyncEventHandler<_, _>(messageCreated))
    discord.add_TypingStarted (AsyncEventHandler<_, _>(typingStart))

    let commandConfig =
        CommandsNextConfiguration(
            StringPrefixes = [ config.CommandPrefix ],
            EnableMentionPrefix = false
        )

    let commands = discord.UseCommandsNext(commandConfig)

    commands.add_CommandErrored (AsyncEventHandler<_, _>(commandErrored))
    commands.RegisterCommands<Commands>()

[<EntryPoint>]
let main _ =
    Core.discord.ConnectAsync() |> Async.AwaitTask |> Async.RunSynchronously

    let youtubeThread =
        Thread(ThreadStart(fun () -> Core.postYoutubeUpdates Core.discord))

    youtubeThread.Start()

    Task.Delay(-1) |> Async.AwaitTask |> Async.RunSynchronously
    0
