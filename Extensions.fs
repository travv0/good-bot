module Extensions

open DSharpPlus.CommandsNext

type CommandContext with
    member ctx.Respond(message: string) =
        ctx.RespondAsync(message)
        |> Async.AwaitTask
        |> Async.Ignore
