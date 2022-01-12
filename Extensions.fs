module Extensions

open DSharpPlus.CommandsNext
open DSharpPlus.Entities
open FSharpPlus
open System.Threading.Tasks

type CommandContext with
    member ctx.RespondChunked(message: string) =
        for message in message |> Seq.chunkBySize 2000 do
            ctx.RespondAsync(message |> ofSeq)
            |> Async.AwaitTask
            |> Async.Ignore
            |> Async.RunSynchronously

        Task.CompletedTask |> Async.AwaitTask

type DiscordUser with
    member user.IsCarl = user.Id = 235148962103951360UL
