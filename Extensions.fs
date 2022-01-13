module Extensions

open DSharpPlus.CommandsNext
open DSharpPlus.Entities
open FSharpPlus
open System.Threading.Tasks

type CommandContext with
    member ctx.RespondChunked(message: string) =
        task {
            for message in message |> Seq.chunkBySize 2000 do
                ctx.RespondAsync(message |> ofSeq).Result
                |> ignore
        }
        |> Task.ignore

type DiscordUser with
    member user.IsCarl = user.Id = 235148962103951360UL
