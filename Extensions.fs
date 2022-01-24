module Extensions

open DSharpPlus.CommandsNext
open DSharpPlus.Entities
open System.Threading.Tasks
open System

type CommandContext with
    member ctx.RespondChunked(message: string) =
        task {
            for message in message |> Seq.chunkBySize 2000 do
                ctx.RespondAsync(String(message)).Result |> ignore
        }
        :> Task

type DiscordUser with
    member user.IsCarl = user.Id = 235148962103951360UL

module String =
    let join (s: string) (strs: seq<string>) = String.Join(s, strs)
