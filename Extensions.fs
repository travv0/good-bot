module Extensions

open DSharpPlus.CommandsNext
open DSharpPlus.Entities
open System

type CommandContext with
    member ctx.RespondChunked(message: string) =
        for message in message |> Seq.chunkBySize 2000 do
            ctx.RespondAsync(String(message)).Result |> ignore

type DiscordUser with
    member user.IsCarl =
        user.Id = 235148962103951360UL

[<RequireQualifiedAccess>]
module String =
    let join (s: string) (strs: seq<string>) = String.Join(s, strs)

[<RequireQualifiedAccess>]
module Result =
    let ok ifError =
        function
        | Ok v -> v
        | Error _ -> ifError

    let error ifOk =
        function
        | Ok _ -> ifOk
        | Error e -> e
