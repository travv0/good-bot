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

[<RequireQualifiedAccess>]
module String =
    let join (s: string) (strs: seq<string>) = String.Join(s, strs)

[<RequireQualifiedAccess>]
module Result =
    let unwrap =
        function
        | Ok v -> v
        | Error e -> failwithf "%s" e

    let unwrapError =
        function
        | Ok v -> failwithf "%A" v
        | Error e -> e

    let isOk =
        function
        | Ok v -> true
        | Error e -> false

    let isError =
        function
        | Ok v -> false
        | Error e -> true
