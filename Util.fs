module Util

open FParsec

type Parser<'a> = Parser<'a, unit>

module private CodeBlockParsers =
    let blockOpen =
        pstring "```" .>> skipMany letter

    let blockClose = pstring "```"

    let doubleBacktick: Parser<string> =
        pstring "``" .>>? notFollowedBy (pchar '`')

    let singleBacktick: Parser<char> =
        pchar '`' .>>? notFollowedBy (pchar '`')

    let blockContents =
        many1Chars (
            satisfy ((<>) '`')
            <|> (pchar '`' .>>? notFollowedBy (pstring "``"))
        )

    let inlineDoubleContents =
        many1Chars (satisfy ((<>) '`') <|> singleBacktick)

    let inlineSingleContents =
        many1Strings (many1Chars (satisfy ((<>) '`')) <|> doubleBacktick)

    let codeBlockParser =
        choice [ between blockOpen blockClose blockContents
                 between doubleBacktick doubleBacktick inlineDoubleContents
                 between singleBacktick singleBacktick inlineSingleContents ]

open CodeBlockParsers

let parseCodeBlockFromMessage s =
    let parser =
        skipMany (satisfy ((<>) '`')) >>. codeBlockParser
        .>> skipMany anyChar
        .>> eof

    match run parser s with
    | Success (code, _, _) -> Core.Ok code
    | Failure (e, _, _) -> Core.Error e
