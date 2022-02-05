module Util

open FParsec

let parseCodeBlock s =
    let parser =
        skipMany (satisfy ((<>) '`'))
        >>. choice [ between (pstring "```") (pstring "```") (manyChars (satisfy ((<>) '`')))
                     between (pchar '`') (pchar '`') (manyChars (satisfy ((<>) '`'))) ]
        .>> skipMany anyChar
        .>> eof

    match run parser s with
    | Success (code, _, _) -> code
    | Failure _ -> s
