[<RequireQualifiedAccess>]
module Calculator

type CalcOp =
    | Plus
    | Minus
    | Times
    | Divide

type CalcExpr =
    | CalcExpr of CalcExpr * CalcOp * CalcExpr
    | CalcVal of float

module private Internal =
    open FParsec

    type Parser<'a> = Parser<'a, unit>

    let calcOp: Parser<CalcOp> =
        spaces
        >>. choice [ pchar '+' >>% Plus
                     pchar '-' >>% Minus
                     pchar '*' >>% Times
                     pchar '/' >>% Divide ]
        .>> spaces

    let calcVal: Parser<CalcExpr> = spaces >>. pfloat .>> spaces |>> CalcVal

    let single, singleRef = createParserForwardedToRef ()

    let rec expr: option<CalcExpr> -> Parser<CalcExpr> =
        function
        | None ->
            parse {
                let! first = single
                return! expr (Some first) <|> preturn first
            }
        | Some prev ->
            parse {
                let! op = calcOp
                let! second = single
                return! expr (Some(CalcExpr(prev, op, second)))
            }
            <|> single
            <|> preturn prev

    let calcParenExpr prev : Parser<CalcExpr> =
        between (pchar '(') (pchar ')') (spaces >>. expr prev .>> spaces)

    singleRef.Value <- calcVal <|> calcParenExpr None

    let rec reduceExpr: CalcExpr -> float =
        function
        | CalcVal v -> v
        | CalcExpr (e1, Plus, e2) -> reduceExpr e1 + reduceExpr e2
        | CalcExpr (e1, Minus, e2) -> reduceExpr e1 - reduceExpr e2
        | CalcExpr (e1, Times, e2) -> reduceExpr e1 * reduceExpr e2
        | CalcExpr (e1, Divide, e2) -> reduceExpr e1 / reduceExpr e2

    let rec exprStr: CalcExpr -> string =
        function
        | CalcVal v -> string v
        | CalcExpr (e1, Plus, e2) -> sprintf "(%s + %s)" (exprStr e1) (exprStr e2)
        | CalcExpr (e1, Minus, e2) -> sprintf "(%s - %s)" (exprStr e1) (exprStr e2)
        | CalcExpr (e1, Times, e2) -> sprintf "(%s * %s)" (exprStr e1) (exprStr e2)
        | CalcExpr (e1, Divide, e2) -> sprintf "(%s / %s)" (exprStr e1) (exprStr e2)

    let calcExpr: Parser<CalcExpr> = expr None .>> eof

open FParsec.CharParsers

let eval s : Result<float, string> =
    match run Internal.calcExpr s with
    | Success (e, _, _) -> Ok(Internal.reduceExpr e)
    | Failure (e, _, _) -> Error e
