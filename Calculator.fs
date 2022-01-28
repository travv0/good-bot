[<RequireQualifiedAccess>]
module Calculator

type BinaryOp =
    | Plus
    | Minus
    | Times
    | Divide
    | Exponent

type PrefixOp = | Sqrt

type Expr =
    | Binary of Expr * BinaryOp * Expr
    | Prefix of PrefixOp * Expr
    | Val of float

module private Internal =
    open FParsec

    type Parser<'a> = Parser<'a, unit>

    let prec =
        function
        | Plus -> 2
        | Minus -> 2
        | Times -> 3
        | Divide -> 3
        | Exponent -> 8

    let binaryOp: Parser<BinaryOp> =
        spaces
        >>. choice [ charReturn '+' Plus
                     charReturn '-' Minus
                     charReturn '*' Times
                     charReturn '/' Divide
                     charReturn '^' Exponent ]
        .>> spaces

    let prefixOp: Parser<PrefixOp> =
        spaces >>. choice [ stringReturn "sqrt" Sqrt ]
        .>> spaces

    let value: Parser<Expr> = spaces >>. pfloat .>> spaces |>> Val

    let single, singleRef = createParserForwardedToRef ()

    let rec expr: option<Expr> -> Parser<Expr> =
        function
        | None ->
            parse {
                let! lhs = single
                return! expr (Some lhs) <|> preturn lhs
            }
        | Some lhs ->
            parse {
                let! op = binaryOp
                let p = prec op
                let! rhs = single
                let! nextOp = lookAhead (opt binaryOp)

                let nextPrecIsHigher =
                    nextOp
                    |> Option.map (fun nop -> prec nop > p)
                    |> Option.defaultValue false

                if nextPrecIsHigher then
                    return! expr (Some rhs) |>> fun e -> Binary(lhs, op, e)
                else
                    return! expr (Some(Binary(lhs, op, rhs)))
            }
            <|> single
            <|> preturn lhs

    let parenExpr lhs : Parser<Expr> =
        between (pchar '(') (pchar ')') (spaces >>. expr lhs .>> spaces)

    let prefixExpr: Parser<Expr> =
        spaces
        >>. pipe2 prefixOp (expr None) (fun op v -> Prefix(op, v))
        .>> spaces

    singleRef.Value <- value <|> prefixExpr <|> parenExpr None

    let rec reduceExpr: Expr -> float =
        function
        | Val v -> v
        | Binary (e1, Plus, e2) -> reduceExpr e1 + reduceExpr e2
        | Binary (e1, Minus, e2) -> reduceExpr e1 - reduceExpr e2
        | Binary (e1, Times, e2) -> reduceExpr e1 * reduceExpr e2
        | Binary (e1, Divide, e2) -> reduceExpr e1 / reduceExpr e2
        | Binary (e1, Exponent, e2) -> reduceExpr e1 ** reduceExpr e2
        | Prefix (Sqrt, e) -> sqrt (reduceExpr e)

    let rec exprStr: Expr -> string =
        function
        | Val v -> string v
        | Binary (e1, Plus, e2) -> sprintf "(%s + %s)" (exprStr e1) (exprStr e2)
        | Binary (e1, Minus, e2) -> sprintf "(%s - %s)" (exprStr e1) (exprStr e2)
        | Binary (e1, Times, e2) -> sprintf "(%s * %s)" (exprStr e1) (exprStr e2)
        | Binary (e1, Divide, e2) -> sprintf "(%s / %s)" (exprStr e1) (exprStr e2)
        | Binary (e1, Exponent, e2) -> sprintf "(%s ^ %s)" (exprStr e1) (exprStr e2)
        | Prefix (Sqrt, e) -> sprintf "sqrt %s" (exprStr e)

    let parseExpr: Parser<Expr> = expr None .>> eof

open FParsec.CharParsers

let eval s : Result<float, string> =
    match run Internal.parseExpr s with
    | Success (e, _, _) -> Ok(Internal.reduceExpr e)
    | Failure (e, _, _) -> Error e
