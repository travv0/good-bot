[<RequireQualifiedAccess>]
module Calculator

module Internal =
    open FParsec

    type BinaryOp =
        | Plus
        | Minus
        | Times
        | Divide
        | Exponent

    type PrefixOp =
        | Sqrt
        | Log

    type SuffixOp = | Percent

    type Expr =
        | Binary of Expr * BinaryOp * Expr
        | Prefix of PrefixOp * Expr
        | Suffix of Expr * SuffixOp
        | Val of float

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
        spaces
        >>. choice [ stringReturn "sqrt" Sqrt
                     stringReturn "log" Log ]
        .>> spaces

    let suffixOp: Parser<SuffixOp> =
        spaces >>. choice [ charReturn '%' Percent ]
        .>> spaces

    let value: Parser<Expr> = spaces >>. pfloat .>> spaces |>> Val

    let prefixExpr expr : Parser<Expr> =
        spaces
        >>. pipe2 prefixOp (expr None) (fun op v -> Prefix(op, v))
        .>> spaces

    let suffixExpr expr lhs : Parser<Expr> =
        attempt suffixOp
        >>= fun op -> expr (Some(Suffix(lhs, op)))

    let parenExpr expr lhs : Parser<Expr> =
        between (pchar '(') (pchar ')') (spaces >>. expr lhs .>> spaces)

    let single expr =
        value <|> prefixExpr expr <|> parenExpr expr None

    let binaryExpr expr lhs =
        parse {
            let! op = binaryOp
            let p = prec op
            let! rhs = single expr
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

    let rec expr: option<Expr> -> Parser<Expr> =
        function
        | None ->
            parse {
                let! lhs = single expr
                return! expr (Some lhs) <|> preturn lhs
            }
        | Some lhs ->
            choice [ suffixExpr expr lhs
                     binaryExpr expr lhs
                     single expr
                     preturn lhs ]

    let rec reduceExpr: Expr -> float =
        function
        | Val v -> v
        | Binary (e1, Plus, e2) -> reduceExpr e1 + reduceExpr e2
        | Binary (e1, Minus, e2) -> reduceExpr e1 - reduceExpr e2
        | Binary (e1, Times, e2) -> reduceExpr e1 * reduceExpr e2
        | Binary (e1, Divide, e2) -> reduceExpr e1 / reduceExpr e2
        | Binary (e1, Exponent, e2) -> reduceExpr e1 ** reduceExpr e2
        | Prefix (Sqrt, e) -> sqrt (reduceExpr e)
        | Prefix (Log, e) -> log (reduceExpr e)
        | Suffix (e, Percent) -> (reduceExpr e) * 0.01

    let rec exprStr: Expr -> string =
        function
        | Val v -> string v
        | Binary (e1, Plus, e2) -> sprintf "(%s + %s)" (exprStr e1) (exprStr e2)
        | Binary (e1, Minus, e2) -> sprintf "(%s - %s)" (exprStr e1) (exprStr e2)
        | Binary (e1, Times, e2) -> sprintf "(%s * %s)" (exprStr e1) (exprStr e2)
        | Binary (e1, Divide, e2) -> sprintf "(%s / %s)" (exprStr e1) (exprStr e2)
        | Binary (e1, Exponent, e2) -> sprintf "(%s ^ %s)" (exprStr e1) (exprStr e2)
        | Prefix (Sqrt, e) -> sprintf "sqrt %s" (exprStr e)
        | Prefix (Log, e) -> sprintf "log %s" (exprStr e)
        | Suffix (e, Percent) -> sprintf "%s%%" (exprStr e)

    let parseExpr: Parser<Expr> = expr None .>> eof

open FParsec.CharParsers

let eval s : Result<float, string> =
    match run Internal.parseExpr s with
    | Success (e, _, _) -> Ok(Internal.reduceExpr e)
    | Failure (e, _, _) -> Error e
