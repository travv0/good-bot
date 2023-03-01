[<RequireQualifiedAccess>]
module Calculator

open FParsec
open System

module Internal =
    type BinaryOp =
        | Plus
        | Minus
        | Times
        | Divide
        | Exponent
        | Mod

    type PrefixOp =
        | Sqrt
        | Cbrt
        | Log
        | Ln
        | Sin
        | Cos
        | Tan
        | Sinh
        | Cosh
        | Tanh
        | Abs
        | Round
        | Floor
        | Ceil
        | Degrees
        | Radians
        | Neg
        | Fact
        | RandFloat
        | RandInt
        | Rand

    type SuffixOp =
        | Percent
        | Factorial
        | DoubleFactorial

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
        | Mod -> 3
        | Exponent -> 8

    let binaryOp: Parser<BinaryOp> =
        choice [ charReturn '+' Plus
                 charReturn '-' Minus
                 charReturn '*' Times
                 charReturn '/' Divide
                 stringReturn "mod" Mod
                 charReturn '^' Exponent ]
        .>> spaces
        <?> "operator"

    let prefixOp: Parser<PrefixOp> =
        choice [ stringReturn "sqrt" Sqrt
                 stringReturn "cbrt" Cbrt
                 stringReturn "log" Log
                 stringReturn "ln" Ln
                 stringReturn "sinh" Sinh
                 stringReturn "cosh" Cosh
                 stringReturn "tanh" Tanh
                 stringReturn "sin" Sin
                 stringReturn "cos" Cos
                 stringReturn "tan" Tan
                 stringReturn "abs" Abs
                 stringReturn "round" Round
                 stringReturn "floor" Floor
                 stringReturn "ceil" Ceil
                 stringReturn "degrees" Degrees
                 stringReturn "radians" Radians
                 charReturn '-' Neg
                 stringReturn "fact" Fact
                 stringReturn "randf" RandFloat
                 stringReturn "randi" RandInt
                 stringReturn "rand" Rand ]
        .>>? (spaces1 <|> lookAhead (skipChar '('))
        <?> "function"

    let suffixOp: Parser<SuffixOp> =
        choice [ charReturn '%' Percent
                 stringReturn "!!" DoubleFactorial
                 charReturn '!' Factorial ]
        .>> spaces
        <?> "suffix"

    let constant =
        choice [ charReturn 'e' Math.E
                 stringReturn "pi" Math.PI ]
        .>> spaces
        <?> "constant"

    let number = pfloat .>> spaces <?> "number"

    let value: Parser<Expr> =
        constant <|> number |>> Val .>> spaces

    let parenExpr expr : Parser<Expr> =
        between (pchar '(' >>. spaces) (pchar ')' >>. spaces) (expr None)
        <?> "parenthesized expression"

    let valExpr expr : Parser<Expr> =
        parse {
            let! v = value <|> parenExpr expr

            match! opt suffixOp with
            | Some op -> return! expr (Some(Suffix(v, op)))
            | None -> return v
        }
        .>> spaces

    let prefixExpr expr : Parser<Expr> =
        pipe2 prefixOp (valExpr expr <|> parenExpr expr) (fun op v ->
            Prefix(op, v))
        .>> spaces

    let single expr =
        valExpr expr
        <|> prefixExpr expr
        <|> parenExpr expr
        .>> spaces

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
        .>> spaces

    let rec expr lhs : Parser<Expr> =
        match lhs with
        | None ->
            parse {
                let! lhs = single expr
                return! expr (Some lhs) <|> preturn lhs
            }
        | Some lhs -> binaryExpr expr lhs <|> preturn lhs
        .>> spaces

    open MathNet.Numerics

    let factorial n = SpecialFunctions.Gamma(n + 1.)

    let doubleFactorial n =
        let k = n / 2.

        factorial k
        * 2. ** k
        * (Math.PI / 2.)
          ** (1. / 4. * (-1. + cos (n * Math.PI)))

    let randGen = Random()

    let randi n =
        (randGen.NextInt64() % int64 (2. ** 53) |> float) % round n

    let randf n = randGen.NextDouble() * n

    let round (n: float) =
        Math.Round(n, MidpointRounding.AwayFromZero)

    let rec reduceExpr: Expr -> float =
        function
        | Val v -> v

        | Binary (e1, Plus, e2) -> reduceExpr e1 + reduceExpr e2
        | Binary (e1, Minus, e2) -> reduceExpr e1 - reduceExpr e2
        | Binary (e1, Times, e2) -> reduceExpr e1 * reduceExpr e2
        | Binary (e1, Divide, e2) -> reduceExpr e1 / reduceExpr e2
        | Binary (e1, Exponent, e2) -> reduceExpr e1 ** reduceExpr e2
        | Binary (e1, Mod, e2) -> reduceExpr e1 % reduceExpr e2

        | Prefix (Sqrt, e) -> sqrt (reduceExpr e)
        | Prefix (Cbrt, e) -> Math.Cbrt(reduceExpr e)
        | Prefix (Log, e) -> log10 (reduceExpr e)
        | Prefix (Ln, e) -> log (reduceExpr e)
        | Prefix (Sin, e) -> sin (Math.PI / 180. * reduceExpr e)
        | Prefix (Cos, e) -> cos (Math.PI / 180. * reduceExpr e)
        | Prefix (Tan, e) -> tan (Math.PI / 180. * reduceExpr e)
        | Prefix (Sinh, e) -> sinh (reduceExpr e)
        | Prefix (Cosh, e) -> cosh (reduceExpr e)
        | Prefix (Tanh, e) -> tanh (reduceExpr e)
        | Prefix (Abs, e) -> abs (reduceExpr e)
        | Prefix (Round, e) -> round (reduceExpr e)
        | Prefix (Floor, e) -> floor (reduceExpr e)
        | Prefix (Ceil, e) -> ceil (reduceExpr e)
        | Prefix (Degrees, e) -> (reduceExpr e * 180. / Math.PI)
        | Prefix (Radians, e) -> (reduceExpr e * Math.PI / 180.)
        | Prefix (Neg, e) -> -(reduceExpr e)
        | Prefix (Fact, e) -> factorial (reduceExpr e)
        | Prefix (RandFloat, e) -> randf (reduceExpr e)
        | Prefix (RandInt, e) -> randi (reduceExpr e)
        | Prefix (Rand, e) ->
            let n = reduceExpr e in if n = round n then randi n else randf n

        | Suffix (e, Percent) -> (reduceExpr e) * 0.01
        | Suffix (e, Factorial) -> factorial (reduceExpr e)
        | Suffix (e, DoubleFactorial) -> doubleFactorial (reduceExpr e)

    let parseExpr: Parser<Expr> =
        spaces >>. expr None .>> eof

let eval s : Result<float, string> =
    match run Internal.parseExpr s with
    | Success (e, _, _) -> Core.Ok(Internal.reduceExpr e)
    | Failure (e, _, _) -> Core.Error e
