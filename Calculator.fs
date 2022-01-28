[<RequireQualifiedAccess>]
module Calculator

open System

module Internal =
    open FParsec

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
        | Floor
        | Ceil
        | Degrees
        | Radians
        | Neg
        | Fact

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
        spaces
        >>. choice [ charReturn '+' Plus
                     charReturn '-' Minus
                     charReturn '*' Times
                     charReturn '/' Divide
                     stringReturn "mod" Mod
                     charReturn '^' Exponent ]
        .>> spaces

    let prefixOp: Parser<PrefixOp> =
        spaces
        >>. choice [ stringReturn "sqrt" Sqrt
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
                     stringReturn "floor" Floor
                     stringReturn "ceil" Ceil
                     stringReturn "degrees" Degrees
                     stringReturn "radians" Radians
                     charReturn '-' Neg
                     stringReturn "fact" Fact ]
        .>> spaces

    let suffixOp: Parser<SuffixOp> =
        spaces
        >>. choice [ charReturn '%' Percent
                     stringReturn "!!" DoubleFactorial
                     charReturn '!' Factorial ]
        .>> spaces

    let rand = Random()

    let value: Parser<Expr> =
        spaces
        >>. choice [ charReturn 'e' Math.E
                     stringReturn "pi" Math.PI
                     pstring "randf" |>> (fun _ -> rand.NextDouble())
                     pstring "randi"
                     |>> (fun _ -> rand.NextInt64() % int64 (2. ** 53) |> float)
                     pfloat ]
        |>> Val
        .>> spaces

    let prefixExpr expr : Parser<Expr> =
        spaces
        >>. pipe2 prefixOp (expr None) (fun op v -> Prefix(op, v))
        .>> spaces

    let parenExpr expr lhs : Parser<Expr> =
        between (pchar '(') (pchar ')') (spaces >>. expr lhs .>> spaces)

    let valExpr expr : Parser<Expr> =
        attempt
        <| parse {
            let! v = value <|> parenExpr expr None

            match! opt suffixOp with
            | Some op -> return! expr (Some(Suffix(v, op)))
            | None -> return v
           }

    let single expr =
        spaces
        >>. (valExpr expr
             <|> prefixExpr expr
             <|> parenExpr expr None)
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

    let rec expr: option<Expr> -> Parser<Expr> =
        function
        | None ->
            parse {
                let! lhs = single expr
                return! expr (Some lhs) <|> preturn lhs
            }
        | Some lhs -> binaryExpr expr lhs <|> preturn lhs

    open MathNet.Numerics

    let factorial n = SpecialFunctions.Gamma(n + 1.)

    let doubleFactorial n =
        let k = n / 2.

        factorial k
        * 2. ** k
        * (Math.PI / 2.)
          ** (1. / 4. * (-1. + cos (n * Math.PI)))

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
        | Prefix (Floor, e) -> floor (reduceExpr e)
        | Prefix (Ceil, e) -> ceil (reduceExpr e)
        | Prefix (Degrees, e) -> (reduceExpr e * 180. / Math.PI)
        | Prefix (Radians, e) -> (reduceExpr e * Math.PI / 180.)
        | Prefix (Neg, e) -> -(reduceExpr e)
        | Prefix (Fact, e) -> factorial (reduceExpr e)

        | Suffix (e, Percent) -> (reduceExpr e) * 0.01
        | Suffix (e, Factorial) -> factorial (reduceExpr e)
        | Suffix (e, DoubleFactorial) -> doubleFactorial (reduceExpr e)

    let parseExpr: Parser<Expr> = expr None .>> eof

open FParsec.CharParsers

let eval s : Result<float, string> =
    match run Internal.parseExpr s with
    | Success (e, _, _) -> Ok(Internal.reduceExpr e)
    | Failure (e, _, _) -> Error e
