module Tests

open System
open Xunit

let unwrapOk =
    function
    | Ok v -> v
    | Error e -> failwithf "%s" e

let unwrapError =
    function
    | Ok v -> failwithf "%A" v
    | Error e -> e

[<Fact>]
let ``calc evals correctly`` () =
    Assert.InRange(Calculator.eval "9 * 18%" |> unwrapOk, 1.62 - 0.01, 1.62 + 0.01)
    Assert.InRange(Calculator.eval "18%" |> unwrapOk, 0.18 - 0.001, 0.18 + 0.001)
    Assert.Equal(12., Calculator.eval "3 * sqrt(2 ^ 4)" |> unwrapOk)

    Assert.InRange(
        Calculator.eval "(1 + 2 * 3 + (1 + 2) * 3 + 1 + (2 * 3))^69"
        |> unwrapOk,
        9.103757594e+93 - 1e+90,
        9.103757594e+93 + 1e+90
    )

    Assert.InRange(
        Calculator.eval "1 + (sin(23.5! * 2) / 5%) - -3213213"
        |> unwrapOk,
        3213213.0656864746 - 1.,
        3213213.0656864746 + 1.
    )

    Assert.Equal(6., Calculator.eval "abs - 6" |> unwrapOk)
    Assert.InRange(Calculator.eval "abs - 6 % + 5!" |> unwrapOk, 120.06 - 0.01, 120.06 + 0.01)
    Assert.InRange(Calculator.eval "abs 6 % + 5!" |> unwrapOk, 120.06 - 0.01, 120.06 + 0.01)
    Assert.Equal(Calculator.eval "sin 5%" |> unwrapOk, Calculator.eval "sin (5%)" |> unwrapOk)
    Assert.NotEqual(Calculator.eval "sin 5%" |> unwrapOk, Calculator.eval "(sin 5)%" |> unwrapOk)

[<Fact>]
let ``calc show helpful parse errors`` () =
    Assert.Equal(
        "Error in Ln: 1 Col: 4
1 +
   ^
Note: The error occurred at the end of the input stream.
Expecting: floating-point number, '(', '-', 'abs', 'cos', 'cosh', 'e', 'ln',
'log', 'pi', 'sin', 'sinh', 'sqrt', 'tan' or 'tanh'
",
        Calculator.eval "1 +" |> unwrapError
    )

    Assert.Equal(
        "Error in Ln: 1 Col: 1
+
^
Expecting: floating-point number, '(', '-', 'abs', 'cos', 'cosh', 'e', 'ln',
'log', 'pi', 'sin', 'sinh', 'sqrt', 'tan' or 'tanh'
",
        Calculator.eval "+" |> unwrapError
    )

    Assert.Equal(
        "Error in Ln: 1 Col: 2
 +
 ^
Expecting: floating-point number, '(', '-', 'abs', 'cos', 'cosh', 'e', 'ln',
'log', 'pi', 'sin', 'sinh', 'sqrt', 'tan' or 'tanh'
",
        Calculator.eval " +" |> unwrapError
    )

    Assert.Equal(
        "Error in Ln: 1 Col: 2
 + \r\n \
 ^
Expecting: floating-point number, '(', '-', 'abs', 'cos', 'cosh', 'e', 'ln',
'log', 'pi', 'sin', 'sinh', 'sqrt', 'tan' or 'tanh'
",
        Calculator.eval " + " |> unwrapError
    )

    Assert.Equal(
        "Error in Ln: 1 Col: 3
(1
  ^
Note: The error occurred at the end of the input stream.
Expecting: '!', '%', ')', '*', '+', '-', '/' or '^'
",
        Calculator.eval "(1" |> unwrapError
    )

    Assert.Equal(
        "Error in Ln: 1 Col: 5
(1+2
    ^
Note: The error occurred at the end of the input stream.
Expecting: '!', '%', ')', '*', '+', '-', '/' or '^'
",
        Calculator.eval "(1+2" |> unwrapError
    )
