module Tests

open Extensions
open Xunit

[<Fact>]
let ``calc evals correctly`` () =
    Assert.InRange(Calculator.eval "9 * 18%" |> Result.unwrap, 1.62 - 0.01, 1.62 + 0.01)
    Assert.InRange(Calculator.eval "18%" |> Result.unwrap, 0.18 - 0.001, 0.18 + 0.001)
    Assert.Equal(12., Calculator.eval "3 * sqrt(2 ^ 4)" |> Result.unwrap)

    Assert.InRange(
        Calculator.eval "(1 + 2 * 3 + (1 + 2) * 3 + 1 + (2 * 3))^69"
        |> Result.unwrap,
        9.103757594e+93 - 1e+90,
        9.103757594e+93 + 1e+90
    )

    Assert.InRange(
        Calculator.eval "1 + (sin(23.5! * 2) / 5%) - -3213213"
        |> Result.unwrap,
        3213213.0656864746 - 100.,
        3213213.0656864746 + 100.
    )

    Assert.True(Calculator.eval "abs - 6" |> Result.isError)
    Assert.Equal(6., Calculator.eval "abs (- 6)" |> Result.unwrap)
    Assert.True(Calculator.eval "abs - 6 % + 5!" |> Result.isError)
    Assert.True(Calculator.eval "abs (- 6) % + 5!" |> Result.isOk)
    Assert.True(Calculator.eval "abs -6 % + 5!" |> Result.isOk)
    Assert.InRange(Calculator.eval "abs 6 % + 5!" |> Result.unwrap, 120.06 - 0.01, 120.06 + 0.01)
    Assert.Equal(Calculator.eval "sin 5%" |> Result.unwrap, Calculator.eval "sin (5%)" |> Result.unwrap)
    Assert.NotEqual(Calculator.eval "sin 5%" |> Result.unwrap, Calculator.eval "(sin 5)%" |> Result.unwrap)
    Assert.Equal(1., Calculator.eval "- 2 + 3" |> Result.unwrap)
    Assert.Equal(-1., Calculator.eval "- 2 + 1" |> Result.unwrap)
    Assert.True(Calculator.eval "sqrt sqrt 4" |> Result.isError)
    Assert.True(Calculator.eval "sqrt (sqrt 4)" |> Result.isOk)

[<Fact>]
let ``calc show helpful parse errors`` () =
    Assert.Equal(
        "Error in Ln: 1 Col: 4
1 +
   ^
Note: The error occurred at the end of the input stream.
Expecting: floating-point number, '(', '-', 'abs', 'cbrt', 'ceil', 'cos',
'cosh', 'degrees', 'e', 'fact', 'floor', 'ln', 'log', 'pi', 'radians', 'rand',
'randf', 'randi', 'round', 'sin', 'sinh', 'sqrt', 'tan' or 'tanh'
",
        Calculator.eval "1 +" |> Result.unwrapError
    )

    Assert.Equal(
        "Error in Ln: 1 Col: 1
+
^
Expecting: floating-point number, '(', '-', 'abs', 'cbrt', 'ceil', 'cos',
'cosh', 'degrees', 'e', 'fact', 'floor', 'ln', 'log', 'pi', 'radians', 'rand',
'randf', 'randi', 'round', 'sin', 'sinh', 'sqrt', 'tan' or 'tanh'
",
        Calculator.eval "+" |> Result.unwrapError
    )

    Assert.Equal(
        "Error in Ln: 1 Col: 2
 +
 ^
Expecting: floating-point number, '(', '-', 'abs', 'cbrt', 'ceil', 'cos',
'cosh', 'degrees', 'e', 'fact', 'floor', 'ln', 'log', 'pi', 'radians', 'rand',
'randf', 'randi', 'round', 'sin', 'sinh', 'sqrt', 'tan' or 'tanh'
",
        Calculator.eval " +" |> Result.unwrapError
    )

    Assert.Equal(
        "Error in Ln: 1 Col: 2
 + \r\n \
 ^
Expecting: floating-point number, '(', '-', 'abs', 'cbrt', 'ceil', 'cos',
'cosh', 'degrees', 'e', 'fact', 'floor', 'ln', 'log', 'pi', 'radians', 'rand',
'randf', 'randi', 'round', 'sin', 'sinh', 'sqrt', 'tan' or 'tanh'
",
        Calculator.eval " + " |> Result.unwrapError
    )

    Assert.Equal(
        "Error in Ln: 1 Col: 3
(1
  ^
Note: The error occurred at the end of the input stream.
Expecting: '!', '!!', '%', ')', '*', '+', '-', '/', '^' or 'mod'
",
        Calculator.eval "(1" |> Result.unwrapError
    )

    Assert.Equal(
        "Error in Ln: 1 Col: 5
(1+2
    ^
Note: The error occurred at the end of the input stream.
Expecting: '!', '!!', '%', ')', '*', '+', '-', '/', '^' or 'mod'
",
        Calculator.eval "(1+2" |> Result.unwrapError
    )
