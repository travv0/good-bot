module Tests

open System
open Xunit

let unwrapOk =
    function
    | Ok v -> v
    | Error e -> failwithf "%s" e

[<Fact>]
let ``calc eval`` () =
    Assert.Equal(1.62, Calculator.eval "9 * 18%" |> unwrapOk)
    Assert.Equal(12., Calculator.eval "3 * sqrt(2 ^ 4)" |> unwrapOk)

    Assert.InRange(
        Calculator.eval "(1 + 2 * 3 + (1 + 2) * 3 + 1 + (2 * 3))^69"
        |> unwrapOk,
        9.103757594e+93 - 1e+90,
        9.103757594e+93 + 1e+90
    )

    Assert.Equal(
        3213213.0656864746,
        Calculator.eval "1 + (sin(23.5! * 2) / 5%) - -3213213"
        |> unwrapOk
    )

    Assert.Equal(6., Calculator.eval "abs - 6" |> unwrapOk)
    Assert.Equal(120.06, Calculator.eval "abs - 6 % + 5!" |> unwrapOk)
    Assert.Equal(120.06, Calculator.eval "abs 6 % + 5!" |> unwrapOk)
    Assert.Equal(Calculator.eval "sin 5%" |> unwrapOk, Calculator.eval "sin (5%)" |> unwrapOk)
    Assert.NotEqual(Calculator.eval "sin 5%" |> unwrapOk, Calculator.eval "(sin 5)%" |> unwrapOk)
