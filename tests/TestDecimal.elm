module TestDecimal exposing (tests)

import Decimal as D exposing (Decimal)
import Expect
import Fuzz
import Random
import Test exposing (Test, describe, fuzz, fuzz2, fuzzWith, only, skip, test)


fuzzDecimal : Fuzz.Fuzzer Decimal
fuzzDecimal =
    Fuzz.map2 D.fromSigExp Fuzz.float Fuzz.int


formatSigfigs : Int -> String -> String
formatSigfigs n str =
    str |> String.split "e" |> List.map (String.slice 0 n >> String.replace "+" "") |> String.join "e"


formatF =
    formatSigfigs 12


expectFDEqual : Float -> Decimal -> Expect.Expectation
expectFDEqual f d =
    Expect.equal
        (f |> String.fromFloat |> formatF)
        (d |> D.toString |> formatF)


expectFEqual : Float -> Float -> Expect.Expectation
expectFEqual a b =
    if isNaN a || isNaN b then
        Expect.equal (String.fromFloat a) (String.fromFloat b)

    else
        Expect.within (Expect.Absolute 1.0e-10) a b


tests : Test
tests =
    describe "tests"
        [ describe "Decimal parsing"
            [ fuzz Fuzz.float "toString on small floats" <|
                \f ->
                    D.fromFloat f
                        |> expectFDEqual f
            , fuzz Fuzz.float "toString/fromString are reversible" <|
                \f ->
                    f
                        |> D.fromFloat
                        |> D.toString
                        |> D.fromString
                        |> Expect.equal (f |> D.fromFloat |> Just)
            , fuzz Fuzz.string "bogus strings don't parse" <|
                \s ->
                    s
                        |> D.fromString
                        |> Maybe.map D.toString
                        -- |> Expect.equal (s |> Just)
                        |> Expect.equal (s |> String.toFloat |> Maybe.map String.fromFloat)
            , describe "string-parsing unit tests" <|
                List.map
                    (\s ->
                        test ("decimal.fromString: " ++ s) <|
                            \() ->
                                s
                                    |> D.fromString
                                    |> Maybe.map D.toString
                                    |> Expect.equal (s |> String.toFloat |> Maybe.map String.fromFloat)
                    )
                <|
                    [ "0", "1", "-1", "100", "1e2", "-100", "-1e2", "invalid", "1ex", "xe1", "" ]
            , fuzz fuzzDecimal "decimal fuzzer works" <|
                \d ->
                    d
                        -- |> Debug.log "fuzzDecimal val"
                        |> D.toString
                        -- |> Debug.log "fuzzDecimal str"
                        |> Expect.equal (d |> D.toString)
            , test "normalize large-significand" <|
                \() ->
                    Expect.equal
                        (D.fromSigExp 3000 0)
                        (D.fromSigExp 3 3)
            , test "normalize small-significand" <|
                \() ->
                    Expect.equal
                        (D.fromSigExp (3 / 1000) 0)
                        (D.fromSigExp 3 -3)
            , test "normalize zero with exponent" <|
                \() ->
                    Expect.equal
                        (D.fromSigExp 0 999)
                        (D.fromSigExp 0 0)
            , test "normalize zero with small exponent" <|
                \() ->
                    Expect.equal
                        (D.fromSigExp 0 -999)
                        (D.fromSigExp 0 0)
            , test "stringify large decimal" <|
                \() ->
                    D.fromSigExp 0.12345 10000
                        |> D.toString
                        |> Expect.equal "1.2345e9999"
            , let
                fuzzDecString =
                    -- limit significand range so we can verify fuzzed strings easily
                    Fuzz.map2 (\s e -> String.fromFloat s ++ "e" ++ String.fromInt e) (Fuzz.floatRange 1 9.9999) (Fuzz.intRange 21 Random.maxInt)
              in
              fuzz fuzzDecString "parse large decimal-string" <|
                \s ->
                    s
                        |> D.fromString
                        |> Maybe.map D.toString
                        |> Expect.equal (s |> Just)
            ]
        , describe "Decimal math"
            [ test "0+0" <|
                \() -> D.fromSigExp 0 0 |> D.add (D.fromSigExp 0 0) |> Expect.equal (D.fromSigExp 0 0)
            , test "0+1" <|
                \() -> D.fromSigExp 0 0 |> D.add (D.fromSigExp 1 0) |> Expect.equal (D.fromSigExp 1 0)
            , test "1+1" <|
                \() -> D.fromSigExp 1 0 |> D.add (D.fromSigExp 1 0) |> Expect.equal (D.fromSigExp 2 0)
            , test "9+1" <|
                \() -> D.fromSigExp 1 0 |> D.add (D.fromSigExp 9 0) |> Expect.equal (D.fromSigExp 1 1)
            , test "10+1" <|
                \() -> D.fromSigExp 1 0 |> D.add (D.fromSigExp 1 1) |> Expect.equal (D.fromSigExp 1.1 1)
            , test "1e20+1" <|
                \() -> D.fromSigExp 1 0 |> D.add (D.fromSigExp 1 20) |> Expect.equal (D.fromSigExp 1 20)
            , fuzz2 Fuzz.float Fuzz.float "dec addition == float addition" <|
                \a b ->
                    D.add (D.fromFloat a) (D.fromFloat b)
                        |> expectFDEqual (a + b)
            , fuzz Fuzz.float "dec negation == float negation" <|
                \a ->
                    D.neg (D.fromFloat a)
                        |> expectFDEqual -a
            , fuzz2 Fuzz.float Fuzz.float "dec subtraction == float subtraction" <|
                \a b ->
                    D.sub (D.fromFloat a) (D.fromFloat b)
                        |> expectFDEqual (a - b)
            , fuzz2 Fuzz.float Fuzz.float "dec mul == float mul" <|
                \a b ->
                    D.mul (D.fromFloat a) (D.fromFloat b)
                        |> expectFDEqual (a * b)
            , fuzz2 Fuzz.float Fuzz.float "dec div == float div" <|
                \a b ->
                    D.div (D.fromFloat a) (D.fromFloat b)
                        |> expectFDEqual (a / b)
            , fuzz Fuzz.float "dec log10 == float log10" <|
                \a ->
                    D.log10 (D.fromFloat a)
                        |> expectFEqual (logBase 10 a)

            -- this one quickly goes to infinity for floats, so limit the range
            , fuzz2 (Fuzz.floatRange -1000 1000) (Fuzz.floatRange -100 100) "dec pow == float pow" <|
                \a b ->
                    D.powFloat (D.fromFloat a) b
                        |> expectFDEqual (a ^ b)
            ]
        , describe "comparison"
            [ fuzz2 Fuzz.float Fuzz.float "dec compare == float compare" <|
                \a b ->
                    D.compare (D.fromFloat a) (D.fromFloat b)
                        |> Expect.equal (compare a b)
            , fuzz2 Fuzz.float Fuzz.float "dec gt == float gt" <|
                \a b ->
                    D.gt (D.fromFloat a) (D.fromFloat b)
                        |> Expect.equal (a > b)
            , fuzz2 Fuzz.float Fuzz.float "dec gte == float gte" <|
                \a b ->
                    D.gte (D.fromFloat a) (D.fromFloat b)
                        |> Expect.equal (a >= b)
            , fuzz2 Fuzz.float Fuzz.float "dec lt == float lt" <|
                \a b ->
                    D.lt (D.fromFloat a) (D.fromFloat b)
                        |> Expect.equal (a < b)
            , fuzz2 Fuzz.float Fuzz.float "dec lte == float lte" <|
                \a b ->
                    D.lte (D.fromFloat a) (D.fromFloat b)
                        |> Expect.equal (a <= b)
            , fuzz2 Fuzz.float Fuzz.float "dec min == float min" <|
                \a b ->
                    D.min (D.fromFloat a) (D.fromFloat b)
                        |> expectFDEqual (min a b)
            , fuzz2 Fuzz.float Fuzz.float "dec max == float max " <|
                \a b ->
                    D.max (D.fromFloat a) (D.fromFloat b)
                        |> expectFDEqual (max a b)
            ]
        ]
