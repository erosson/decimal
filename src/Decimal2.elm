module Decimal2 exposing
    ( Decimal, create, fromFloatN, fromFloat10, fromString, toString, scale, value, tuple
    , compare, eq, ne, gt, gte, lt, lte, min, max
    , add, sub, mul, div, divide, powInt, mulInt
    , roundTo, roundN, round, floorN, floor, ceilN, ceil
    , sum, product, minimum, maximum
    , neg, abs, clamp
    , flipSub, flipPowInt
    , setScale, matchScales, trim, normalize
    , isNaN, isInfinite, isFinite
    , zero, one, negativeOne, ten
    )

{-| A fixed-point decimal type with 32 bits of precision in both its value and exponent.

Decimals are expressed as a pair of `Int`s - `Decimal Int Int` - signed mantissa/significand and exponent, respectively.

Floating-point is the usual way to handle decimal values, but this file is a relatively simple way to add more exponent-precision when necessary.


# Construction

@docs Decimal, create, fromFloatN, fromFloat10, fromString, toString, scale, value, tuple


# Comparison

@docs compare, eq, ne, gt, gte, lt, lte, min, max


# Math

@docs add, sub, mul, div, divide, powInt, mulInt

#Rounding

@docs roundTo, roundN, round, floorN, floor, ceilN, ceil


# List operations

@docs sum, product, minimum, maximum


# Fancier math

@docs neg, abs, clamp, sqrt, logBase, log10


# Pipelined math

@docs flipSub, flipDiv, flipPowInt


# Rescaling

@docs setScale, matchScales, trim, normalize

#Validation

@docs isNaN, isInfinite, isFinite


# Common values

@docs zero, one, negativeOne, ten

-}


type Decimal
    = Decimal Int Int


create : Int -> Int -> Decimal
create val scl =
    Decimal val scl


base : Int
base =
    10


baseF : Float
baseF =
    base |> Basics.toFloat


infiniteInt : Int
infiniteInt =
    1 / 0 |> Basics.floor


value : Decimal -> Int
value (Decimal val scl) =
    val


scale : Decimal -> Int
scale (Decimal _ scl) =
    scl


tuple : Decimal -> ( Int, Int )
tuple (Decimal val scl) =
    ( val, scl )


setScale : Int -> Decimal -> Decimal
setScale scale1 (Decimal val0 scale0) =
    let
        dScale =
            scale1 - scale0
    in
    Decimal
        (if dScale == 0 then
            val0

         else if dScale > 0 then
            val0 * base ^ dScale

         else
            -- this is the same thing on paper, but there seems to be an elm bug:
            -- negative powers can return an "int" containing a decimal.
            -- Workaround: avoid negative powers using division.
            val0 // base ^ -dScale
        )
        scale1


matchScales : Decimal -> Decimal -> ( Decimal, Decimal )
matchScales ((Decimal valA sclA) as a) ((Decimal valB sclB) as b) =
    if sclA < sclB then
        ( setScale sclB a, b )

    else if sclA > sclB then
        ( a, setScale sclA b )

    else
        ( a, b )



-- Comparison


compare : Decimal -> Decimal -> Order
compare a0 b0 =
    let
        ( Decimal valA _, Decimal valB _ ) =
            matchScales a0 b0
    in
    Basics.compare valA valB


eq : Decimal -> Decimal -> Bool
eq a b =
    compare a b == EQ


ne : Decimal -> Decimal -> Bool
ne a b =
    compare a b /= EQ


gt : Decimal -> Decimal -> Bool
gt a b =
    compare a b == GT


gte : Decimal -> Decimal -> Bool
gte a b =
    compare a b /= LT


lt : Decimal -> Decimal -> Bool
lt a b =
    compare a b == LT


lte : Decimal -> Decimal -> Bool
lte a b =
    compare a b /= GT


min : Decimal -> Decimal -> Decimal
min a b =
    if lt a b then
        a

    else
        b


max : Decimal -> Decimal -> Decimal
max a b =
    if gt a b then
        a

    else
        b


minimum : List Decimal -> Maybe Decimal
minimum ds =
    case ds of
        [] ->
            Nothing

        head :: tail ->
            Just <| List.foldl min head tail


maximum : List Decimal -> Maybe Decimal
maximum ds =
    case ds of
        [] ->
            Nothing

        head :: tail ->
            Just <| List.foldl max head tail


abs : Decimal -> Decimal
abs (Decimal val scl) =
    Decimal (Basics.abs val) scl


clamp : Decimal -> Decimal -> Decimal -> Decimal
clamp bot top =
    min top >> max bot



-- math ops


add : Decimal -> Decimal -> Decimal
add a0 b0 =
    let
        ( Decimal valA scl, Decimal valB _ ) =
            matchScales a0 b0
    in
    Decimal (valA + valB) scl


sum : List Decimal -> Decimal
sum =
    List.foldl add zero


neg : Decimal -> Decimal
neg (Decimal val scl) =
    Decimal -val scl


sub : Decimal -> Decimal -> Decimal
sub a =
    neg >> add a


{-| Pipeline-friendly subtraction

a - b == sub a b == flipSub b a == a |> flipSub b

-}
flipSub : Decimal -> Decimal -> Decimal
flipSub a b =
    sub b a


mul : Decimal -> Decimal -> Decimal
mul (Decimal valA sclA) (Decimal valB sclB) =
    Decimal (valA * valB) (sclA + sclB)


product : List Decimal -> Decimal
product =
    List.foldl mul one


mulInt : Int -> Decimal -> Decimal
mulInt i (Decimal val scl) =
    Decimal (val * i) scl


powInt : Decimal -> Int -> Decimal
powInt (Decimal val scl) n =
    Decimal (val ^ n) (scl * n)


flipPowInt : Int -> Decimal -> Decimal
flipPowInt n d =
    powInt d n



-- Rounding and division


type alias Precision =
    Maybe Int


type RoundingMode
    = RoundUp
    | RoundDown
    | RoundCeiling
    | RoundFloor
    | RoundHalfUp
    | RoundHalfDown
    | RoundHalfEven


{-| How many digits of precision in this Int
-}
precision : Decimal -> Int
precision (Decimal val _) =
    case val of
        0 ->
            1

        _ ->
            val |> Basics.abs |> Basics.toFloat |> Basics.logBase 10 |> Basics.floor |> (+) 1


{-| Division with maximum control of precision and rounding
-}
divide : RoundingMode -> Precision -> Decimal -> Decimal -> Decimal
divide rounding precision0 a0 b0 =
    -- based on http://hackage.haskell.org/package/HasBigDecimal-0.1.1/docs/src/Data.BigDecimal.html#divide
    let
        ( (Decimal valA scl) as a, (Decimal valB _) as b ) =
            matchScales a0 b0

        maxPrecision : Int
        maxPrecision =
            case precision0 of
                Just p ->
                    p

                Nothing ->
                    precision a + ((precision b |> Basics.toFloat |> (*) 10) / 3 |> Basics.round)
    in
    Decimal (divUsing rounding (valA * 10 ^ maxPrecision) valB) maxPrecision |> trim maxPrecision


{-| Default division: rounds up and does not limit precision
-}
div : Decimal -> Decimal -> Decimal
div =
    divide RoundHalfUp Nothing


{-| Sign of the number. -1 for negatives, 1 for positives, 0 for zero.

Based on <http://zvon.org/other/haskell/Outputprelude/signum_f.html>

-}
signum : Int -> Int
signum =
    Basics.clamp -1 1


{-| divide two matching-scale numbers and round
-}
divUsing : RoundingMode -> Int -> Int -> Int
divUsing rounding a b =
    -- based on http://hackage.haskell.org/package/HasBigDecimal-0.1.1/docs/src/Data.BigDecimal.html#divUsing
    let
        quot =
            a // b

        rem =
            a |> remainderBy b

        delta =
            (10 * Basics.abs rem // Basics.abs b) - 5
    in
    case rounding of
        RoundUp ->
            if Basics.abs rem > 0 then
                quot + signum quot

            else
                quot

        RoundCeiling ->
            if Basics.abs rem > 0 && quot >= 0 then
                quot + 1

            else
                quot

        RoundHalfUp ->
            if delta >= 0 then
                quot + signum quot

            else
                quot

        RoundHalfDown ->
            if delta <= 0 then
                quot

            else
                quot + signum quot

        RoundDown ->
            quot

        RoundFloor ->
            if quot >= 0 then
                quot

            else
                quot + 1

        RoundHalfEven ->
            if delta > 0 || (delta == 0 && remainderBy quot 2 == 1) then
                quot + signum quot

            else
                quot


roundTo : RoundingMode -> Int -> Decimal -> Decimal
roundTo rounding precision_ ((Decimal val scl) as d) =
    if precision_ < 0 || precision_ >= scl then
        d

    else
        Decimal (divUsing rounding val <| 10 ^ (scl - precision_)) precision_


roundN : Int -> Decimal -> Decimal
roundN =
    roundTo RoundHalfUp


round : Decimal -> Decimal
round =
    roundN 0


floorN : Int -> Decimal -> Decimal
floorN =
    roundTo RoundFloor


floor : Decimal -> Decimal
floor =
    floorN 0


ceilN : Int -> Decimal -> Decimal
ceilN =
    roundTo RoundCeiling


ceil : Decimal -> Decimal
ceil =
    ceilN 0


{-| remove trailing zeros from a Decimal's val by decreasing the scale
-}
trim : Int -> Decimal -> Decimal
trim prefScl ((Decimal val0 scl0) as d) =
    -- based on http://hackage.haskell.org/package/HasBigDecimal-0.1.1/docs/src/Data.BigDecimal.html#trim
    let
        val =
            val0 // 10

        rem =
            val0 |> remainderBy 10
    in
    if rem == 0 && 0 <= prefScl && prefScl < scl0 then
        trim prefScl (Decimal val <| scl0 - 1)

    else
        d


normalize : Decimal -> Decimal
normalize =
    trim 0



-- conversion


toFloat : Decimal -> Maybe Float
toFloat (Decimal val scl) =
    let
        f : Float
        f =
            Basics.toFloat val * (Basics.toFloat base ^ Basics.toFloat -scl)
    in
    if Basics.isInfinite f then
        Nothing

    else
        Just f


intExp : Int -> Int
intExp =
    Basics.abs >> Basics.toFloat >> Basics.logBase baseF >> Basics.floor


toString : Decimal -> String
toString ((Decimal val scl) as d) =
    case toFloat d of
        Nothing ->
            -- Very large numbers are stringified with e-notation
            toEString d

        Just f ->
            if Basics.abs scl + intExp val > 21 then
                -- Moderately large numbers, too
                toEString d

            else
                -- Smallish numbers can be stringified as floats
                String.fromFloat f


{-| toString guaranteeing the e-notation, like `1.23e+10`
-}
toEString : Decimal -> String
toEString (Decimal val scl) =
    let
        strScl =
            -scl + intExp val

        strVal0 =
            String.fromInt val

        msb =
            if val < 0 then
                2

            else
                1

        strVal =
            String.left msb strVal0
                ++ (if String.length strVal0 > msb then
                        "." ++ String.dropLeft msb strVal0

                    else
                        ""
                   )

        eSign =
            if strScl > 0 then
                "+"

            else
                -- stringifying strScl will add the negative sign naturally
                ""
    in
    strVal ++ "e" ++ eSign ++ String.fromInt strScl


fromString : String -> Maybe Decimal
fromString s =
    case s |> String.split "e" of
        [ valdec, strExp ] ->
            Maybe.map2 (\exp ( dexp, val ) -> Decimal val (exp - dexp))
                (String.toInt strExp |> Maybe.map ((*) -1))
                (valFromString valdec)

        [ valdec ] ->
            Maybe.map (\( dexp, val ) -> Decimal val -dexp)
                (valFromString valdec)

        _ ->
            -- more than 1 `e` is bogus
            Nothing


valFromString : String -> Maybe ( Int, Int )
valFromString s =
    case s |> String.split "." of
        [ whole, dec ] ->
            String.toInt (whole ++ dec) |> Maybe.map (Tuple.pair <| -1 * String.length dec)

        [ whole ] ->
            String.toInt whole |> Maybe.map (Tuple.pair 0)

        _ ->
            Nothing


fromFloatN : Int -> Float -> Decimal
fromFloatN sigfigs f =
    let
        scl =
            sigfigs - floatExp f

        val =
            (f * baseF ^ Basics.toFloat scl) |> Basics.floor
    in
    Decimal val scl


floatExp : Float -> Int
floatExp f =
    if f == 0 then
        0

    else
        f |> Basics.abs |> Basics.logBase 10 |> Basics.floor |> (+) 1


fromFloat10 : Float -> Decimal
fromFloat10 =
    fromFloatN 10


{-| Implies `not isInfinite` and `not isFinite`
-}
isNaN : Decimal -> Bool
isNaN (Decimal val scl) =
    -- the (isNaN (toFloat exp)) is necessary, surprisingly.
    -- 0 // 0 --> 0
    -- ...but:
    -- (0 / 0 |> floor) --> NaN : Int
    Basics.isNaN (Basics.toFloat val) || Basics.isNaN (Basics.toFloat scl)


{-| Implies `not isNaN` and `not isFinite`
-}
isInfinite : Decimal -> Bool
isInfinite ((Decimal val scl) as d) =
    -- the (isInfinite (toFloat exp)) is necessary, surprisingly.
    -- 1 // 0 --> 0
    -- ...but:
    -- (1 / 0 |> floor) --> Infinite : Int
    not (isNaN d) && (infiniteInt == val || infiniteInt == scl)


{-| Implies `not isNaN` and `not isInfinite`

This is usually what we want

-}
isFinite : Decimal -> Bool
isFinite =
    isInfinite >> not


zero : Decimal
zero =
    Decimal 0 0


one : Decimal
one =
    Decimal 0 1


negativeOne : Decimal
negativeOne =
    Decimal 0 -1


ten : Decimal
ten =
    Decimal 1 1
