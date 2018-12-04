module Decimal exposing
    ( Decimal(..), fromFloat, fromSigExp, fromString, toString
    , add, sub, mul, div, neg, powFloat, logBase, log10
    , compare, gt, gte, lt, lte, min, max
    , flipSub, flipDiv
    , flipDivFloat, flipPowFloat, mulFloat
    )

{-| Limited-precision large-range reasonably-fast floating-point numbers.


# Construction and strings

@docs Decimal, fromFloat, fromSigExp, fromString, toString


# Math

@docs add, sub, mul, div, neg, powFloat, logBase, log10


# Comparison

No special equality functions - use Elm's builtin `==` and `/=` .

@docs compare, gt, gte, lt, lte, min, max


# Pipelined math

@docs flipSub, flipDiv


# Interacting with floats

@docs flipDivFloat, flipPowFloat, mulFloat

-}


type Decimal
    = Decimal
        -- significand. The value of a Decimal is `sig * (10 ^ exp)`.
        { sig : Float

        -- exponent, in base 10.
        -- This limits the range of values we can represent, since Int has a min and max value.
        -- We could expand this range by using a BigInt here instead, like
        -- https://github.com/hickscorp/elm-bigint, or a custom (List Int)-based
        -- BigInt implementation.
        -- (We can't nest Decimals here - exponent precision is too important,
        -- and Decimal does not have that precision.)
        , exp : Int
        }


fromSigExp : Float -> Int -> Decimal
fromSigExp sig exp =
    -- no exponent for zero
    if sig == 0 then
        Decimal { sig = sig, exp = 0 }

    else
        let
            -- normalize significand and exponent
            dExp : Int
            dExp =
                sig |> abs |> Basics.logBase baseFloat |> floor
        in
        Decimal
            { sig = sigTimesExp sig dExp
            , exp = exp + dExp
            }


fromString : String -> Maybe Decimal
fromString str =
    case str |> String.split "e" of
        [ sig, exp ] ->
            Maybe.map2 fromSigExp (String.toFloat sig) (String.toInt exp)

        _ ->
            str |> String.toFloat |> Maybe.map fromFloat


fromFloat : Float -> Decimal
fromFloat sig =
    fromSigExp sig 0


base : Int
base =
    10


baseFloat =
    base |> toFloat


significand : Decimal -> Float
significand (Decimal { sig }) =
    sig


exponent : Decimal -> Int
exponent (Decimal { exp }) =
    exp


infiniteInt : Int
infiniteInt =
    1 / 0 |> floor


toString : Decimal -> String
toString (Decimal { sig, exp }) =
    if abs exp < 21 || exp == infiniteInt || isInfinite sig || isNaN sig then
        sig * toFloat (base ^ exp) |> String.fromFloat

    else
        String.fromFloat sig ++ "e" ++ String.fromInt exp


{-|

    Return this decimal's significand converted to the given exponent.

    sigOfExp (fromSigExp 3 10) 9 --> 30
    sigOfExp (fromSigExp 3 10) 11 --> 0.3

-}
sigOfExp : Decimal -> Int -> Float
sigOfExp (Decimal { sig, exp }) targetExp =
    targetExp - exp |> sigTimesExp sig


sigTimesExp : Float -> Int -> Float
sigTimesExp sig dExp =
    if dExp == 0 || isInfinite sig then
        -- dExp == 0: it looks like the result should be identical, but js does
        -- strange things with float precision, so let's be sure.
        --
        -- isInfinite sig: this check avoids NaN when dExp is also infinite
        sig

    else
        sig / toFloat (base ^ dExp)


compare : Decimal -> Decimal -> Order
compare a b =
    let
        (Decimal { sig }) =
            sub a b
    in
    if sig == 0 then
        EQ

    else if sig > 0 then
        GT

    else
        LT


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


add : Decimal -> Decimal -> Decimal
add ((Decimal a) as da) ((Decimal b) as db) =
    let
        exp =
            Basics.max a.exp b.exp
    in
    fromSigExp (sigOfExp da exp + sigOfExp db exp) exp


neg : Decimal -> Decimal
neg (Decimal a) =
    Decimal { a | sig = -a.sig }


sub : Decimal -> Decimal -> Decimal
sub a b =
    add a (neg b)


{-| Pipeline-friendly subtraction

a - b == sub a b == flipSub b a == a |> flipSub b

-}
flipSub : Decimal -> Decimal -> Decimal
flipSub a b =
    sub b a


mul : Decimal -> Decimal -> Decimal
mul (Decimal a) (Decimal b) =
    fromSigExp (a.sig * b.sig) (a.exp + b.exp)


mulFloat : Float -> Decimal -> Decimal
mulFloat f (Decimal { sig, exp }) =
    fromSigExp (sig * f) exp


div : Decimal -> Decimal -> Decimal
div (Decimal a) (Decimal b) =
    fromSigExp (a.sig / b.sig) (a.exp - b.exp)


{-| Pipeline-friendly division

a / b == div a b == flipDiv b a == a |> flipDiv b

-}
flipDiv : Decimal -> Decimal -> Decimal
flipDiv a b =
    div b a


flipDivFloat : Float -> Decimal -> Decimal
flipDivFloat f (Decimal { sig, exp }) =
    fromSigExp (sig / f) exp


log10 : Decimal -> Float
log10 (Decimal { sig, exp }) =
    Basics.logBase baseFloat sig + toFloat exp


ln10 =
    Basics.logBase e 10


logBase : Float -> Decimal -> Float
logBase base_ val =
    log10 val * ln10 / Basics.logBase e base_


exp10 : Float -> Decimal
exp10 val =
    let
        log =
            Basics.logBase baseFloat val

        exp =
            log |> floor
    in
    fromSigExp (10 ^ (log - toFloat exp)) exp


powFloat : Decimal -> Float -> Decimal
powFloat (Decimal { sig, exp }) toExp =
    -- cribbed from https://github.com/Patashu/break_infinity.js/blob/0ba078baca0ad1761103907a34c1268f0e5f6f53/break_infinity.js#L1186
    let
        expMul =
            toFloat exp * toExp

        newExp =
            truncate expMul

        residue =
            expMul - toFloat newExp

        newSig =
            if residue == 0 then
                sig ^ toExp

            else
                10 ^ (toExp * Basics.logBase 10 sig + residue)
    in
    fromSigExp newSig newExp


flipPowFloat : Float -> Decimal -> Decimal
flipPowFloat exp base_ =
    powFloat base_ exp