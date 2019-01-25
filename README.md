# elm-decimal

Arbitrary-precision decimals for Elm of the form `significand * 10 ^ exponent`

## related work

* https://github.com/MikeMcl/decimal.js - Mine's based on this one, though it's certainly not complete enough for me to call it a "port" of decimal.js.
* https://github.com/prikhi/decimal - same idea, BigDecimals in Elm. I wrote my library before I found this one. It's been around much longer; you're probably better off trying it first. It's missing decimal/decimal division and a power function, so I'll keep maintaining mine.
