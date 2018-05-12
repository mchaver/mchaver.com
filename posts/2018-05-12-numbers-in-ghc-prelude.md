---
title: Numbers in GHC Prelude
tags: haskell, numeric
---

When using number literals in GHC, it is easy to run into type errors 
because of the various numeric types that exist in GHC. Prelude has six 
different number types.

## Number Types

- `Int`: 
    - a fixed-precision integer type with at least the range [-2^29 .. 2^29-1]
	- numeric type class instances: `Integral`, `Num`, `Real`

- `Integer`
    - arbitrary precision integer like bignum or bigint in other languages
	- numeric type class instances: `Integral`, `Num`, `Real`

- `Float`
    - single-precision floating point numbers
	- numeric type class instances: `Floating`, `Fractional`, `Num`, `Real`, `RealFloat`, `RealFrac`

- `Double`
    - double-precision floating point numbers
	- numeric type class instances: `Floating`, `Fractional`, `Num`, `Real`, `RealFloat`, `RealFrac`
	
- `Rational`
    - arbitrary precision rational number
	- type synonym for `Ratio Integer`
	- numeric type class instances: `Integral`, `Num`, `Real`

- `Word` 
    - unsigned number with the same size as `Int`
	- numeric type class instances: `Integral`, `Num`, `Real`

## Number Type Classes

The common arithmetic operators shared between the various number types
come from type classes in Prelude. Most of the binary operators require 
that both parameters be of the same type (`a -> a -> a`). If any of the 
parameters are of a different type, they must be manually converted to 
match the other type.

#### Num

The basic type class implemented by all of the Prelude number types.

- basic arithmetic except for division: `(+)`, `(-)`, `(*)` 

- sign related functions: `negate`, `abs`, `signum` 

- `fromInteger`: convert from `Integer` to another `Num` type

`signum` returns `-1` for negative, `0` for zero and `1` for positive.

#### Real

- `toRational`: convert a number type into `Rational` (`Ratio Integer`)

#### Integral

Provide integer division for integral (whole) numbers.

- `quot`: integer division with truncation towards zero. 
    - `quot 15 7` is `2`
	- `quot (-15) 7` is `-2`

- `div`: integer division with truncation towards negative infinity.
    - `div 15 7` is `2`
	- `div (-15) 7` is `-3`

- `rem`
    - `rem 7 3`  is `1` 
    - `rem 7 (-3)` is `1`
    - `rem (-7) 3` is `-1`
    - `rem (-7) (-3)`is `-1`

- `mod`
    - `mod 7 3`  is `1` 
    - `mod 7 (-3)` is `-2`
    - `mod (-7) 3` is `2`
    - `mod (-7) (-3)`is `-1`

- two operations returned as a tuple: `quotRem`, `divMod`

- `toInteger`: convert a numeric type to `Integer`

#### Fractional

Support real division.

- `(/)`: real division
- `fromRational`: convert `Rational` (`Ratio Integer`) to another number type

#### Floating

Trigonometric and hyperbolic functions.

- `pi`
- `exp`
- `log`
- `sqrt`
- `(**)`
- `logBase`
- `sin`, `cos`, `tan`
- `asin`, `acos`, `atan`
- `sinh`, `cosh`, `tanh`
- `asinh`, `acosh`, `atanh`

#### RealFrac

- `truncate`: round towards the nearest integer in the direction of zero

- `round`: round towards the nearest integer

- `ceiling`: round up

- `floor`: round down

#### RealFloat

A collection of functions related to the components of floating-point numbers: [Prelude.RealFloat](http://hackage.haskell.org/package/base-4.11.1.0/docs/Prelude.html#t:RealFloat).

## Numeric Functions

These functions are not part of a type classes, but they all have type class requirements.

- `Num` requirement: `subtract`, 

- `Integral` requirement: `even`, `odd`, `gcd`, `lcm`

- `Num` to `Integral`: `(^)` raise to a non-negative power

- `Fractional` to `Num`: `(^^)` raise to a negative power

## Other Numeric Modules in GHC base

- [Data.Int](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Int.html)

- [Data.Ratio](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Ratio.html)

- [Data.Word](http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Word.html)

- [Foreign.ForeignPtr](http://hackage.haskell.org/package/base-4.11.1.0/docs/Foreign-ForeignPtr.html)

- [Foreign.C.Types](http://hackage.haskell.org/package/base-4.11.1.0/docs/Foreign-C-Types.html)

- [System.Posix.Types](http://hackage.haskell.org/package/base-4.11.1.0/docs/System-Posix-Types.html)
