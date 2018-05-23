---
title: Modulus and Remainder
tags: math, numerics
---
behave differently 

for any two real numbers D (dividend) and d (divisor) where d != 0, there exists a pair of numbers
q (quotient) and r (remainder) that satisy the following conditions

	1. q is an integer
	2. The division rule `D = d * q + r `
	3. `|r| < |d|`
	
`q = D div d`

`r = D mod d`

## Modulo as Euclidean division

```
 7 modulo  3 ->  1
 7 modulo -3 ->  1
-7 modulo  3 ->  2
-7 modulo -3 ->  2
```

remainder
```
 7 %  3 ->  1
 7 % -3 ->  1
-7 %  3 -> -1
-7 % -3 -> -1
```


```
int modulo_Euclidean(int a, int b) {
	int m = a % b;
	if (m < 0) {
		m = (b < 0) ? m - b : m + b;
	}
	return m;
}
```

-21 mod 4 is 3 because -21 + 4 x 6 is 3.

But -21 divided by 4 gives -5 with a remainder of -1.


Integer mod, mod definition
`(x div y) * y + (x mod y) == x`

quot 
integer division truncated toward zero
`(x quot y) * y + (x rem y) == x`

rem
integer remainder

div
integer division truncated toward negative infinity

```
n `quot` d          =  q  where (q,_) = quotRem n d
    n `rem` d           =  r  where (_,r) = quotRem n d
    n `div` d           =  q  where (q,_) = divMod n d
    n `mod` d           =  r  where (_,r) = divMod n d

    divMod n d          =  if signum r == negate (signum d) then (q-1, r+d) else qr
                           where qr@(q,r) = quotRem n d
```

```
5 `mod` 3 == 2
5 `rem` 3 == 2

5 `mod` (-3) == -1
5 `rem` (-3) == 2

(-5) `mod` 3 == 1
(-5) `rem` 3 == -2

(-5) `mod` (-3) == -2
(-5) `rem` (-3) == -2
```

The difference is clear when the second parameter is a negative number and the result is not zero.

[Division and Modulus for Computer Scientists by Daan Leijen](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf)
[Haskell Integral](http://hackage.haskell.org/package/base-4.11.1.0/docs/Prelude.html#t:Integral)
[Euclidean division](https://en.wikipedia.org/wiki/Euclidean_division)
