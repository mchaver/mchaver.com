---
title: Multidimensional Arrays in Rust
tags: rust, arrays
---


```
let x = [[1; 2], [3; 4], [5; 6]];
let y = [[1, 1], [5, 5], [10, 10]];
fn foo(x: [[u8; 2]; 3]) {
	println!("{:?}", x)
}
```


```
AFAIK type A = [T; 32]; type B = [A; 32] just means that the flat memory layout is:

    [T; 32] = T_0, ..., T_31

    [A; 32] = [T;32]_0, ..., [T; 32]_31

    = T_0_0, ... T_0_32, ... T_31_0, ..., T_31_31

so given let x: [A; 32] = [[..] ...];, then:

let y = x[3]; // y: [T; 32]

and

let z = x[1][2]; // z == T_1_2

```


```
No. By definition, arrays have a length defined at compile time. A variable (because it can vary) is not known at compile time. The compiler would not know how much space to allocate on the stack to provide storage for the array.
```

Vec allow dynamic



It would certainly make handling arrays much nicer. At the moment, if you want to implement a trait for arrays, you need to implement it specifically for each size. This is why the std crate only implements traits up to size 32.

If we had const integers, we could do something like this:

impl<T, N> MyTrait for [T; N] {}

And get that trait implemented for every sized array in one line.


https://github.com/rust-lang/rfcs/issues/1038


- [Tracking issue for const generics (RFC 2000)](https://github.com/rust-lang/rust/issues/44580)
- [ndarray](https://docs.rs/ndarray/0.11.2/ndarray/)
- [](https://www.reddit.com/r/rust/comments/3qsk2a/how_do_you_work_with_multi_dimensional_arrays/)
- [](https://users.rust-lang.org/t/solved-iteration-with-2d-array/15027) 
- [](https://crates.io/crates/init_with)
https://www.reddit.com/r/rust/comments/76olo3/why_rust_fails_hard_at_scientific_computing/
