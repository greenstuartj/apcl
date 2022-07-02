# APCL

##Adaptive Parameter Calculator Language

An incremental functional language designed for exploring parameter tweaking in calculations

## Installation

Install [racket](https://racket-lang.org/)

Compile with `raco exe apcl.rkt`

Run tests with `racket tests/test.rkt`

Run with `./apcl`

## Example Use
```
    input x;
    input y;
    input z;
    result: (x*y)+z;
    x: 10;
    y: 2;
    z: 5;
    result
25
    y: 3;
35
    factorial n: (reduce (*)) (map (+1)) iota n;
    z: factorial 5;
    result
150
    result/4
37.5
```
