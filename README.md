# APCL

## Adaptive Parameter Calculator Language

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

## Types and Expressions

### Identifiers
Identifiers are names given to values or functions

Identifiers marked as input can be changed
```
    x: 10;
    x
10
    f n: n+1;
    f x
11
    x: 11
[ERROR] definition of 'x' already exists as non-input
    input y;
    y: 10
    y
10
    y: 11
    y
11
```

### Numbers
Numbers can be whole numbers or decimals
```
    10
10
    4.6
4.6
```

### Booleans
true and false
```
   true
true
   false
false
```

### Strings
Strings are collections of characters within single quotes
```
    'this is a string'
this is a string
```

### Vectors
Vectors are collections of values withing square brackets
```
    [1,2,3,4,5]
[1, 2, 3, 4, 5]
```

### None
None represents no value, or a failure

It propogates itself in binary expressions
```
    none
none
    none+1
none
```

### Option
Option represents a value that doesnt need to be used

It removes itself from binary expressions
```
    option
option
    option+1
1
```

### If
If expressions can be used for branching

false and none lead to the else branch
```
    if true then 1 else 0
1
    if false then 1 else 0
0
    if 10>3 then 'bigger' else 'smaller'
bigger
```

### Lambda
Lambda functions are anonymous functions if naming isnt needed
```
    named_function n: n+1;
    named_function 10
11
    (\n: n+1) 10
11
    (\n m: n*m) 10 2
20
```

### Let
Let expressions can be used to storing intermediate values for convenience
```
    let m: 10+2 in m*m
144
```

### Modules (Experimental)
Modules can be used for grouping code together

Currently not finished
```
    m: { input n; result: n*2; };
    m->n<-10
    m->result
20
```

## Symbol Functions

### Add +
```
    1+2
3
```

### Minus -
```
    10-3
7
```

### Multiply *
```
    10*2
20
```

### Divide /
```
    10/2
5
```

### Power ^
```
    10^2
100
```

### Modulo %
```
    10%2
0
```

### Minimum <.
```
    10 <. 4
4
```

### Maximum >.
```
    10 >.4
10
```

### Equal =
```
    6=6
true
    5=6
false
```

### Not Equal !=
```
    6!=6
false
    5!=6
true
```

### Less Than <
```
    10<2
false
```

### Less Than or Equal <=
```
    10<=2
false
```

### Greater Than >
```
    10>2
true
```

### Greater Than or Equal >=
```
    10>=2
true
```

### Or or
```
    true or false
true
    false or false
false
```

### And and
```
    true and true
true
    true and false
false
```

### Concatenate &
```
    [1, 2, 3] & [4, 5, 6]
[1, 2, 3, 4, 5, 6]
    'hello' & 'world'
helloworld
```

### Push Front &>
```
    1 &> [2]
[1, 2]
```

### Push Front and Enlist &>>
```
    1 &>> 2
[1, 2]
```

### Push Back <&
```
    [1] <& 2
[1, 2]
```

### Push Back and Enlist <<&
```
    1 <<& 2
[1, 2]
```

### Compose @
```
    ((+1) @ (+2)) 10
13
    (sum @ iota) 101
5050
```

### Negate ~
```
    ~10
-10
```

### Abs |
```
    |10
10
    |~10
10
```

### Not !
```
    !true
false
    !false
true
```

## Named Functions

### optional
Turns none into option

Useful for when expressing that a parameter doesnt have to be used in an equation
```
    input important;
    input unimportant;
    equation: important + optional unimportant;
    equation
none
    important: 10
    equation
10
    unimportant: 5
    equation
5
```

### get
gets an index or a vector or a string, starting from index 0

get can use a vector of indicies
```
    get 0 ['hello', 'world']
hello
    get 1 ['hello', 'world']
world
    get 2 ['hello', 'world']
none
    get [1,0] ['hello', 'world']
[world, hello]
```

### index
index finds all positions of an element in a vector or string

index can use a vector or string of elements
```
    index 'vector' ['here', 'is', 'a', 'vector', 'of', 'strings']
[3]
    index 'l' 'hello'
[2, 3]
    index ['o', 'e'] 'hello'
[[4], [1]]
    index 'oe' 'hello'
[[4], [1]]
```

### iota
iota generates a vector of contiguous integers from 0 upto (but not including) its argument
```
    iota 10
[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    iota 3
[0, 1, 2]
```

### map
map takes a function and applied it to each element of a vector or string
```
    (map (\n: n*2)) iota 5
[0, 2, 4, 6, 8]
    (map (*2)) iota 5
[0, 2, 4, 6, 8]
    (map id) 'hello'
[h, e, l, l, o]
```

### filter
filter takes a function and a vector or string and removes each element when the function applied return false or none
```
    (filter (>5)) iota 10
[6, 7, 8, 9]
```

### reduce
reduce uses a function to collapse a vector of values into a single value
```
    (reduce (*)) [1,2,3,4,5]
120
    product: reduce (*);
    product [1,2,3,4,5]
120
```

### reverse
reverse reverses a vector or string
```
    reverse iota 5
[4, 3, 2, 1, 0]
    reverse 'hello'
olleh
```

### drop
drop removes the first n elements from a vector or string
```
    drop 2 [1, 2, 3, 4, 5]
[3, 4, 5]
    drop 3 'hello'
lo
```

### take
take takes the first n elements from a vector or string
```
    take 2 [1, 2, 3, 4, 5]
[1, 2]
    take 3 'hello'
hel
```

### length
length gets the number of elements in a vector or string
```
    length [1,2,3]
3
    length 'hello'
5
```

### replicate
replicate takes 2 arguments

1. a vector or integers or bools
2. a vector or string the same length as the vector in the first argument

replicate then replicates each element in the second argument by the number in the first vector (or by one if true, and by zero if false)

when combined with map, this can be used for filtering
```
    replicate [1,2,3] ['a', 'b', 'c']
[a, b, b, c, c, c]
    replicate [true, false, true, false] [1,2,3,4]
[1, 3]
    is_even n: (n%2)=0;
    let numbers: iota 10 in replicate (map is_even) numbers numbers
[0, 2, 4, 6, 8]
    my_filter f v: replicate (map f) v v
    (my_filter is_even) iota 10
[0, 2, 4, 6, 8]
```

### scan
scan works like reduce, but creates a vector of all its intermediary steps
```
    (scan (+)) iota 10
[0, 1, 3, 6, 10, 15, 21, 28, 36, 45]
    (reduce (+)) iota 10
45
    cumulative_sum: scan (+);
    cumulative_sum [1,2,3]
[1, 3, 6]
```

### scan_n
scan_n takes an integer representing the size of a sliding window to reduce over a vector
```
    (scan_n (+)) 2 [1,2,3]
[3, 5]
    moving_average n v: (map (/n)) (scan_n (+)) n v;
    moving_average 3 [2,6,4,8,24,16]
[4, 6, 12, 16]
```

### zip
zip takes two vectors and combines them linearly into a vector or two elements, representing their pairs
```
    zip [1,2,3] [10,20,30]
[[1, 10], [2, 20], [3, 30]]
```

### zip_with
zip_with is similar to zip, but instead of combining their pairs into vectors, it uses a function to combine the pairs
```
    (zip_with (+)) [1,2,3] [10,20,30]
[11, 22, 33]
```

### member
### repeat
### floor
### ceiling
### seq
### while
### rotate
### string_to_vector
### catalogue
### catalogue_with
### string
### transpose
### show_table
### string_to_number
### string_split
### unique
### id
### const
### amend
### amend_with
### intersection
### reflex
### void
### slice
### random
### roll
### sum
### concat
### round
### flip
### join
### read_table
### read_dsv
### read_lines
### new
### type_of
### is_number
### is_bool
### is_string
### is_vector
### is_none
### is_option
### is_lambda
### is_builtin
### is_binop
### is_module