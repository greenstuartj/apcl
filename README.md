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

Source code can also be stored in files with a .apcl extension

These can be loaded into the interpreter by either running the executable with the file as argument

```
./apcl source.apcl
```

or it can be imported into the interpreter

```
./apcl
    import 'source.apcl'
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
Vectors are collections of values within square brackets
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
Turns none into option, and returns its argument for any other value

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
15
```

### get
gets an index or a vector or a string, starting from index 0

get can use a vector of indicies

negative indicies can be used to access the back of the vector
```
   get 0 ['hello', 'world']
hello
   get 1 ['hello', 'world']
world
   get 2 ['hello', 'world']
none
   get [1,0] ['hello', 'world']
[world, hello]
   get ~1 [1,2,3]
3
```

### slice
slice is similar to get but takes two integers representing indicies, and it takes all elements from a vector from the first index, upto but not including the second index

the first index must be larger than the second

if the index goes beyond the length of the original vector, none will be used
```
   slice 2 5 ['this', 'is', 'a', 'vector', 'of', 'strings']
[a, vector, of]
   slice 0 4 [1,2]
[1, 2, none, none]
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

### index_where
index_where is similar to index, but takes a function and returns a vector of indicies representing the positions of elements where the function returned true after application
```
   starts_with c s: c = get 0 s;
   (index_where (starts_with 'f')) ['france', 'england', 'finland', 'germany']
[0, 2]
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

### sum
sum takes a vector of numbers and adds them together
```
   sum [1,2,3]
6
   sum iota 10
45
```

### reverse
reverse reverses a vector or string
```
   reverse iota 5
[4, 3, 2, 1, 0]
   reverse 'hello'
olleh
```

### rotate
rotate shifts all elements in a vector or string by a number

negative numbers shift elements the other direction
```
   rotate 1 [1,2,3]
[2, 3, 1]
   rotate 2 [1,2,3]
[3, 1, 2]
   rotate ~1 [1,2,3]
[3, 1, 2]
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
checks if an element is contained within a vector, or if a substring is contained within a string
```
   member 2 [1,2,3]
true
   member 5 [1,2,3]
false
   member 'ab' 'abcde'
true
```

### floor
floor rounds down to the nearest whole number
```
   floor 4.3
4
```

### ceiling
ceiling rounds up to the nearest whole number
```
   ceiling 4.3
5
```

### round
round takes a number of significant digits and a decimal number to round to that many significant figures
```
   round 2 3.14159265359
3.14
   round 5 3.14159265359
3.14159
```

### repeat
repeat applies a function n times over a value

the example below applies the function double 5 times, over the initial value of 2
```
   double n: n*2;
   (repeat double) 5 2
64
```

### seq
seq is similar to repeat, but generates a vector with each step recorded

however, it generates a vector of n length, rather than applying the function n times
```
   double n: n*2;
   (seq double) 5 2
[2, 4, 8, 16, 32]
```

### while
while takes 2 functions and an initial value

the first function is applied to the value, and if the result if false or none, the while function stop

the second function is applied to the value on each iteration

in the example below, after successive doubling, the initial value eventually reaches above 1000 and the while function stops
```
   double n: n*2;
   continue n: n<1000;
   ((while continue) double) 2
1024
```

### string_to_vector
string_to_vector breaks a string into a vector of strings consisting of each character from the original string
```
   string_to_vector 'hello'
[h, e, l, l, o]
```

### catalogue
catalogue takes two vectors and creates a cartesian product of all possible pairs
```
   catalogue [1,2,3] [10,20,30]
[[[1, 10], [1, 20], [1, 30]], [[2, 10], [2, 20], [2, 30]], [[3, 10], [3, 20], [3, 30]]]
```

### catalogue_with
catalogue_with is similar to catalogue, but takes a function to combine the pairs instead of making a two element vector
```
   (catalogue_with (*)) [1,2,3] [10,20,30]
[[10, 20, 30], [20, 40, 60], [30, 60, 90]]
```

### show_table
show_table takes a vector of vectors of of equal length and produces a string spanning several lines with each field separated by a tab

doesnt always result in an easy to read table due to naive use of tabs
```
   show_table [[1,2,3], [4,5,6], [7,8,9]]
1	4	7
2	5	8
3	6	9
```

### transpose
transpose reflects a vector of vectors of equal length along its diagonal
```
   transpose [[1,2,3], [4,5,6], [7,8,9]]
[[1, 4, 7], [2, 5, 8], [3, 6, 9]]
   show_table [[1,2,3], [4,5,6], [7,8,9]]
1	4	7
2	5	8
3	6	9
   show_table transpose [[1,2,3], [4,5,6], [7,8,9]]
1	2	3
4	5	6
7	8	9
```

### string
string takes a value and returns its string representation
```
   10 & '!'
[ERROR] &: type mismatch
   (string 10) & '!'
10!
```

### string_to_number
string_to_number takes a string and attempts to convert it to a number

returns none if it cannot be converted
```
   '10'*2
[ERROR] *: non-numeric type
   (string_to_number '10')*2
20
   string_to_number '1x0'
none
```

### string_split
string_split splits a string on a substring and returns a vector of strings

the first argument is the substring used to split string from the second argument
```
   string_split '-' 'some-text-separated-by-hyphens'
[some, text, separated, by, hyphens]
   string_split 'll' 'hello'
[he, o]
```

### join
join takes joines either 2 strings or 2 vectors using an element to insert between them

the first argument is the element to insert between
```
   join ' ' 'hello' 'world'
hello world
   join '-' 'hello' 'world'
hello-world
   join 4 [1,2,3] [5,6,7]
[1, 2, 3, 4, 5, 6, 7]
   (reduce join '\n') string_split ' ' 'words in a sentence on different lines'
words
in
a
sentence
on
different
lines
```

### concat
concat takes a vector of vectors or a vector of strings and concatenates each element together
```
   concat [[1,2,3], [4,5,6], [7,8,9]]
[1, 2, 3, 4, 5, 6, 7, 8, 9]
   concat ['here', 'is', 'some', 'split', 'text']
hereissomesplittext
```

### unique
unique removes duplicate elements in a vector, or duplicate characters in a string
```
   unique 'hello'
helo
   unique [1,1,7,2,1,3,4,2,3,1,5]
[1, 7, 2, 3, 4, 5]
```

### intersection
intersection returns a vector of the common elements of 2 vectors, or the common characters of 2 strings

result will be the unique common elements, ordered by their appearance in the second argument
```
   intersection 'hello' 'world'
[o, l]
   intersection [1,2,3,4] [5,4,3,2]
[4, 3, 2]
```

### id
id returns its argument
```
   id 10
10
   id 'hello'
'hello'
```

### const
const takes 2 arguments and returns its first
```
   const 10 20
10
   const 'hello' 'world'
hello
   (map const 1) iota 5
[1, 1, 1, 1, 1]
```

### amend
amend changes changes an element in a vector, or character in a string, at the specified index
```
   amend 0 'hello' [1,2,3]
[hello, 2, 3]
   amend 1 'z' 'abc'
azc
```

### amend_with
amend_with is similar to amend, but it uses a function to change the existing value, rather than supplying a new value
```
   (amend_with (+10)) 0 [1,2,3]
[11, 2, 3]
   (amend_with (+10)) 2 [1,2,3]
[1, 2, 13]
   (amend_with (\c: if c='b' then 'z' else 'n')) 1 'abc'
azc
   (amend_with (\c: if c='b' then 'z' else 'n')) 2 'abc'
abn
```

### reflex
reflex takes a function that takes 2 arguments, and applies it to a single argument twice
```
   (reflex (+)) 10
20
   double: reflex (+);
   double 2
4
   square: reflex (*);
   square 6
36
```

### void
void takes and argument and returns nothing
```
   void 1


```

### random
random takes no arguments and returns a random number between 0 and 1
```
   random
0.9745691185142792
   random
0.06221802112212135
   random
0.7056680984746118
   random
0.4852263706100838
   random
0.031812800703817645
```
### roll
roll takes an integer and returns an integer between 0 (inclusive) and the integer (uninclusive)
```
   roll 6
5
   roll 6
3
   roll 6
0
   roll 6
1
```

### flip
flip takes a function of two arguments and reverses the order it takes arguments
```
   10 - 8
2
   (-) 10 8
2
   (flip (-)) 10 8
-2
```

### sort
sort takes a vector or string and returns a vector or string with all elements sorted in ascending order

the reverse function can be used to get the descending order
```
   sort [2,6,4,6,1]
[1, 2, 4, 6, 6]
   reverse sort [2,6,4,6,1]
[6, 6, 4, 2, 1]
   sort 'hello'
ehllo
```

### grade_up
grade up takes a vector or string and returns a vector of indicies representing the sorted ascending order of the vector or string

useful for using the sorted order of one vector to order another vector
```
   grade_up [2,6,4,6,1]
[4, 0, 2, 1, 3]
   get (grade_up [2,6,4,6,1]) [2,6,4,6,1]
[1, 2, 4, 6, 6]
   fastest animals speeds: reverse get (grade_up speeds) animals;
   fastest ['Swift', 'Falcon', 'Eagle'] [169, 389, 320] # km/h #
[Falcon, Eagle, Swift]
```

### grade_down
similar to grade_up but for descending order
```
   fastest animals speeds: get (grade_down speeds) animals;
   fastest ['Swift', 'Falcon', 'Eagle'] [169, 389, 320] # km/h #
[Falcon, Eagle, Swift]
```

## Table/File Functions
Tables are assumed to be a vector of equal length vectors where the subvectors are the columns and the first element is the column header

### read_table
read_table reads a csv file

it assumes the csv is comma separated and quoted with double quotes

anything that can be converted to a number will be converted

```
   read_table 'example.csv'
```

### read_dsv
read_dsv is similar to read_table, but the user can supply the separator and quote characters

anything that can be converted to a number will be converted
```
   read_dsv 'example.csv' ',' '\"'
```

### read_dsv_strings
similar to read_dsv but will keep each field as a string instead of converting to a number
```
   read_dsv_strings 'example.csv' ',' '\"'
```

### read_lines
read_lines reads each line of a file as a string
```
   read_lines 'example.txt'
```

## Module Functions

Experimental and unfinished

### new
new takes a module and returns a deep copy
```
   x: { input n; result: n*20; }
   y: new x;
   x->n<-10
   y->n<-20
   x->result
200
   y->result
400
```

## Type Checking Functions
```
   type_of 10
number
   is_number 10
true
   is_bool true
true
   is_string 'hello'
true
   is_vector [1,2,3]
true
   is_none none
true
   is_option option
true
   is_lambda (\n: n+1)
true
   is_builtin iota
true
   is_binop (+)
true
   m: { input n; }
   is_module m
true
```
