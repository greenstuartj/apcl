#lang racket
(require "framework/framework.rkt")

(Tests "FEATURE"

 (Name "rationals"
       (Given "")
       (When "(3/10) = (1/10) + 2/10")
       (Then "true"))

 (Name "lambda"
       (Given "")
       (When "(\\arg: arg+1) 10")
       (Then "11"))

 (Name "if true"
       (Given "")
       (When "if true then 1 else 0")
       (Then "1"))

 (Name "if false"
       (Given "")
       (When "if false then 1 else 0")
       (Then "0"))

 (Name "if expr"
       (Given "")
       (When "if 10=5*2 then 1 else 0")
       (Then "1"))

 (Name "let"
       (Given "")
       (When "let x: 10 in x+x")
       (Then "20"))

 (Name "variable"
       (Given
        "
        x: 10
        ")
       (When "x")
       (Then "10"))

 (Name "adding variables"
       (Given
        "
        x: 10;
        y: 20;
        ")
       (When "x+y")
       (Then "30"))

 (Name "function"
       (Given
        "
        f x: x+1;
        ")
       (When "f 1")
       (Then "2"))

 (Name "curried function map"
       (Given
        "
        add x y: x+y;
        ")
       (When "(map add 3) iota 3")
       (Then "[3,4,5]"))

 (Name "partial binop map"
       (Given "")
       (When "(map (+3)) iota 3")
       (Then "[3,4,5]"))

 (Name "inputs default to none"
       (Given
        "
        input x;
        ")
       (When "x")
       (Then "none"))

 (Name "set inputs"
       (Given
        "
        input x;
        ")
       (When "x: 10"
             "x")
       (Then "10"))

 (Name "changes propagate to dependencies"
       (Given
        "
        input x;
        y: x
        ")
       (When "x: 10"
             "(x = y) and y != none")
       (Then "true"))

 (Name "add inputs"
       (Given
        "
        input x;
        input y;
        result: x+y;
        ")
       (When "x: 10"
             "y: 20"
             "result")
       (Then "30"))

 (Name "get vector"
       (Given
        "
        x: [1,2,3];
        ")
       (When "x->1")
       (Then "2"))

 (Name "set vector"
       (Given
        "
        x: [1,2,3];
        ")
       (When "x->1<-10"
             "x")
       (Then "[1,10,3]"))

 (Name "set nested vector"
       (Given
        "
        x: [[1,2,3], [4,5,6]];
        ")
       (When "x->1->0<-10"
             "x")
       (Then "[[1,2,3],[10,5,6]]"))

 (Name "get module"
       (Given
        "
        m: {
          x: 10;
        };
        ")
       (When "m->x")
       (Then "10"))

 (Name "set module"
       (Given
        "
        m: {
          input x;
        };
        ")
       (When "m->x<-10"
             "m->x")
       (Then "10"))

 (Name "module calculation"
       (Given
        "
        m: {
          input x;
          m: 2;
          result: x/m;
        };
        ")
       (When "m->x<-10"
             "m->result")
       (Then "5"))

 (Name "new module"
       (Given
        "
        m: {
          input n;
          result: n*2;
        };
        ")
       (When "m2: new m;"
             "m2->n<-10"
             "m->n<-2"
             "[m2->result, m->result]")
       (Then "[20, 4]"))
       
 )


(Tests "BUILTIN"

 (Name "addition"
       (Given "")
       (When "10+2")
       (Then "12"))

 (Name "iota"
       (Given "")
       (When "iota 4")
       (Then "[0,1,2,3]"))

 (Name "string concat"
       (Given "")
       (When "'hello' & '!'")
       (Then "'hello!'"))

 (Name "vector concat"
       (Given "")
       (When "[1,2,3] & [4,5,6]")
       (Then "[1,2,3,4,5,6]"))

 )
 

(Tests "ERROR"
       
 (Name "set non-inputs"
       (Given
        "
        x: 1;
        ")
       (When "x: 10")
       (Then "[ERROR] definition of 'x' already exists as non-input"))

  )
