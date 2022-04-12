#lang racket
(require "framework/framework.rkt")

(Tests

 (Name "addition"
       (Given "")
       (When "10+2")
       (Then "12"))

 (Name "iota"
       (Given "")
       (When "iota 4")
       (Then "[0, 1, 2, 3]"))

 (Name "rationals"
       (Given "")
       (When "(3/10) = (1/10) + 2/10")
       (Then "true"))

 (Name "lambda"
       (Given "")
       (When "(\\arg: arg+1) 10")
       (Then "11"))

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
 )
