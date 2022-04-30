#lang racket
(require "../../src/types.rkt")
(require "../../src/ast.rkt")
(require "../../src/either.rkt")
(require "../../src/eval.rkt")
(require "../../src/interpreter.rkt")
(provide Tests Name Given When Then)

(define (Tests . results)
  (let ([fails (filter string? results)])
    (if (null? fails)
        (displayln "ALL TESTS PASSING")
        (display (string-join fails)))))

(define (Then expected)
  (lambda (name)
    (lambda (env result)
      (let ([eval-expected (match (interpret expected env)
                               [(Ok x) x]
                               [(Fail x) x])])
        (let ([trimmed-result (string-trim result)]
              [trimmed-expected (string-trim eval-expected)])
          (if (equal? trimmed-result trimmed-expected)
              #f
              (format "
--- TEST FAILED ---
TEST NAME: ~a
EXPECTED:  ~a
ACTUAL:    ~a
-------------------
" name trimmed-expected trimmed-result)))))))

(define (When . inputs)
  (lambda (env)
    (let loop ([in inputs]
               [out ""])
      (cond
        [(null? in) out]
        [else (loop (cdr in)
                    (match (interpret (car in) env)
                      [(Ok x) x]
                      [(Fail x) x]))]))))

(define (Given source-code)
  (let ([env (make-environment)])
    (make-top-level source-code env)
    (lambda (when-f then-f)
      (then-f env (when-f env)))))

(define (Name name given-f when-f then-f)
    (given-f when-f (then-f name)))
