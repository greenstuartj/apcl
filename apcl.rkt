#lang typed/racket
(require "types.rkt")
(require "ast.rkt")
(require "environment.rkt")
(require "eval.rkt")
(require "interpreter.rkt")

(: main (-> Void))
(define (main)
  (let ([source : String
                (if (> (vector-length (current-command-line-arguments)) 0)
                    (file->string (vector-ref (current-command-line-arguments) 0))
                    "")]
        [env : (Environment Type (AST Type))
             (make-environment)])
    (make-top-level source env)
    (repl env)))

(main)
