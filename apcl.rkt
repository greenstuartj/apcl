#lang typed/racket
(require "src/types.rkt")
(require "src/ast.rkt")
(require "src/environment.rkt")
(require "src/eval.rkt")
(require "src/interpreter.rkt")

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
