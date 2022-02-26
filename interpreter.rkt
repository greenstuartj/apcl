#lang typed/racket
(require "either.rkt")
(require "lexer.rkt")
(require "types.rkt")
(require "ast.rkt")
(require "parser.rkt")
(require "eval.rkt")
(require "environment.rkt")
(require "statements.rkt")
(provide make-environment make-top-level interpret repl)

(: make-environment (-> (Environment Type)))
(define (make-environment)
  (Environment
   (ann (make-hash)
        (Mutable-HashTable String True))
   (ann (make-hash)
        (Mutable-HashTable String (Def Type)))
   (ann (make-hash)
        (Mutable-HashTable String (Setof String)))))

(: make-top-level (-> String (Environment Type) (Either True String)))
(define (make-top-level source environment)
  (match (lex (string->list source) 1)
    [(Fail x) (Fail x)]
    [(Ok x) (build-top-level (parse-top-level x) environment)]))

(: interpret (-> String (Environment Type) (Either String String)))
(define (interpret source environment)
  (match (lex (string->list source) 1)
    [(Fail x) (Fail x)]
    [(Ok x) (match (parse-statement x)
              [(Ok s) (match (add-statement s environment)
                        [(Fail x) (Fail x)]
                        [_ (Ok "")])]
              [(Fail _) (match (parse x)
                          [(Fail x) (Fail x)]
                          [(Ok x) (match (eval-ast x (hash) environment)
                                    [(Fail x) (Fail x)]
                                    [(Ok ast)
                                     (Ok (string-append ((show-ast show-type) ast)
                                                        "\n"))])])])]))

(: repl (-> (Environment Type) Void))
(define (repl environment)
  (let loop ()
    (display "   ")
    (let ([line (read-line)])
      (if (eof-object? line)
          (display "")
          (let ([result (interpret line environment)])
            (match result
              [(Fail x) (displayln x)
                        (loop)]
              [(Ok x) (display x)
                      (loop)]))))))
