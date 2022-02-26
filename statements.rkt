#lang typed/racket
(require racket/match)
(require "either.rkt")
(require "lexer.rkt")
(require "types.rkt")
(require "ast.rkt")
(require "parser.rkt")
(require "environment.rkt")
(require "eval.rkt")
(provide parse-statement parse-top-level add-statement build-top-level)

(define-type Statement
  (U Input
     Definiton))

(struct Input
  ([name : String])
  #:transparent)

(struct Definiton
  ([name : String]
   [body : (AST Type)])
  #:transparent)

(: parse-statement (-> (Listof Token) (Either Statement String)))
(define (parse-statement tl)
  (match tl
    [`(,(Token 'input _ _) ,(Token 'identifier name _) . ())
     (Ok (Input name))]
    [`(,(Token 'input _ _) ,(Token 'identifier name _) ,(Token 'semicolon _ _) . ,d)
     (Ok (Input name))]
    [`(,(Token 'input _ line) . ,d)
     (Fail (string-append "[ERROR] malformed input on line " (format "~a" line)))]
    [`(,(Token 'identifier name line) . ,d)
     (match (parse-lambda d line)
       [(Fail x) (Fail x)]
       [(Ok x) (Ok (Definiton name x))])]
    [_ (Fail "[ERROR] invalid statement")]))
    
(: split-top-level (-> (Listof Token) (Listof (Listof Token))))
(define (split-top-level tl)
  (let loop ([t : (Listof Token) tl]
             [temp : (Listof Token) '()]
             [acc : (Listof (Listof Token)) '()])
    (match t
      ['()
       (if (null? temp)
           (reverse acc)
           (reverse (cons (reverse temp) acc)))]
      [`(,(Token 'semicolon _ _) . ,d)
       (if (null? temp)
           (loop d '() acc)
           (loop d '() (cons (reverse temp) acc)))]
      [`(,a . ,d)
       (loop d (cons a temp) acc)])))

(: parse-top-level (-> (Listof Token) (Listof (Either Statement String))))
(define (parse-top-level tl)
  (map parse-statement (split-top-level tl)))

(: add-statement (-> Statement (Environment Type)
                     (Either True String)))
(define (add-statement s e)
  (match (list s e)
    [(list (Input name) (Environment inputs defs depends))
     (hash-set! inputs name #t)
     (hash-set! defs
                name
                (ann (Def (Nil) #t (Unary (NoneT) (Nil)))
                     (Def Type)))
     (Ok #t)]
    [(list (Definiton name body) (Environment inputs defs depends))
     (cond
       [(and (not (hash-ref inputs name #f))
             (not (hash-ref defs name #f)))
        (hash-set! defs name (Def (Nil) #t body))
        (let loop ([deps : (Listof String) ((depends-ast depends-type) body)])
          (cond
            [(null? deps) (void)]
            [else (let ([known-deps : (U (Setof String) #f)
                                    (hash-ref depends (car deps) #f)])
                    (if known-deps
                        (hash-set! depends (car deps) (set-add known-deps name))
                        (hash-set! depends (car deps) (set name))))
                  (loop (cdr deps))]))
        (Ok #t)]
       [(and (not (hash-ref inputs name #f))
             (hash-ref defs name #f))
        (Fail "[ERROR] definition already exists as non-input")]
       [else
        (let ([result (eval-ast body (hash) (Environment inputs defs depends))])
          (match result
            [(Fail x) (Fail x)]
            [(Ok x)
             (hash-set! defs name (Def x #f x))
             (let loop ([keys (get-dependants name depends)])
               (cond
                 [(null? keys) (Ok #t)]
                 [else (set-Def-recalc! (hash-ref (Environment-defs e) (car keys)) #t)
                       (loop (cdr keys))]))]))])]))

(: build-top-level (-> (Listof (Either Statement String)) (Environment Type)
                       (Either True String)))
(define (build-top-level sl env)
  (let loop ([s : (Listof (Either Statement String)) sl])
    (match s
      ['() (Ok #t)]
      [`(,(Fail y) . ,d) (Fail y)]
      [`(,(Ok statement) . ,d)
       (match (add-statement statement env)
         [(Fail z) (Fail z)]
         [(Ok _) (loop d)])])))
