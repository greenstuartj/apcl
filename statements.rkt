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
     Definition))

(struct Input
  ([name : String]
   [validation-vector : (Mutable-Vectorof (AST Type))])
  #:transparent)

(struct Definition
  ([name : String]
   [body : (AST Type)])
  #:transparent)

(: parse-statement (-> (Listof Token) (Either Statement String)))
(define (parse-statement tl)
  (match tl
    [`(,(Token 'input _ _) ,(Token 'identifier name _) . ())
     (Ok (Input name (vector)))]
    [`(,(Token 'input _ _) ,(Token 'identifier name _) ,(Token 'semicolon _ _) . ,d)
     (Ok (Input name (vector)))]
    [`(,(Token 'input _ _) ,(Token 'identifier name _) ,(Token 'colon _ _)
                           ,(Token 'open-square s l) . ,d)
     (match (parse (cons (Token 'open-square s l) d))
       [(Fail x) (Fail x)]
       [(Ok (Unary (LiteralVectorT vv) (Nil)))
        (Ok (Input name vv))]
       [(Ok (Unary (LiteralVectorT _) _))
        (Fail (string-append "[ERROR] invalid expression after validation vector on line "
                             (format "~a" l)))]
       [(Ok _)
        (Fail (string-append "[ERROR] invalid validation vector on line "
                             (format "~a" l)))])]
    [`(,(Token 'input _ line) . ,d)
     (Fail (string-append "[ERROR] malformed input on line " (format "~a" line)))]
    [`(,(Token 'identifier name line) . ,d)
     (match (parse-lambda d line)
       [(Fail x) (Fail x)]
       [(Ok x) (Ok (Definition name x))])]
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

(: check-input-validity (-> Statement (Environment Type (AST Type))
                            (Either True String)))
(define (check-input-validity def env)
  (match (list def env)
    [(list (Definition name (Unary (NoneT) (Nil))) _)
     (Ok #t)]
    [(list (Definition name body) (Environment inputs _ _))
     (let [(vv (hash-ref inputs name))]
       (let loop ([i : Integer 0])
         (cond
           [(>= i (vector-length vv)) (Ok #t)]
           [else (match (eval-ast (append-ast (vector-ref vv i) body)
                                  (hash)
                                  env)
                   [(Fail x)
                    (Fail (string-append "[ERROR] validation of "
                                         name
                                         " failed at validation vector index "
                                         (format "~a" i)
                                         " with error {" x "}"))]
                   [(Ok (Unary (BoolT #f) _))
                    (Fail (string-append "[ERROR] validation of "
                                         name
                                         " failed at validation vector index "
                                         (format "~a" i)))]
                   [(Ok _)
                    (loop (add1 i))])])))]))

(: add-statement (-> Statement (Environment Type (AST Type))
                     (Either True String)))
(define (add-statement s e)
  (: ast-is-function? (-> (AST Type) Boolean))
  (define (ast-is-function? ast)
    (match ast
      [(Unary (LambdaT (list) _ _) _) #f]
      [(Unary (LambdaT _ _ _) _) #t]
      [_ #f]))
  (match (list s e)
    [(list (Input name validation-vector) (Environment inputs defs depends))
     (hash-set! inputs name validation-vector)
     (hash-set! defs
                name
                (ann (Def (Nil) #t (Unary (NoneT) (Nil)))
                     (Def Type)))
     (Ok #t)]
    [(list (Definition name body) (Environment inputs defs depends))
     (cond
       [(and (not (hash-ref inputs name #f))
             (or (not (hash-ref defs name #f))
                 (ast-is-function? body)))
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
        (match (check-input-validity s e)
          [(Fail x) (Fail x)]
          [(Ok _)
           (let ([result (eval-ast body (hash) (Environment inputs defs depends))])
             (match result
               [(Fail x) (Fail x)]
               [(Ok x)
                (hash-set! defs name (Def x #f x))
                (let loop ([keys (get-dependants name depends (set))])
                  (cond
                    [(null? keys) (Ok #t)]
                    [else (set-Def-recalc! (hash-ref (Environment-defs e) (car keys)) #t)
                          (loop (cdr keys))]))]))])])]))

(: build-top-level (-> (Listof (Either Statement String)) (Environment Type (AST Type))
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
