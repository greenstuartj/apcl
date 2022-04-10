#lang typed/racket
(require racket/match)
(require "either.rkt")
(require "lexer.rkt")
(require "types.rkt")
(require "ast.rkt")
(provide parse parse-lambda depth-take)

(: bury-ref (-> (AST Type) (AST Type)))
(define (bury-ref b)
  (match b
    [(Unary t next) (Binary (RefT #f #f) (Nil) b)]
    [(Binary b2 (Nil) y) (Binary (RefT #f #f) (Nil) y)]
    [(Binary b2 x y) (Binary (RefT #f #f) (bury-ref x) y)]))

(: add-to-ref-left (-> Type (AST Type) (Option (AST Type))))
(define (add-to-ref-left t r)
  (match r
    [(Unary _ _) #f]
    [(Binary (RefT _ _) (Nil) y)
     (Binary (RefT #f #f) (Unary t (Nil)) y)]
    [(Binary (RefT _ _) x y)
     (match (add-to-ref-left t x)
       [#f #f]
       [nb
        (assert nb)
        (Binary (RefT #f #f) nb y)])]))

(: tree (-> Type (Listof Token) (Either (AST Type) String)))
(define (tree t tl)
  (let ([result (parse tl)])
    (match result
      [(Ok (Binary (SetT #f #f) (Nil) y))
       (Ok (Binary (SetT #f #f) (Unary t (Nil)) y))]
      [(Ok (Binary (SetT #f #f) x y))
       (match t
         [(RefT _ _)
          (Ok (Binary (SetT #f #f) (bury-ref x) y))]
         [_
          (match (add-to-ref-left t x)
            [#f (Ok (Unary t (Binary (SetT #f #f) x y)))]
            [nx
             (assert nx)
             (Ok (Binary (SetT #f #f) nx y))])])]
      [(Ok (Binary (RefT #f #f) (Nil) y))
       (Ok (Binary (RefT #f #f) (Unary t (Nil)) y))]
      [(Ok (Binary (RefT #f #f) x y))
       (match t
         [(RefT _ _)
          (Ok (Binary (RefT #f #f) (bury-ref x) y))]
         [_
          (match (add-to-ref-left t x)
            [#f (Ok (Unary t (Binary (RefT #f #f) x y)))]
            [nx
             (assert nx)
             (Ok (Binary (RefT #f #f) nx y))])])]
      [(Ok (Binary b (Nil) y))
       (match t
         [(BinopT _ _ _) (Ok (Binary t (Nil) (Binary b (Nil) y)))]
         [_ (Ok (Binary b (Unary t (Nil)) y))])]
      [(Ok (Binary b x y))
       (match t
         [(BinopT _ _ _) (Ok (Binary t (Nil) (Binary b x y)))]
         [_ (Ok (Unary t (Binary b x y)))])]
      [(Ok ast)
       (match t
         [(SetT _ _) (Ok (Binary t (Nil) ast))]
         [(RefT _ _) (Ok (Binary t (Nil) ast))]
         [(BinopT _ _ _) (Ok (Binary t (Nil) ast))]
         [_ (Ok (Unary t ast))])]
      [(Fail x) (Fail x)])))

(: binop (-> String Type))
(define (binop s)
  (cond
    [(equal? s "->") (RefT #f #f)]
    [(equal? s "<-") (SetT #f #f)]
    [else (BinopT #f #f s)]))

(: depth-take
   (-> (Listof Token) Integer TokenType TokenType String
       (Either (List (Listof Token) (Listof Token)) String)))
(define (depth-take tl dp tts tte err)
  (let loop ([t : (Listof Token) tl]
             [d : Integer dp]
             [acc : (Listof Token) '()])
    (match t
      ['()
       (Fail err)]
      [`(,(Token tt s l) . ,rt)
       (cond
         [(and (zero? d) (eq? tte tt))
          (Ok (list (reverse acc) rt))]
         [(eq? tte tt)
          (loop rt (sub1 d) (cons (car t) acc))]
         [(eq? tts tt)
          (loop rt (add1 d) (cons (car t) acc))]
         [else [loop rt d (cons (car t) acc)]])])))

(: parse-group
   (-> (Listof Token) Integer
       (Either (AST Type) String)))
(define (parse-group tl line)
  (match (depth-take tl 0 'open-paren 'close-paren
                     (string-append "[ERROR] unmatched ( on line "
                                    (format "~a" line)))
    [(Fail x) (Fail x)]
    [(Ok (list g-tl tld))
     (let ([result (parse g-tl)])
       (match result
         [(Fail x) (Fail x)]
         [(Ok x)
          (tree (GroupT x) tld)]))]))

(: parse-module
   (-> (Listof Token) Integer
       (Either (AST Type) String)))
(define (parse-module tl line)
  (match (depth-take tl 0 'open-curl 'close-curl
                     (string-append "[ERROR] unmatched { on line "
                                    (format "~a" line)))
    [(Fail x) (Fail x)]
    [(Ok (list m-tl tld))
     (tree (LiteralModuleT m-tl) tld)]))

(: parse-vector
   (-> (Listof Token) Integer
       (Either (List (Listof (AST Type)) (Listof Token)) String)))
(define (parse-vector tl line)
  (let loop ([t : (Listof Token) tl]
             [temp : (Listof Token) '()]
             [acc : (Listof (AST Type)) '()])
    (match t
      ['()
       (Fail (string-append "[ERROR] unmatched [ on line " (format "~a" line)))]
      [`(,(Token 'close-square _ _) . ,d)
       (let ([result (parse (reverse temp))])
         (match result
           [(Ok x) (Ok (list (reverse (cons x acc)) d))]
           [(Fail x) (Fail x)]))]
      [`(,(Token 'open-square _ line) . ,d)
       (match (depth-take d 0 'open-square 'close-square
                          (string-append "[ERROR] unmatched [ on line "
                                         (format "~a" line)))
         [(Fail x) (Fail x)]
         [(Ok (list v-tl tdl))
          (loop tdl
                (cons (Token 'close-square "]" line)
                      (append (reverse v-tl)
                              (cons (Token 'open-square "[" line)
                                    temp)))
                acc)])]
      [`(,(Token 'comma _ _) . ,d)
       (let ([result (parse (reverse temp))])
         (match result
           [(Ok x) (loop d '() (cons x acc))]
           [(Fail x) (Fail x)]))]
      [`(,a . ,d)
       (loop d (cons a temp) acc)])))

(: parse-if (-> (Listof Token) Integer (Either (AST Type) String)))
(define (parse-if tl line)
  (let ([ctest (depth-take tl 0 'if 'then
                           (string-append "[ERROR] malformed if starting at line "
                                          (format "~a" line)))])
    (match ctest
      [(Fail x) (Fail x)]
      [(Ok (list test next))
       (let ([ctrue (depth-take next 0 'then 'else
                                (string-append "[ERROR] malformed if starting at line "
                                          (format "~a" line)))])
         (match ctrue
           [(Fail y) (Fail y)]
           [(Ok (list t f))
            (match (list (parse test) (parse t) (parse f))
              [(list (Ok test-ast) (Ok t-ast) (Ok f-ast))
               (Ok (Unary (IfT test-ast t-ast f-ast) (Nil)))]
              [(list (Fail x) _ _) (Fail x)]
              [(list _ (Fail x) _) (Fail x)]
              [(list _ _ (Fail x)) (Fail x)])]))])))

(: parse-let (-> (Listof Token) Integer (Either (AST Type) String)))
(define (parse-let tl line)
  (match (depth-take tl 0 'let 'in
                     (string-append "[ERROR] malformed let starting at line "
                                    (format "~a" line)))
    [(Fail x) (Fail x)]
    [(Ok (list l r))
     (match (list (parse l) (parse r))
       [(list (Fail x) _)
        (Fail x)]
       [(list _ (Fail x))
        (Fail x)]
       [(list (Ok (Unary (LambdaT (list a) ic body) (Nil))) (Ok next))
        (Ok (Unary (LambdaT (list a) ic next) body))]
       [_ (Fail (string-append "[ERROR] malformed let starting at line "
                               (format "~a" line)))])]))

(: collect-args
   (-> (Listof Token) Integer
       (Either (List (Listof String) (Listof Token)) String)))
(define (collect-args tl line)
  (let loop ([t : (Listof Token) tl]
             [acc : (Listof String) '()])
    (match t
      ['()
       (Fail (string-append "[ERROR] unfinished lambda expression starting on line "
                            (format "~a" line)))]
      [`(,(Token 'colon _ _) . ,d)
       (Ok (list (reverse acc) d))]
      [`(,(Token 'identifier name _) . ,d)
       (loop d (cons name acc))]
      [`(,(Token _ _ l) . ,d)
       (Fail (string-append "[ERROR] non-identifier in lambda arguments on line "
                            (format "~a" l)))])))

(: parse-lambda (-> (Listof Token) Integer
                    (Either (AST Type) String)))
(define (parse-lambda tl line)
  (match (collect-args tl line)
    [(Fail x) (Fail x)]
    [(Ok (list args next))
     (match (parse next)
       [(Fail y) (Fail y)]
       [(Ok body) (Ok (Unary (LambdaT args (hash) body) (Nil)))])]))

(: parse (-> (Listof Token) (Either (AST Type) String)))
(define (parse tl)
  (match tl
    ['() (Ok (Nil))]
    [`(,(Token 'semicolon _ _) . ,d)
     (Ok (Nil))]
    [`(,(Token 'identifier name _) . ,d)
     (tree (IdentifierT name) d)]
    [`(,(Token 'not _ _) . ,d)
     (tree (IdentifierT "not") d)]
    [`(,(Token 'neg _ _) . ,d)
     (tree (IdentifierT "neg") d)]
    [`(,(Token 'abs _ _) . ,d)
     (tree (IdentifierT "abs") d)]
    [`(,(Token 'number num _) . ,d)
     (let ((result (string->number num)))
       (if result
           (tree (NumberT (cast result Real)) d)
           (Fail (string-append "[ERROR] invalid number " num))))]
    [`(,(Token 'bool "true" _) . ,d)
     (tree (BoolT #t) d)]
    [`(,(Token 'bool "false" _) . ,d)
     (tree (BoolT #f) d)]
    [`(,(Token 'string s _) . ,d)
     (tree (StringT (list->vector (string->list s))) d)]
    [`(,(Token 'none _ _) . ,d)
     (tree (NoneT) d)]
    [`(,(Token 'option _ _) . ,d)
     (tree (OptionT) d)]
    [`(,(Token 'binop s _) . ,d)
     (tree (binop s) d)]
    [`(,(Token 'right-arrow s _) . ,d)
     (tree (binop s) d)]
    [`(,(Token 'left-arrow s _) . ,d)
     (tree (binop s) d)]
    [`(,(Token 'open-square _ line) . ,d)
     (let ([result (parse-vector d line)])
       (match result
         [(Ok (list v r)) (tree (LiteralVectorT (list->vector v)) r)]
         [(Fail x) (Fail x)]))]
    [`(,(Token 'open-paren _ line) . ,d)
     (parse-group d line)]
    [`(,(Token 'open-curl _ line) . ,d)
     (parse-module d line)]
    [`(,(Token 'backslash _ line) . ,d)
     (parse-lambda d line)]
    [`(,(Token 'if _ line) . ,d)
     (parse-if d line)]
    [`(,(Token 'let _ line) . ,d)
     (parse-let (cons (Token 'backslash "\\" line) d) line)]
    [`(,(Token _ s line) . ,d)
     (Fail (string-append "[ERROR] Unexpected " s " on line " (format "~a" line)))]))
