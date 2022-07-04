#lang typed/racket
(require racket/match)
(provide AST Nil Unary Binary append-ast show-ast depends-ast copy-ast)

(define-type (AST a)
  (U (Unary a)
     (Binary a)
     Nil))

(struct Nil
  ()
  #:transparent)

(struct (a) Unary
  ([t : a]
   [next : (AST a)])
  #:transparent)

(struct (a) Binary
  ([t : a]
   [lhs : (AST a)]
   [rhs : (AST a)])
  #:transparent)

(: append-ast (All (a) (-> (AST a) (AST a) (AST a))))
(define (append-ast a b)
  (match a
    [(Nil) b]
    [(Unary t next) (Unary t (append-ast next b))]
    [(Binary t lhs rhs) (Binary t lhs (append-ast rhs b))]))

(: show-ast (All (a) (-> (-> a String) (-> (AST a) String))))
(define (show-ast sf)
  (lambda (e)
    (match e
      [(Nil)
       ""]
      [(Unary x (Nil))
       (sf x)]
      [(Unary x next)
       (string-append (sf x) " " ((show-ast sf) next))]
      [(Binary x l r)
       (string-append "(" ((show-ast sf) l) " " (sf x) " " ((show-ast sf) r) ")")])))

(: depends-ast (All (a) (-> (-> a (Listof String)) (-> (AST a) (Listof String)))))
(define (depends-ast df)
  (lambda (ast)
    (match ast
      [(Nil) '()]
      [(Unary t next) (append (df t)
                              ((depends-ast df) next))]
      [(Binary t l r) (append (df t)
                              ((depends-ast df) l)
                              ((depends-ast df) r))])))

(: copy-ast (All (a) (-> (-> a a) (-> (AST a) (AST a)))))
(define (copy-ast ct)
  (lambda (ast)
    (match ast
      [(Nil) (Nil)]
      [(Unary t next) (Unary (ct t)
                             ((copy-ast ct) next))]
      [(Binary t l r) (Binary (ct t)
                              ((copy-ast ct) l)
                              ((copy-ast ct) r))])))
