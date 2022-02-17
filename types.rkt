#lang typed/racket
(require "ast.rkt")
(require "either.rkt")
(require "environment.rkt")
(provide Type
         IdentifierT
         NumberT
         BoolT
         StringT
         LiteralVectorT
         VectorT
         NoneT
         OptionT
         IfT
         GroupT
         LambdaT
         BuiltinT
         BinopT

         show-type
         depends-type)

(define-type Type
  (U IdentifierT
     NumberT
     BoolT
     StringT
     LiteralVectorT
     VectorT
     NoneT
     OptionT
     IfT
     GroupT
     LambdaT
     BuiltinT
     BinopT))

(struct IdentifierT
  ([n : String])
  #:transparent)

(struct NumberT
  ([n : Real])
  #:transparent)

(struct BoolT
  ([b : Boolean])
  #:transparent)

(struct StringT
  ([s : (Mutable-Vectorof Char)])
  #:transparent
  #:mutable)

(struct LiteralVectorT
  ([v : (Mutable-Vectorof (AST Type))])
  #:transparent
  #:mutable)

(struct VectorT
  ([v : (Mutable-Vectorof (AST Type))])
  #:transparent
  #:mutable)

(struct NoneT ()
  #:transparent)

(struct OptionT ()
  #:transparent)

(struct IfT
  ([test : (AST Type)]
   [t : (AST Type)]
   [f : (AST Type)])
  #:transparent)

(struct GroupT
  ([g : (AST Type)])
  #:transparent)

(struct LambdaT
  ([args : (Listof String)]
   [argt : (IContext Type)]
   [body : (AST Type)])
  #:transparent)

(struct BuiltinT
  ([argc : Integer]
   [argl : (Listof Type)]
   [body : (-> (Listof Type)
               (IContext Type)
               (Environment Type)
               (Either (AST Type) String))])
  #:transparent)

(struct BinopT
  ([lhs : (Option Type)]
   [rhs : (Option Type)]
   [body : (-> Type
               Type
               (IContext Type)
               (Environment Type)
               (Either (AST Type) String))])
  #:transparent)

(: show-type (-> Type String))
(define (show-type t)
  (match t
    [(IdentifierT name) name]
    [(NumberT n)
     (format "~a" (if (integer? n) n (exact->inexact n)))]
    [(BoolT #t) "true"]
    [(BoolT #f) "false"]
    [(StringT s) (list->string (vector->list s))]
    [(VectorT v)
     (let ([sv (vector-map (show-ast show-type) v)])
       (let loop ([ns : String "["]
                  [i : Integer 0])
         (cond
           [(>= i (vector-length sv)) (string-append ns "]")]
           [(= i (sub1 (vector-length sv)))
            (string-append ns (vector-ref sv i) "]")]
           [else
            (loop (string-append ns (vector-ref sv i) ", ") (add1 i))])))]
    [(NoneT) "none"]
    [(OptionT) "option"]
    [_ "not implemented"]))

(: depends-type (-> Type (Listof String)))
(define (depends-type t)
  (match t
    [(IdentifierT name) (list name)]
    [(LiteralVectorT v) (let loop ([i : Integer 0]
                                   [acc : (Listof String) '()])
                          (cond
                            [(>= i (vector-length v)) acc]
                            [else (loop (add1 i)
                                        (append
                                         ((depends-ast depends-type) (vector-ref v i))
                                         acc))]))]
    [(IfT test vrai faux)
     (append ((depends-ast depends-type) test)
             ((depends-ast depends-type) vrai)
             ((depends-ast depends-type) faux))]
    [(GroupT g) ((depends-ast depends-type) g)]
    [(LambdaT args _ body)
     (let ([depends ((depends-ast depends-type) body)]
           [arg-set (list->set args)])
       (filter (lambda (s) (not (set-member? arg-set s))) depends))]
    [(BinopT l r _)
     (let ([dl (if l (depends-type l) '())]
           [dr (if r (depends-type r) '())])
       (append dl dr))]
    [_ '()]))
