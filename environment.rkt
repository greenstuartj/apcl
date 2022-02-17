#lang typed/racket
(require racket/set)
(require "ast.rkt")
(require "either.rkt")
(provide IContext Environment Def
         Environment-defs set-Environment-defs!
         set-Def-recalc!)

(define-type (IContext a) (Immutable-HashTable String a))

(struct (a) Def
  ([value : (AST a)]
   [recalc : Boolean]
   [func : (AST a)])
  #:mutable
  #:transparent)

(struct (a) Environment
  ([inputs : (Mutable-HashTable String True)]
   [defs : (Mutable-HashTable String (Def a))]
   [depends : (Mutable-HashTable String (Setof String))])
  #:transparent
  #:mutable)
