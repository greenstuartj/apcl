#lang typed/racket
(require racket/set)
(require "ast.rkt")
(require "either.rkt")
(provide IContext Environment Def
         Environment-defs set-Environment-defs!
         set-Def-recalc!
         get-dependants)

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

(: get-dependants (-> String (Mutable-HashTable String (Setof String))
                      (Listof String)))
(define (get-dependants name depends)
  (let ([deps : (U (Setof String) #f) (hash-ref depends name #f)])
    (if (not deps)
        '()
        (let ([dep-list (set->list deps)])
          (set->list
           (list->set
            (apply append
                   (cons dep-list
                         (map (lambda ([n : String]) (get-dependants n depends))
                              dep-list)))))))))
                
