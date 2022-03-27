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

(struct (a b) Environment
  ([inputs : (Mutable-HashTable String (Mutable-Vectorof b))]
   [defs : (Mutable-HashTable String (Def a))]
   [depends : (Mutable-HashTable String (Setof String))])
  #:transparent
  #:mutable)

(: get-dependants (-> String (Mutable-HashTable String (Setof String))
                      (Listof String)))
(define (get-dependants name depends)
  (: seen (Mutable-HashTable String True))
  (define seen (make-hash))
  (: aux (-> String (Listof String)))
  (define (aux n)
    (hash-set! seen n #t)
    (let ([deps : (U (Setof String) #f) (hash-ref depends n #f)])
      (if (not deps)
          '()
          (let ([dep-list (set->list deps)])
            (set->list
             (set-remove
              (list->set
               (apply append
                      (cons dep-list
                            (map (lambda ([nm : String]) (aux nm))
                                 (filter (lambda (d) (not (hash-ref seen d #f)))
                                         dep-list)))))
              name))))))
  (aux name))
