#lang typed/racket
(provide Either Ok Fail inject-either bind-either)

(define-type (Either a b)
  (U (Ok a)
     (Fail b)))

(struct (a) Ok
  ([s : a])
  #:transparent)

(struct (a) Fail
  ([f : a])
  #:transparent)

(: inject-either (All (a b c) (-> (-> a b) (Either a c) (Either b c))))
(define (inject-either func v)
  (match v
    [(Ok s) (Ok (func s))]
    [(Fail f) (Fail f)]))

(: bind-either (All (a b c) (-> (-> a (Either c b)) (Either a b) (Either c b))))
(define (bind-either func v)
  (match v
    [(Ok s) (func s)]
    [(Fail x) (Fail x)]))
