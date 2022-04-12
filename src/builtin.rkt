#lang typed/racket
(require racket/match)
(require "ast.rkt")
(require "either.rkt")
(require "types.rkt")
(require "environment.rkt")
(require "table.rkt")
(provide core-table binop-table)

;; HELPERS

(: s-un (-> Type (Either (AST Type) String)))
(define (s-un t)
  (Ok (Unary t (Nil))))

(: bn (-> Boolean Real))
(define (bn b) (if b 1 0))

(: is-zero? (-> Type Boolean))
(define (is-zero? t)
  (match t
    [(NumberT 0) #t]
    [_ #f]))

;; BINOP 

(define-type binop-signature
  (-> Type
      Type
      (IContext Type)
      (Environment Type (AST Type))
      (Either (AST Type) String)))

; BINOP MATHS FUNCTIONS

(: binop-mf (-> (-> Real Real (Either (AST Type) String))
                String
                binop-signature))
(define (binop-mf f sop)
  (lambda (t1 t2 ic e)
    (match (list t1 t2)
      [(list (NoneT) _)
       (s-un (NoneT))]
      [(list _ (NoneT))
       (s-un (NoneT))]
      [(list (OptionT) _)
       (s-un t2)]
      [(list _ (OptionT))
       (s-un t1)]
      [(list (NumberT n) (NumberT m))
       (f n m)]
      [(list (NumberT n) (BoolT m))
       (f n (bn m))]
      [(list (BoolT n) (NumberT m))
       (f (bn n) m)]
      [(list (BoolT n) (BoolT m))
       (f (bn n) (bn m))]
      [_
       (Fail (string-append "[ERROR] " sop ": non-numeric type"))])))

(: add binop-signature)
(define add
  (binop-mf (lambda (a b) (s-un (NumberT (+ a b)))) "+"))

(: minus binop-signature)
(define minus
  (binop-mf (lambda (a b) (s-un (NumberT (- a b))))  "-"))

(: mult binop-signature)
(define mult
  (binop-mf (lambda (a b) (s-un (NumberT (* a b)))) "*"))

(: div binop-signature)
(define div
  (binop-mf (lambda (a b)
              (if (zero? b)
                  (Fail "[ERROR] /: divide by zero")
                  (s-un (NumberT (/ a b)))))
            "/"))

(: pow binop-signature)
(define pow
  (binop-mf (lambda (a b)
              (s-un (NumberT (cast (expt (cast a Number) (cast b Number)) Real))))
            "^"))

(: mod binop-signature)
(define mod
  (binop-mf (lambda (a b)
              (if (zero? b)
                  (Fail "[ERROR] %: divide by zero")
                  (if (and (integer? a) (integer? b))
                      (s-un (NumberT (modulo (exact-floor a) (exact-floor b))))
                      (Fail "[ERROR] %: non-integer"))))
            "%"))

; reimplement to get string <. and >.
(: min-f binop-signature)
(define min-f
  (binop-mf (lambda (a b)
              (s-un (NumberT  (min a b))))
            "<."))

(: max-f binop-signature)
(define max-f
  (binop-mf (lambda (a b)
              (s-un (NumberT  (max a b))))
            ">."))

; BINOP COMPARISON FUNCTIONS
(: eq binop-signature)
(define (eq t1 t2 ic e)
  (match (list t1 t2)
    [(list (NumberT 1) (BoolT #t))
     (s-un (BoolT #t))]
    [(list (NumberT 0) (BoolT #f))
     (s-un (BoolT #t))]
    [(list (BoolT #t) (NumberT 1))
     (s-un (BoolT #t))]
    [(list (BoolT #f) (NumberT 0))
     (s-un (BoolT #t))]
    [_
     (s-un (BoolT (equal? t1 t2)))]))

(: neq binop-signature)
(define (neq t1 t2 ic e)
  (let ((r (eq t1 t2 ic e)))
    (match r
      [(Ok (Unary (BoolT b) _)) (s-un (BoolT (not b)))]
      [_ r])))


(: cmp (-> (-> Real Boolean)
           String
           binop-signature))
(define (cmp func sop)
  (lambda (t1 t2 ic e)
     (let ((c
            (match (list t1 t2)
              [(list (NumberT n) (NumberT m))
               (Ok
                (cond ((> n m) 1)
                      ((= n m) 0)
                      (else   -1)))]
              [_
               (Fail (string-append "[ERROR] " sop ": unsupported type"))])))
       ; debug inject-either
       (match c
         [(Ok n) (s-un (BoolT (func n)))]
         [(Fail f) (Fail f)]))))

(: lt binop-signature)
(define lt (cmp (lambda (n) (< n 0)) "<"))

(: le binop-signature)
(define le (cmp (lambda (n) (< n 1)) "<="))

(: gt binop-signature)
(define gt (cmp (lambda (n) (> n 0)) ">"))

(: ge binop-signature)
(define ge (cmp (lambda (n) (> n -1)) ">="))

(: or-f binop-signature)
(define (or-f t1 t2 ic e)
  (match (list t1 t2)
    [(list (BoolT #f) (BoolT #f)) (s-un (BoolT #f))]
    [_ (s-un (BoolT #t))]))

(: and-f binop-signature)
(define (and-f t1 t2 ic e)
  (match (list t1 t2)
    [(list (BoolT #f) _) (s-un (BoolT #f))]
    [(list _ (BoolT #f)) (s-un (BoolT #f))]
    [_ (s-un (BoolT #t))]))

(: concat-f binop-signature)
(define (concat-f t1 t2 ic e)
  (match (list t1 t2)
    [(list (StringT s1) (StringT s2))
     (s-un (StringT (vector-append s1 s2)))]
    [(list (VectorT v1) (VectorT v2))
     (s-un (VectorT (vector-append v1 v2)))]
    [_ (Fail "[ERROR] &: type mismatch")]))

;; OTHER FUNCTIONS

(define-type core-signature
  (-> (-> (AST Type)
          (IContext Type)
          (Environment Type (AST Type))
          (Either (AST Type) String))
      (-> (Listof Type)
          (IContext Type)
          (Environment Type (AST Type))
          (Either (AST Type) String))))

; UNARY
(: not-f core-signature)
(define (not-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (BoolT #f))
       (s-un (BoolT #t))]
      [_ (s-un (BoolT #f))])))

(: neg-f core-signature)
(define (neg-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (NumberT n))
       (if (> n 0)
           (s-un (NumberT (- n)))
           (s-un (NumberT (abs n))))]
      [_
       (Fail "[ERROR] ~: number expected")])))

(: abs-f core-signature)
(define (abs-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (NumberT n))
       (s-un (NumberT (abs n)))]
      [_
       (Fail "[ERROR] |: number expected")])))
;

(: optional-f core-signature)
(define (optional-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (NoneT)) (s-un (OptionT))]
      [(list x) (s-un x)])))

(: iota-f core-signature)
(define (iota-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (NumberT n))
       (if (not (integer? n))
           (Fail "[ERROR] iota: integer expected")
           (let ([v : (Mutable-Vectorof (AST Type))
                    (make-vector (cast n Integer) (Nil))])
             (let loop ([i : Integer 0])
               (cond
                 [(>= i (vector-length v)) (s-un (VectorT v))]
                 [else (vector-set! v i (Unary (NumberT i) (Nil)))
                       (loop (+ i 1))]))))]
      [_
       (Fail "[ERROR] iota: integer expected")])))

(: map-f core-signature)
(define (map-f ev)
  (lambda (tl ic e)
    (match tl
      [(list f (VectorT vec))
       (let ([v : (Mutable-Vectorof (AST Type))
                (make-vector (vector-length vec) (Nil))])
         (let loop ([i : Integer 0])
           (cond
             [(>= i (vector-length v)) (s-un (VectorT v))]
             [else (let ([result (ev (Unary f (vector-ref vec i))
                                     ic e)])
                     (match result
                       [(Ok r) (vector-set! v i r)
                               (loop (add1 i))]
                       [(Fail x) (Fail x)]))])))]
      [(list f (StringT str))
       ((map-f ev)
        (list f (VectorT (vector-map (lambda ([c : Char]) (Unary (StringT (vector c)) (Nil)))
                                     str)))
        ic e)]
      [_
       (Fail "[ERROR] map: vector expected")])))

(: reduce-f core-signature)
(define (reduce-f ev)
  (lambda (tl ic e)
    (match tl
      [(list f (VectorT vec))
       (let loop ([i : Integer (- (vector-length vec) 2)]
                  [acc : (AST Type) (vector-ref vec (sub1 (vector-length vec)))])
         (cond
           [(< i 0) (Ok acc)]
           [else (let ([result (ev (append-ast (Unary f (vector-ref vec i))
                                               acc)
                                   ic e)])
                   (match result
                     [(Ok r) (loop (sub1 i) r)]
                     [(Fail x) (Fail x)]))]))]
    [_
     (Fail "[ERROR] reduce: vector expected")])))

(: reverse-f core-signature)
(define (reverse-f ev)
  (: f (All (a) (-> (Mutable-Vectorof a) a (Mutable-Vectorof a))))
  (define (f v d)
    (let ([nv : (Mutable-Vectorof a)
              (make-vector (vector-length v) d)])
      (let loop ([i : Integer 0])
        (cond
          [(>= i (vector-length nv)) nv]
          [else (vector-set! nv i (vector-ref v (- (sub1 (vector-length v)) i)))
                (loop (add1 i))]))))
  (lambda (tl ic e)
    (match tl
      [(list (StringT s))
       (s-un (StringT (f s #\0)))]
      [(list (VectorT v))
       (s-un (VectorT (f v (Nil))))]
      [_ (Fail "[ERROR] reverse: vector|string expected")])))

(: drop-f core-signature)
(define (drop-f ev)
  (: f (All (a) (-> Integer (Mutable-Vectorof a) a (Mutable-Vectorof a))))
  (define (f n v d)
    (let ([nv : (Mutable-Vectorof a)
              (make-vector (- (vector-length v) n) d)])
      (let loop ([i : Integer n]
                 [j : Integer 0])
        (cond
          [(>= j (vector-length nv)) nv]
          [else (vector-set! nv j (vector-ref v i))
                (loop (add1 i) (add1 j))]))))
  (lambda (tl ic e)
    (match tl
      [(list (NumberT n) t)
       (if (not (integer? n))
           (Fail "[ERROR] drop: integer expected")
           (match t
             [(StringT s) (s-un (StringT (f (cast n Integer) s #\0)))]
             [(VectorT v) (s-un (VectorT (f (cast n Integer) v (Nil))))]
             [_ (Fail "[ERROR] drop: vector|string expected")]))])))

(: take-f core-signature)
(define (take-f ev)
  (: f (All (a) (-> Integer (Mutable-Vectorof a) a (Mutable-Vectorof a))))
  (define (f n v d)
    (let ([nv : (Mutable-Vectorof a)
              (make-vector n d)])
      (let loop ([i : Integer 0])
        (cond
          [(>= i (vector-length nv)) nv]
          [else (vector-set! nv i (vector-ref v i))
                (loop (add1 i))]))))
  (lambda (tl ic e)
    (match tl
      [(list (NumberT n) t)
       (if (not (integer? n))
           (Fail "[ERROR] take: integer expected")
           (match t
             [(StringT s) (s-un (StringT (f (cast n Integer) s #\0)))]
             [(VectorT v) (s-un (VectorT (f (cast n Integer) v (Nil))))]
             [_ (Fail "[ERROR] take: vector|string expected")]))])))

(: read-table-f core-signature)
(define (read-table-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (StringT fn))
       (read-dsv (list->string (vector->list fn)) #\, #\" #t "read_table")]
      [_ (Fail "[ERROR] read_table: string expected")])))

(: read-dsv-f core-signature)
(define (read-dsv-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (StringT fn) (StringT sep) (StringT qt))
       (cond
         [(> (vector-length sep) 1)
          (Fail "[ERROR] read_dsv: separator should be 1 char long string")]
         [(> (vector-length qt) 1)
          (Fail "[ERROR] read_dsv: quote should be 1 char long string")]
         [else
          (read-dsv (list->string (vector->list fn))
                    (vector-ref sep 0)
                    (vector-ref qt 0)
                    #t
                    "read_dsv")])]
      [(list (StringT _) (StringT _) _)
       (Fail "[ERROR] read_dsv: quote should be 1 char long string")]
      [(list (StringT _) _ (StringT _))
       (Fail "[ERROR] read_dsv: separator should be 1 char long string")]
      [_
       (Fail "[ERROR] read_dsv: filename should be string")])))

(: read-lines-f core-signature)
(define (read-lines-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (StringT fn))
       (match (read-dsv (list->string (vector->list fn)) #\newline #\" #f "read-lines")
         [(Fail x) (Fail x)]
         [(Ok x)
          (match x
            [(Unary (VectorT v) next)
             (let loop ([i : Integer 0])
               (cond
                 [(>= i (vector-length v)) (Ok (Unary (VectorT v) next))]
                 [else (match (vector-ref v i)
                         [(Unary (VectorT ve2) r)
                          (let ([new-unary : (AST Type) (vector-ref ve2 0)])
                            (vector-set! v i new-unary))
                          (loop (add1 i))]
                         [_ (loop (add1 i))])]))]
            [_ (Ok x)])])]
      [_ (Fail "[ERROR] read_lines: string expected")])))

(: get-f core-signature)
(define (get-f ev)
  (: handle-neg (-> Integer Integer Integer))
  (define (handle-neg n len)
    (if (>= n 0)
        n
        (- len (abs n))))
  (lambda (tl ic e)
    (match tl
      [(list (NoneT) _)
       (s-un (NoneT))]
      [(list (OptionT) _)
       (s-un (OptionT))]
      [(list (NumberT n) vs)
       (cond
         [(not (integer? n))
          (Fail "[ERROR] get: integer expected")]
         [else
          (match vs
            [(VectorT v)
             (if (or (>= n (vector-length v))
                     (> (abs n) (vector-length v)))
                 (s-un (NoneT))
                 (Ok (vector-ref v (handle-neg (cast n Integer)
                                               (vector-length v)))))]
            [(StringT s)
             (if (or (>= n (vector-length s))
                     (> (abs n) (vector-length s)))
                 (s-un (NoneT))
                 (s-un
                  (StringT (vector
                            (vector-ref s (handle-neg (cast n Integer)
                                                      (vector-length s)))))))]
             [_ (Fail "[ERROR] get: string|vector expected")])])]
      [_ (Fail "[ERROR] get: integer expected")])))

(: length-f core-signature)
(define (length-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (VectorT v))
       (s-un (NumberT (vector-length v)))]
      [(list (StringT s))
       (s-un (NumberT (vector-length s)))]
      [_ (Fail "[ERROR] length: string|vector expected")])))

(: replicate-f core-signature)
(define (replicate-f ev)
  (: get-len (-> (Mutable-Vectorof (AST Type)) (Either Integer String)))
  (define (get-len m)
    (let loop ([i : Integer 0]
               [acc : Integer 0])
      (if (>= i (vector-length m))
          (Ok acc)
          (match (vector-ref m i)
            [(Unary (BoolT #t) _)
             (loop (add1 i) (add1 acc))]
            [(Unary (BoolT #f) _)
             (loop (add1 i) acc)]
            [(Unary (NumberT n) _)
             (if (integer? n)
                 (loop (add1 i) (+ acc (cast n Integer)))
                 (Fail "[ERROR] replicate: numbers must be integers"))]
            [_ (Fail "[ERROR] replicate: integer|bool expected")]))))
  (: type->number (-> (AST Type) Integer))
  (define (type->number t)
    (match t
      [(Unary (BoolT #f) _)  0]
      [(Unary (BoolT #t) _)  1]
      [(Unary (NumberT n) _) (cast n Integer)] ; will always be integer due to get-len
      [_ 0])) ; _ wont be hit as invalid state in get-len
  (: f (All (a) (-> (Mutable-Vectorof (AST Type)) (Mutable-Vectorof a) a
                    (Either (Mutable-Vectorof a) String))))
  (define (f m v d)
    (let ([nv-len (get-len m)])
      (match nv-len
        [(Fail x) (Fail x)]
        [(Ok n)
         (let ([nv : (Mutable-Vectorof a)
                   (make-vector n d)])
           (let loop ([i : Integer 0]
                      [j : Integer (type->number (vector-ref m 0))]
                      [k : Integer 0])
             (cond
               [(>= i (vector-length nv)) (Ok nv)]
               [(and (< j 0) (>= i (vector-length nv))) (Ok nv)]
               [(<= j 0)
                (loop i (type->number (vector-ref m (add1 k))) (add1 k))]
               [else
                (vector-set! nv i (vector-ref v k))
                (loop (add1 i) (sub1 j) k)])))])))
  (lambda (tl ic e)
    (match tl
      [(list (VectorT m) (VectorT v))
       (let ([result (f m v (Nil))])
         (match result
           [(Fail x) (Fail x)]
           [(Ok x)
            (s-un (VectorT x))]))]
      [(list (VectorT m) (StringT s))
       (let ([result (f m s #\0)])
         (match result
           [(Fail x) (Fail x)]
           [(Ok x)
            (s-un (StringT x))]))]
      [(list (VectorT _) _)
       (Fail "[ERROR] replicate: string|vector expected")]
      [_ (Fail "[ERROR] replicate: vector expected")])))

(: scan-f core-signature)
(define (scan-f ev)
  (lambda (tl ic e)
    (match tl
      [(list f (VectorT v))
       (let ([nv : (Mutable-Vectorof (AST Type))
                 (make-vector (vector-length v) (Nil))])
         (let loop ([i : Integer 1])
           (cond
             [(> i (vector-length nv)) (s-un (VectorT nv))]
             [else
              (let ([tv : (Mutable-Vectorof (AST Type))
                        (make-vector i (Nil))])
                (vector-copy! tv 0 v 0 i)
                (match ((reduce-f ev) (list f (VectorT tv)) ic e)
                  [(Fail x) (Fail x)]
                  [(Ok x)
                   (vector-set! nv (sub1 i) x)
                   (loop (add1 i))]))])))]
      [_ (Fail "[ERROR] scan: vector expected")])))

(: scan-n-f core-signature)
(define (scan-n-f ev)
  (lambda (tl ic e)
    (match tl
      [(list f (NumberT n) (VectorT v))
       (cond
         [(not (integer? n))
          (Fail "[ERROR] scan_n: integer expected")]
         [(> n (vector-length v))
          (Fail "[ERROR] scan_n: integer must not be larger than vector length")]
         [else
          (let* ([m : Integer (cast n Integer)]
                 [nv : (Mutable-Vectorof (AST Type))
                     (make-vector (- (vector-length v) (sub1 m)) (Nil))])
            (let loop ([i : Integer 0])
              (cond
                [(> (+ i m) (vector-length v)) (s-un (VectorT nv))]
                [else
                 (let ([tv : (Mutable-Vectorof (AST Type))
                           (make-vector m (Nil))])
                   (vector-copy! tv 0 v i (+ i m))
                   (match ((reduce-f ev) (list f (VectorT tv)) ic e)
                     [(Fail x) (Fail x)]
                     [(Ok x)
                      (vector-set! nv i x)
                      (loop (add1 i))]))])))])]
      [(list _ _ (VectorT _))
       (Fail "[ERROR] scan_n: integer expected")]
      [(list _ (NumberT _) _)
       (Fail "[ERROR] scan_n: vector expected")])))

(: zip-f core-signature)
(define (zip-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (VectorT v) (VectorT w))
       (let ([nv : (Mutable-Vectorof (AST Type))
                 (make-vector (max (vector-length v) (vector-length w)) (Nil))])
         (let loop ([i : Integer 0])
           (cond
             [(>= i (vector-length nv)) (s-un (VectorT nv))]
             [else
              (let ([ne : Type
                        (VectorT (vector (if (>= i (vector-length v))
                                             (Unary (NoneT) (Nil))
                                             (vector-ref v i))
                                         (if (>= i (vector-length w))
                                             (Unary (NoneT) (Nil))
                                             (vector-ref w i))))])
                (vector-set! nv i (Unary ne (Nil)))
                (loop (add1 i)))])))]
      [_ (Fail "[ERROR] zip: expected vector")])))

(: zip-with-f core-signature)
(define (zip-with-f ev)
  (lambda (tl ic e)
    (match tl
      [(list f (VectorT v) (VectorT w))
       (let ([nv : (Mutable-Vectorof (AST Type))
                 (make-vector (max (vector-length v) (vector-length w)) (Nil))])
         (let loop ([i : Integer 0])
           (cond
             [(>= i (vector-length nv)) (s-un (VectorT nv))]
             [else
              (let ([ne1 : (AST Type) (if (>= i (vector-length v))
                                          (Unary (NoneT) (Nil))
                                          (vector-ref v i))]
                    [ne2 : (AST Type) (if (>= i (vector-length w))
                                          (Unary (NoneT) (Nil))
                                          (vector-ref w i))])
                (let ([result (ev (append-ast (Unary f (Nil)) (append-ast ne1 ne2))
                                  ic e)])
                  (match result
                    [(Fail x) (Fail x)]
                    [(Ok x)
                     (vector-set! nv i x)
                     (loop (add1 i))])))])))]
      [_ (Fail "[ERROR] zip-with: expected vector")])))

(: member-f core-signature)
(define (member-f ev)
  (: char-vector->string (-> (Mutable-Vectorof Char) String))
  (define (char-vector->string v)
    (list->string (vector->list v)))
  (lambda (tl ic e)
    (match tl
      [(list elem (VectorT v))
       (let loop ([i : Integer 0])
         (cond
           [(>= i (vector-length v)) (s-un (BoolT #f))]
           [(equal? (Unary elem (Nil)) (vector-ref v i)) (s-un (BoolT #t))]
           [else (loop (add1 i))]))]
      [(list (StringT s1) (StringT s2))
       (s-un (BoolT (string-contains? (char-vector->string s2) (char-vector->string s1))))]
      [(list _ (StringT _))
       (Fail "[ERROR] member: string expected")]
      [_ (Fail "[ERROR] member: expected string|vector")])))

(: repeat-f core-signature)
(define (repeat-f ev)
  (lambda (tl ic e)
    (match tl
      [(list f (NumberT n) x)
       (if (not (integer? n))
           (Fail "[ERROR] repeat: integer expected")
           (let loop ([i : Integer 0]
                      [acc : (AST Type) (Unary x (Nil))])
             (cond
               [(>= i n) (Ok acc)]
               [else (match (ev (Unary f acc) ic e)
                       [(Fail y) (Fail y)]
                       [(Ok y) (loop (add1 i) y)])])))]
      [_ (Fail "[ERROR] repeat: integer expected")])))

(: floor-f core-signature)
(define (floor-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (NumberT n)) (s-un (NumberT (inexact->exact (floor n))))]
      [_ (Fail "[ERROR] floor: number expected")])))

(: ceiling-f core-signature)
(define (ceiling-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (NumberT n)) (s-un (NumberT (inexact->exact (ceiling n))))]
      [_ (Fail "[ERROR] ceiling: number expected")])))

(: seq-f core-signature)
(define (seq-f ev)
  (lambda (tl ic e)
    (match tl
      [(list f (NumberT n) start)
       (if (not (integer? n))
           (Fail "[ERROR] seq: integer expected")
           (let ([nv : (Mutable-Vectorof (AST Type))
                     (make-vector (cast n Integer) (Unary start (Nil)))])
             (let loop ([i : Integer 1])
               (cond
                 [(>= i n) (s-un (VectorT nv))]
                 [else (match (ev (Unary f (vector-ref nv (sub1 i))) ic e)
                         [(Fail x) (Fail x)]
                         [(Ok x) (vector-set! nv i x)
                                 (loop (add1 i))])]))))]
      [_ (Fail "[ERROR] seq: integer expected")])))

(: while-f core-signature)
(define (while-f ev)
  (lambda (tl ic e)
    (match tl
      [(list cf f start)
       (let loop ([acc : (AST Type) (Unary start (Nil))])
         (match (ev (Unary cf acc) ic e)
           [(Fail x) (Fail x)]
           [(Ok (Unary (BoolT #f) _)) (Ok acc)]
           [_ (match (ev (Unary f acc) ic e)
                [(Fail x) (Fail x)]
                [(Ok x) (loop x)])]))])))

(: rotate-f core-signature)
(define (rotate-f ev)
  (: calc-index (-> Integer Integer Integer Integer))
  (define (calc-index i n vl)
    (modulo (+ i n) vl))
  (: f (All (a) (-> Real (Mutable-Vectorof a) a (Either (Mutable-Vectorof a) String))))
  (define (f n v d)
    (if (not (integer? n))
        (Fail "[ERROR] rotate: integer expected")
        (let ([nv : (Mutable-Vectorof a)
                  (make-vector (vector-length v) d)])
          (let loop ([i : Integer 0])
            (cond
              [(>= i (vector-length nv)) (Ok nv)]
              [else (vector-set! nv i (vector-ref v (calc-index i
                                                                (cast n Integer)
                                                                (vector-length nv))))
                    (loop (add1 i))])))))
  (lambda (tl ic e)
    (match tl
      [(list (NumberT n) (VectorT v))
       (match (f n v (Nil))
         [(Fail x) (Fail x)]
         [(Ok x) (s-un (VectorT x))])]
      [(list (NumberT n) (StringT s))
       (match (f n s #\0)
         [(Fail x) (Fail x)]
         [(Ok x) (s-un (StringT x))])]
      [(list (NumberT _) _)
       (Fail "[ERROR] rotate: string|vector expected")]
      [_ (Fail "[ERROR] rotate: integer expected")])))

(: string-to-vector-f core-signature)
(define (string-to-vector-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (StringT s))
       (s-un (VectorT (vector-map (lambda ([c : Char])
                                    (Unary (StringT (vector c)) (Nil)))
                                  s)))]
      [_ (Fail "[ERROR] string_to_vector: string expected")])))

(: index-f core-signature)
(define (index-f ev)
  (: f (All (a) (-> a (Mutable-Vectorof a) (Either (AST Type) String))))
  (define (f elem v)
    (let loop ([i : Integer 0]
               [acc : (Listof (AST Type)) '()])
      (cond
        [(>= i (vector-length v)) (s-un (VectorT (list->vector (reverse acc))))]
        [else (if (equal? elem (vector-ref v i))
                  (loop (add1 i) (cons (Unary (NumberT i) (Nil)) acc))
                  (loop (add1 i) acc))])))
  (lambda (tl ic e)
    (match tl
      [(list elem (VectorT v))
       (f (Unary elem (Nil)) v)]
      [(list (StringT elem) (StringT s))
       (if (> (vector-length elem) 1)
           (Fail "[ERROR] index: expected 1 char string")
           (f (vector-ref elem 0) s))]
      [_ (Fail "[ERROR] index: string|vector expected")])))

(: catalogue-f core-signature)
(define (catalogue-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (VectorT v) (VectorT w))
       (let ([nv : (Mutable-Vectorof (AST Type))
                 (make-vector (vector-length v) (Nil))])
         (let loop ([i : Integer 0]
                    [j : Integer 0]
                    [nw : (Mutable-Vectorof (AST Type))
                        (make-vector (vector-length w) (Nil))])
           (cond
             [(>= i (vector-length v)) (s-un (VectorT nv))]
             [(>= j (vector-length w)) (vector-set! nv i (Unary (VectorT nw) (Nil)))
                                       (loop (add1 i)
                                             0
                                             (make-vector (vector-length w) (Nil)))]
             [else
              (let ([c-vec : (Mutable-Vectorof (AST Type))
                           (vector (vector-ref v i)
                                   (vector-ref w j))])
                (vector-set! nw
                             j
                             (Unary (VectorT c-vec)
                                    (Nil)))
                (loop i (add1 j) nw))])))]
      [_ (Fail "[ERROR] catalogue: vector expected")])))

(: catalogue-with-f core-signature)
(define (catalogue-with-f ev)
  (lambda (tl ic e)
    (match tl
      [(list f (VectorT v) (VectorT w))
       (let ([nv : (Mutable-Vectorof (AST Type))
                 (make-vector (vector-length v) (Nil))])
         (let loop ([i : Integer 0]
                    [j : Integer 0]
                    [nw : (Mutable-Vectorof (AST Type))
                        (make-vector (vector-length w) (Nil))])
           (cond
             [(>= i (vector-length v)) (s-un (VectorT nv))]
             [(>= j (vector-length w)) (vector-set! nv i (Unary (VectorT nw) (Nil)))
                                       (loop (add1 i)
                                             0
                                             (make-vector (vector-length w) (Nil)))]
             [else
              (match (ev (Unary f
                                (append-ast (vector-ref v i)
                                            (vector-ref w j)))
                         ic
                         e)
                [(Fail x) (Fail x)]
                [(Ok x)
                 (vector-set! nw j x)
                 (loop i (add1 j) nw)])])))]
      [_ (Fail "[ERROR] catalogue_with: vector expected")])))

(: string-f core-signature)
(define (string-f ev)
  (lambda (tl ic e)
    (match tl
      [(list t) (s-un (StringT (list->vector (string->list (show-type t)))))])))

(: transpose-f core-signature)
(define (transpose-f ev)
  (lambda (tl ic e)
    (let ([ast
           (Unary
            (LambdaT
             '("tbl")
             '#hash()
             (Unary
              (GroupT
               (Unary
                (IdentifierT "map")
                (Unary
                 (GroupT
                  (Unary
                   (LambdaT
                    '("i")
                    '#hash()
                    (Unary
                     (GroupT
                      (Unary
                       (IdentifierT "map")
                       (Unary
                        (GroupT (Unary (IdentifierT "get")
                                       (Unary (IdentifierT "i") (Nil))))
                        (Nil))))
                     (Unary (IdentifierT "tbl") (Nil))))
                   (Nil)))
                 (Nil))))
              (Unary
               (IdentifierT "iota")
               (Unary
                (IdentifierT "length")
                (Unary (IdentifierT "get")
                       (Unary (NumberT 0) (Unary (IdentifierT "tbl") (Nil))))))))
            (Nil))])
      (match tl
        [(list tbl)
         (ev (append-ast ast (Unary tbl (Nil))) ic e)]))))

(: show-table-f core-signature)
(define (show-table-f ev)
  (lambda (tl ic e)
    (let ([ast
           (Unary
            (LambdaT
             '("tbl")
             '#hash()
             (Unary
              (GroupT
               (Unary
                (IdentifierT "reduce")
                (Unary
                 (GroupT
                  (Unary
                   (LambdaT
                    '("v" "a")
                    '#hash()
                    (Binary
                     (BinopT #f #f "&")
                     (Unary
                      (GroupT (Unary (IdentifierT "string")
                                     (Unary (IdentifierT "v") (Nil))))
                      (Nil))
                     (Binary
                      (BinopT #f #f "&")
                      (Unary (StringT (vector #\newline)) (Nil))
                      (Unary
                       (GroupT (Unary (IdentifierT "string")
                                      (Unary (IdentifierT "a") (Nil))))
                       (Nil)))))
                   (Nil)))
                 (Nil))))
              (Unary
               (GroupT
                (Unary
                 (IdentifierT "map")
                 (Unary
                  (GroupT
                   (Unary
                    (IdentifierT "reduce")
                    (Unary
                     (GroupT
                      (Unary
                       (LambdaT
                        '("v" "a")
                        '#hash()
                        (Binary
                         (BinopT #f #f "&")
                         (Unary
                          (GroupT (Unary (IdentifierT "string")
                                         (Unary (IdentifierT "v") (Nil))))
                          (Nil))
                         (Binary
                          (BinopT #f #f "&")
                          (Unary (StringT (vector #\tab)) (Nil))
                          (Unary
                           (GroupT (Unary (IdentifierT "string")
                                          (Unary (IdentifierT "a") (Nil))))
                           (Nil)))))
                       (Nil)))
                     (Nil))))
                  (Nil))))
               (Unary (IdentifierT "transpose")
                      (Unary (IdentifierT "tbl") (Nil))))))
            (Nil))])
      (match tl
        [(list tbl)
         (ev (append-ast ast (Unary tbl (Nil))) ic e)]))))

(: filter-f core-signature)
(define (filter-f ev)
  (lambda (tl ic e)
    (let ([ast
           (Unary
            (LambdaT
             '("p" "v")
             '#hash()
             (Unary
              (IdentifierT "replicate")
              (Unary
               (GroupT (Unary (IdentifierT "map")
                              (Unary (IdentifierT "p")
                                     (Nil))))
               (Unary (IdentifierT "v")
                      (Unary (IdentifierT "v")
                             (Nil))))))
            (Nil))])
      (match tl
        [(list p v)
         (ev (append-ast (Unary (GroupT (append-ast ast (Unary p (Nil)))) (Nil))
                         (Unary v (Nil)))
             ic e)]))))

(: string-to-number-f core-signature)
(define (string-to-number-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (StringT s))
       (let ([n (string->number (list->string (vector->list s)))])
         (if n
             (s-un (NumberT (cast n Real)))
             (Fail (format "[ERROR] string_to_number: invalid number conversion '~a'"
                           (list->string (vector->list s))))))]
      [_ (Fail "[ERROR] string_to_number: string expected")])))

(: string-split-f core-signature)
(define (string-split-f ev)
  (lambda (tl ic e)
    (match tl
      [(list (StringT s1) (StringT s2))
       (s-un (VectorT (list->vector
                       (map (lambda ([s : String])
                              (Unary (StringT (list->vector (string->list s))) (Nil)))
                            (string-split (list->string (vector->list s2))
                                          (list->string (vector->list s1)))))))]
      [_ (Fail "[ERROR] string_split: string expected")])))

(: unique-f core-signature)
(define (unique-f ev)
  (: f (All (a) (-> (Mutable-Vectorof a) (Mutable-Vectorof a))))
  (define (f v)
    (let loop ([s : (Setof a) (set)]
               [acc : (Listof a) '()]
               [lst : (Listof a) (vector->list v)])
      (cond
        [(null? lst) (list->vector (reverse acc))]
        [(set-member? s (car lst)) (loop s acc (cdr lst))]
        [else (loop (set-add s (car lst)) (cons (car lst) acc) (cdr lst))])))
  (lambda (tl ic e)
    (match tl
      [(list (VectorT v))
       (s-un (VectorT (f v)))]
      [(list (StringT s))
       (s-un (StringT (f s)))]
      [_ (Fail "[ERROR] unique: string|vector expected")])))

; get-many
; group_n
; slice
; amend (and amend_mutate?)
; index_where
; push (or diff name, like & but will promote to list)
; key
; intersection
; uppercase
; lowercase
; grade_up
; grade_down
; sort

(: binop-table (Immutable-HashTable String binop-signature))
(define binop-table
  (hash
   "+"   add
   "-"   minus
   "*"   mult
   "/"   div
   "^"   pow
   "%"   mod
   "<."  min-f
   ">."  max-f
   "="   eq
   "!="  neq
   "<"   lt
   "<="  le
   ">"   gt
   ">="  ge
   "or"  or-f
   "and" and-f
   "&"   concat-f))

(: core-table (Immutable-HashTable String (List Integer core-signature)))
(define core-table
  (hash
   "optional" (list 1 optional-f)
   "abs" (list 1 abs-f)
   "not" (list 1 not-f)
   "neg" (list 1 neg-f)
   "get" (list 2 get-f)
   "iota" (list 1 iota-f)
   "map" (list 2 map-f)
   "reduce" (list 2 reduce-f)
   "reverse" (list 1 reverse-f)
   "drop" (list 2 drop-f)
   "take" (list 2 take-f)
   "read_table" (list 1 read-table-f)
   "read_dsv" (list 3 read-dsv-f)
   "read_lines" (list 1 read-lines-f)
   "length" (list 1 length-f)
   "replicate" (list 2 replicate-f)
   "scan" (list 2 scan-f)
   "scan_n" (list 3 scan-n-f)
   "zip" (list 2 zip-f)
   "zip_with" (list 3 zip-with-f)
   "member" (list 2 member-f)
   "repeat" (list 3 repeat-f)
   "floor" (list 1 floor-f)
   "ceiling" (list 1 ceiling-f)
   "seq" (list 3 seq-f)
   "while" (list 3 while-f)
   "rotate" (list 2 rotate-f)
   "string_to_vector" (list 1 string-to-vector-f)
   "index" (list 2 index-f)
   "catalogue" (list 2 catalogue-f)
   "catalogue_with" (list 3 catalogue-with-f)
   "string" (list 1 string-f)
   "transpose" (list 1 transpose-f)
   "show_table" (list 1 show-table-f)
   "filter" (list 2 filter-f)
   "string_to_number" (list 1 string-to-number-f)
   "string_split" (list 2 string-split-f)
   "unique" (list 1 unique-f)))