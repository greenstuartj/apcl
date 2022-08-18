#lang typed/racket
(require racket/match)
(require "either.rkt")
(require "types.rkt")
(require "ast.rkt")
(provide read-dsv)

(: to-type (-> (Listof Char) Boolean (AST Type)))
(define (to-type cl np)
  (Unary
   (cond
     [(null? cl)
      (NoneT)]
     [(not np)
      (StringT (list->vector cl))]
     [(not (or (char-numeric? (car cl))
               (eq? #\- (car cl))))
      (StringT (list->vector cl))]
     [else (let ([n (string->number (list->string cl))])
             (if n
                 (NumberT (cast n Real))
                 (StringT (list->vector cl))))])
   (Nil)))
  
(: parse-cell (-> (Listof Char) Char Char Boolean
                  (List Boolean (AST Type) (Listof Char))))
(define (parse-cell cl sp qt np)
  (let loop ([c : (Listof Char) cl]
             [q : Boolean #f]
             [acc : (Listof Char) '()])
    (cond
      [(null? c)
       (list #t (to-type (reverse acc) np) '())]
      [(and (eq? (car c) qt)
            q)
       (loop (cdr c) #f acc)]
      [q
       (loop (cdr c) q (cons (car c) acc))]
      [(and (eq? (car c) qt)
            (not q))
       (loop (cdr c) #t acc)]
      [(eq? (car c) sp)
       (list #f (to-type (reverse acc) np) (cdr c))]
      [(eq? (car c) #\return)
       (loop (cdr c) q acc)]
      [(eq? (car c) #\newline)
       (list #t (to-type (reverse acc) np) (cdr c))]
      [else (loop (cdr c) q (cons (car c) acc))])))

(: parse-line (-> (Listof Char) Char Char Boolean
                  (List Boolean (Listof (AST Type)) (Listof Char))))
(define (parse-line cl sp qt np)
  (let loop ([c : (Listof Char) cl]
             [acc : (Listof (AST Type)) '()])
    (match (parse-cell c sp qt np)
      [(list #t t '()) (list #t (reverse (cons t acc)) '())]
      [(list #t t next) (list #f (reverse (cons t acc)) next)]
      [(list #f t next) (loop next (cons t acc))])))

(: process-lists (-> (Listof (Mutable-Vectorof (AST Type)))
                     (AST Type)))
(define (process-lists rows)
  (: len Integer)
  (define len (length rows))
  (: row-len Integer)
  (define row-len (vector-length (car rows)))
  (: table (Mutable-Vectorof (AST Type)))
  (define table (make-vector row-len (Nil)))
  (: prep-col (-> Integer (AST Type)))
  (define (prep-col i)
    (: col (Mutable-Vectorof (AST Type)))
    (define col (make-vector len (Nil)))
    (let loop ([j : Integer 0]
               [rs : (Listof (Mutable-Vectorof (AST Type))) rows])
      (cond
        [(= j len) (Unary (VectorT col) (Nil))]
        [(>= i (vector-length (car rs)))
         (vector-set! col j (Unary (NoneT) (Nil)))
         (loop (add1 i) rs)]
        [else (let ([elem (vector-ref (car rs) i)])
                (vector-set! col j elem))
              (loop (add1 j) (cdr rs))])))
  (let loop ([i : Integer 0])
    (cond
      [(= i row-len) (Unary (VectorT table) (Nil))]
      [else (let ([col (prep-col i)])
              (vector-set! table i col))
            (loop (add1 i))])))

(: read-dsv (-> String Char Char Boolean String
                (Either (AST Type) String)))
(define (read-dsv fn sp qt np err-name)
  (if (not (file-exists? fn))
      (Fail (string-append "[ERROR] " err-name ": does not exist in directory"))
      (match (parse-line (string->list (file->string fn))
                         sp qt np)
        [(list #t head _)
         (let ([header : (Mutable-Vectorof (AST Type))
                       (list->vector
                        (map (lambda ([t : (AST Type)])
                               (Unary (VectorT (vector t)) (Nil)))
                             head))])
           (Ok (Unary (VectorT header) (Nil))))]
        [(list #f head next)
         (let loop ([acc : (Listof (Mutable-Vectorof (AST Type))) '()]
                    [n : (Listof Char) next])
           (match (parse-line n sp qt np)
             [(list #t line _)
              (Ok (process-lists (cons (list->vector head)
                                       (reverse (cons (list->vector line) acc)))))]
             [(list #f line n2)
              (loop (cons (list->vector line) acc) n2)]))])))

                
