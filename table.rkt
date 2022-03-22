#lang typed/racket
(require racket/match)
(require "either.rkt")
(require "types.rkt")
(require "ast.rkt")
(provide read-dsv)

(: to-type (-> (Listof Char) Boolean (AST Type)))
(define (to-type cl np)
  (cond
    [(null? cl)
     (Unary (NoneT) (Nil))]
    [(not np)
     (Unary (StringT (list->vector cl)) (Nil))]
    [(not (or (char-numeric? (car cl))
              (eq? #\- (car cl))))
     (Unary (StringT (list->vector cl)) (Nil))]
    [else (let ([n (string->number (list->string cl))])
            (if n
                (Unary (NumberT (cast n Real)) (Nil))
                (Unary (StringT (list->vector cl)) (Nil))))]))

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

(: vector-push-back (-> (AST Type) (AST Type)
                        (AST Type)))
(define (vector-push-back a b)
  (match a
    [(Unary (VectorT v) (Nil))
     (Unary (VectorT (vector-append v
                                    (ann (vector b)
                                         (Mutable-Vectorof (AST Type)))))
            (Nil))]))

(: append-line (-> (Mutable-Vectorof (AST Type)) (Listof (AST Type)) Integer
                   (Either (Mutable-Vectorof (AST Type)) String)))
(define (append-line head line i)
  (let loop ([l : (Listof (AST Type)) line]
             [j : Integer 0]
             [h : (Mutable-Vectorof (AST Type)) head])
    (cond
      [(>= j (vector-length h))
       (Ok h)]
      [(null? l)
       (Fail (string-append "[ERROR] malformed table on line "
                            (format "~a" i)
                            ". Number of columns fewer than header"))]
      [else
       (vector-set! h j (vector-push-back (vector-ref h j)
                                          (car l)))
       (loop (cdr l) (add1 j) h)])))

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
         (let ([header : (Mutable-Vectorof (AST Type))
                       (list->vector
                        (map (lambda ([t : (AST Type)])
                               (Unary (VectorT (vector t)) (Nil)))
                             head))])
           (let loop ([n : (Listof Char) next]
                      [i : Integer 2])
             (match (parse-line n sp qt np)
               [(list #t line _)
                (match (append-line header line i)
                  [(Fail x) (Fail x)]
                  [(Ok x) (Ok (Unary (VectorT x) (Nil)))])]
               [(list #f line n2)
                (match (append-line header line i)
                  [(Fail x) (Fail x)]
                  [_ (loop n2 (add1 i))])])))])))
                
