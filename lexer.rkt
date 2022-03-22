#lang typed/racket
(require racket/match)
(require "either.rkt")
(provide TokenType Token lex)

(define-type TokenType
  (U 'identifier
     'number
     'bool
     'string
     'none
     'option
     'if
     'then
     'else
     'binop
     'abs
     'neg
     'not
     'backslash
     'open-paren
     'close-paren
     'open-square
     'close-square
     'colon
     'semicolon
     'comma
     'input
     'let
     'in))

(struct Token ([t : TokenType]
               [d : String]
               [l : Integer])
  #:transparent)

(: cons-lex (-> TokenType String (Listof Char) Integer (Either (Listof Token) String)))
(define (cons-lex tt s d line)
  (let ([r (lex d line)])
       (match r
         [(Ok x) (Ok (cons (Token tt s line) x))]
         [(Fail x) (Fail x)])))

(: string-lex (-> (Listof Char) Integer (Either (Listof Token) String)))
(define (string-lex cl line)
  (let loop ([c cl]
             [l line]
             [acc : (Listof Char) '()])
    (match c
      ['() (Fail (string-append "[ERROR] Unmatched ' at line " (format "~a" line)))]
      [`(#\newline . ,d) (loop d (+ l 1) (cons #\newline acc))]
      [`(#\\ #\n . ,d) (loop d l (cons #\newline acc))]
      [`(#\\ #\t . ,d) (loop d l (cons #\tab acc))]
      [`(#\\ #\r . ,d) (loop d l (cons #\return acc))]
      [`(#\\ ,a . ,d) (loop d l (cons a acc))]
      [`(#\' . ,d) (cons-lex 'string (list->string (reverse acc)) d l)]
      [`(,a . ,d) (loop d l (cons a acc))])))

(: number-lex (-> (Listof Char) Integer (Either (Listof Token) String)))
(define (number-lex cl line)
  (let loop ([c cl]
             [p : Boolean #f]
             [acc : (Listof Char) '()])
    (cond
      [(null? c) (Ok (list (Token 'number (list->string (reverse acc)) line)))]
      [(char-numeric? (car c)) (loop (cdr c) p (cons (car c) acc))]
      [(and (equal? (car c) #\.)
            (not p))
       (loop (cdr c) #t (cons (car c) acc))]
      [(and (equal? (car c) #\.)
            p)
       (Fail (string-append "[ERROR] Invalid number "
                            (list->string (reverse (cons #\. acc)))))]
      [else (cons-lex 'number (list->string (reverse acc)) c line)])))

(: invalid-set (Setof Char))
(define invalid-set (set #\# #\\ #\( #\) #\[ #\] #\: #\; #\, #\.
                         #\+ #\- #\* #\/ #\^ #\% #\< #\> #\= #\&
                         #\| #\~ #\!
                         #\space #\newline #\tab #\return))

(: decide-other (-> String Integer Token))
(define (decide-other s l)
  (match s
    ["true"   (Token 'bool s l)]
    ["false"  (Token 'bool s l)]
    ["if"     (Token 'if s l)]
    ["then"   (Token 'then s l)]
    ["else"   (Token 'else s l)]
    ["none"   (Token 'none s l)]
    ["option" (Token 'option s l)]
    ["or"     (Token 'binop s l)]
    ["and"    (Token 'binop s l)]
    ["input"  (Token 'input s l)]
    ["let"    (Token 'let s l)]
    ["in"     (Token 'in s l)]
    [_        (Token 'identifier s l)]))

(: other-lex (-> (Listof Char) Integer (Either (Listof Token) String)))
(define (other-lex cl line)
  (let loop ([c cl]
             [acc : (Listof Char) '()])
    (cond
      [(null? c) (Ok (list (decide-other (list->string (reverse acc)) line)))]
      [(set-member? invalid-set (car c))
       (match (decide-other (list->string (reverse acc)) line)
         [(Token tt s l) (cons-lex tt s c l)])]
      [else (loop (cdr c) (cons (car c) acc))])))

(: skip-comment (-> (Listof Char) Integer (Either (Listof Token) String)))
(define (skip-comment cl line)
  (match cl
    ['() (Ok '())]
    [`(#\newline . ,d) (skip-comment d (+ line 1))]
    [`(#\# . ,d) (lex d line)]
    [`(,a . ,d) (skip-comment d line)]))

(: lex (-> (Listof Char) Integer (Either (Listof Token) String)))
(define (lex cl line)
  (match cl
    ['() (Ok '())]
    [`(#\# . ,d)
     (skip-comment d line)]
    [`(#\newline . ,d)
     (lex d (+ line 1))]
    [`(#\space . ,d)
     (lex d line)]
    [`(#\tab . ,d)
     (lex d line)]
    [`(#\return . ,d)
     (lex d line)]
    [`(#\\ . ,d)
     (cons-lex 'backslash "\\" d line)]
    [`(#\( . ,d)
     (cons-lex 'open-paren "(" d line)]
    [`(#\) . ,d)
     (cons-lex 'close-paren ")" d line)]
    [`(#\[ . ,d)
     (cons-lex 'open-square "[" d line)]
    [`(#\] . ,d)
     (cons-lex 'close-square "]" d line)]
    [`(#\: . ,d)
     (cons-lex 'colon ":" d line)]
    [`(#\; . ,d)
     (cons-lex 'semicolon ";" d line)]
    [`(#\, . ,d)
     (cons-lex 'comma "," d line)]
    [`(#\+ . ,d)
     (cons-lex 'binop "+" d line)]
    [`(#\- . ,d)
     (cons-lex 'binop "-" d line)]
    [`(#\* . ,d)
     (cons-lex 'binop "*" d line)]
    [`(#\/ . ,d)
     (cons-lex 'binop "/" d line)]
    [`(#\^ . ,d)
     (cons-lex 'binop "^" d line)]
    [`(#\% . ,d)
     (cons-lex 'binop "%" d line)]
    [`(#\< #\. . ,d)
     (cons-lex 'binop "<." d line)]
    [`(#\> #\. . ,d)
     (cons-lex 'binop ">." d line)]
    [`(#\! #\= . ,d)
     (cons-lex 'binop "!=" d line)]
    [`(#\= . ,d)
     (cons-lex 'binop "=" d line)]
    [`(#\< #\= . ,d)
     (cons-lex 'binop "<=" d line)]
    [`(#\< . ,d)
     (cons-lex 'binop "<" d line)]
    [`(#\> #\= . ,d)
     (cons-lex 'binop ">=" d line)]
    [`(#\> . ,d)
     (cons-lex 'binop ">" d line)]
    [`(#\& . ,d)
     (cons-lex 'binop "&" d line)]
    [`(#\| . ,d)
     (cons-lex 'abs "|" d line)]
    [`(#\~ . ,d)
     (cons-lex 'neg "~" d line)]
    [`(#\! . ,d)
     (cons-lex 'not "!" d line)]
    [`(#\' . ,d)
     (string-lex d line)]
    [`(,a . ,d)
     (cond
       [(char-numeric? a)
        (number-lex (cons a d) line)]
       [else (other-lex (cons a d) line)])]))
