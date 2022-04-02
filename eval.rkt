#lang typed/racket
(require "ast.rkt")
(require "types.rkt")
(require "either.rkt")
(require "environment.rkt")
(require "builtin.rkt")
(provide eval-ast)

(: collect-types
   (-> (AST Type) Integer (Listof Type) (List (AST Type) Integer (Listof Type))))
(define (collect-types ast n lst)
  (match (list ast n)
    [(list (Nil) _) (list (Nil) n lst)]
    [(list ast2 0) (list ast2 0 lst)]
    [(list (Unary t next) _) (collect-types next (- n 1) (cons t lst))]
    [(list (Binary t _ rhs) _) (collect-types rhs (- n 1) (cons t lst))]))

(: eval-vector
   (-> (Mutable-Vectorof (AST Type)) (IContext Type) (Environment Type (AST Type))
       (Either (Mutable-Vectorof (AST Type)) String)))
(define (eval-vector v icontext environment)
  (: nv (Mutable-Vectorof (AST Type)))
  (define nv (make-vector (vector-length v) (Unary (NoneT) (Nil))))
  (let loop ([i : Integer 0])
    (cond
      [(>= i (vector-length nv)) (Ok nv)]
      [else (let ([result (eval-ast (vector-ref v i) icontext environment)])
              (match result
                [(Fail x) (Fail x)]
                [(Ok x) (vector-set! nv i x)
                        (loop (+ i 1))]))])))

(: combine-icontext (-> (IContext Type) (IContext Type) (IContext Type)))
(define (combine-icontext ic1 ic2)
  (let loop ([ks (hash-keys ic1)]
             [ic3 : (IContext Type) ic2])
    (cond
      [(null? ks) ic3]
      [else (loop (cdr ks) (hash-set ic3 (car ks) (hash-ref ic1 (car ks))))])))

(: eval-ast
   (-> (AST Type)
       (IContext Type)
       (Environment Type (AST Type))
       (Either (AST Type) String)))
(define (eval-ast ast icontext environment)
  (match ast
    [(Nil) (Ok (Nil))]
    [(Unary (IdentifierT name) next)
     (let ([result (hash-ref icontext name #f)])
       (if result
           (eval-ast (Unary result next) icontext environment)
           (let ([result2 (hash-ref (Environment-defs environment) name #f)])
             (match result2
               [#f
                (let ([result3 (hash-ref core-table name #f)])
                  (match result3
                    [(list n b)
                      (eval-ast (Unary (BuiltinT n '() (b eval-ast)) next)
                                icontext
                                environment)]
                    [_
                      (Fail (string-append "[ERROR] " name " is undefined"))]))]
               [(Def v #f _)
                (eval-ast (append-ast v next) icontext environment)]
               [(Def _ #t p)
                (let ([result3 (eval-ast p icontext environment)])
                  (match result3
                    [(Ok nv)
                     (hash-set! (Environment-defs environment)
                                name
                                (Def nv #f p))
                     (eval-ast (append-ast nv next) icontext environment)]
                    [(Fail x) (Fail x)]))]))))]
    [(Unary (LambdaT (list) nic body) next)
     (let ([nb (eval-ast body (combine-icontext nic icontext) environment)]
           [next2 (eval-ast next icontext environment)])
       (match (list nb next2)
         [(list (Fail x) _) (Fail x)]
         [(list _ (Fail y)) (Fail y)]
         [(list (Ok (Unary (LambdaT args nic2 body) (Nil))) (Ok y))
          (eval-ast (append-ast (Unary (LambdaT args nic body) (Nil)) y)
                    icontext environment)]
         [(list (Ok x) (Ok y))
          (eval-ast (append-ast x y) icontext environment)]))]
    [(Unary (LambdaT args nic body) (Nil))
     (Ok (Unary (LambdaT args nic body) (Nil)))]
    [(Unary (LambdaT args nic body) next)
     (let ([next2 (eval-ast next icontext environment)])
       (match next2
         [(Fail x) (Fail x)]
         [(Ok (Nil)) (Ok (Unary (LambdaT args nic body) (Nil)))]
         [(Ok next3) (let loop ([n : (AST Type) next3]
                                [a : (Listof String) args]
                                [nnic : (IContext Type) nic])
                       (match (list a n)
                         [(list '() next4)
                          (eval-ast (Unary (LambdaT '() nnic body) n) icontext environment)]
                         [(list a2 (Nil))
                          (Ok (Unary (LambdaT a2 nnic body) (Nil)))]
                         [(list `(,a2 . ,d) (Unary t r))
                          (loop r d (hash-set nnic a2 t))]))]))]
    [(Unary (BuiltinT 0 args body) next)
     (inject-either
      (lambda ([x : (AST Type)])
        (append-ast x next))
      (body (reverse args) icontext environment))]
    [(Unary (BuiltinT n args body) (Nil))
     (Ok (Unary (BuiltinT n args body) (Nil)))]
    [(Unary (BuiltinT n args body) next)
     (let ((result (eval-ast next icontext environment)))
       (match result
         [(Ok x) (let ((b (collect-types x n args)))
                   (eval-ast (Unary (BuiltinT (second b) (third b) body) (first b))
                             icontext
                             environment))]
         [(Fail x) (Fail x)]))]
    [(Unary (GroupT g) next)
     (let ((result (eval-ast g icontext environment)))
       (match result
         [(Ok x) (let ([next2 (eval-ast next icontext environment)])
                   (match next2
                     [(Ok y) (eval-ast (append-ast x y) icontext environment)]
                     [(Fail y) (Fail y)]))]
         [(Fail x) (Fail x)]))]
    [(Unary (IfT test t f) next)
     (let ([result (eval-ast test icontext environment)])
       (match result
         [(Fail x) (Fail x)]
         [(Ok (Unary (BoolT #f) _))
          (eval-ast (append-ast f next) icontext environment)]
         [(Ok (Unary (NoneT) _))
          (eval-ast (append-ast f next) icontext environment)]
         [(Ok (Unary (OptionT) _))
          (eval-ast (append-ast f next) icontext environment)]
         [_
          (eval-ast (append-ast t next) icontext environment)]))]
    ; TODO: modules
    [(Binary (RefT #f #f) lhs rhs)
     (let ([left (eval-ast lhs icontext environment)])
       (match left
         [(Ok (Unary (ModuleT e) (Nil)))
          (match rhs
            [(Unary (IdentifierT name) next)
             (match (eval-ast next icontext environment)
               [(Fail x) (Fail x)]
               [(Ok x)
                (eval-ast (Unary (IdentifierT name) x) icontext e)])]
            [_
             (Fail "[ERROR] expected identifier when referencing in a module")])]
         [_
          (let ([right (eval-ast rhs icontext environment)])
            (match (list left right)
              [(list (Fail l) _) (Fail l)]
              [(list _ (Fail r)) (Fail r)]
              [(list (Ok (Unary l (Nil))) (Ok (Unary r (Nil))))
               (eval-ast (Unary (RefT l r) (Nil)) icontext environment)]))]))]
    [(Unary (RefT x y) next)
     (match (list x y)
       [(list (VectorT v) (NumberT n))
        (if (integer? n)
            (eval-ast (append-ast (vector-ref v (cast n Integer)) next)
                      icontext environment)
            (Fail "[ERROR] integer expected when referencing vector"))]
       [_
        (displayln (list x y))
        (Fail "[ERROR] reference type mismatch")])]
    [(Unary (BinopT #f #f b) next)
     (let ([next2 (eval-ast next icontext environment)])
       (match next2
         [(Fail x) (Fail x)]
         [(Ok (Nil))
          (Ok (Unary (BinopT #f #f b) (Nil)))]
         [(Ok (Unary x (Nil)))
          (Ok (Unary (BinopT x #f b) (Nil)))]
         [(Ok (Unary x (Unary y next3)))
          (eval-ast (Unary (BinopT x y b) next3) icontext environment)]))]
    [(Unary (BinopT l #f b) next)
     (let ([next2 (eval-ast next icontext environment)])
       (match next2
         [(Fail x) (Fail x)]
         [(Ok (Nil))
          (Ok (Unary (BinopT l #f b) (Nil)))]
         [(Ok (Unary x next3))
          (eval-ast (Unary (BinopT l x b) next3) icontext environment)]))]
    [(Unary (BinopT #f r b) next)
     (let ([next2 (eval-ast next icontext environment)])
       (match next2
         [(Fail x) (Fail x)]
         [(Ok (Nil))
          (Ok (Unary (BinopT #f r b) (Nil)))]
         [(Ok (Unary x next3))
          (eval-ast (Unary (BinopT x r b) next3) icontext environment)]))]
    [(Unary (BinopT l r b) next)
     (assert l) ; not #f due to above match cases
     (assert r) ; not #f due to above match cases
     (let ([result (b l r icontext environment)])
       (match result
         [(Fail x) (Fail x)]
         [(Ok x) (eval-ast (append-ast x next) icontext environment)]))]
    [(Binary (BinopT #f #f b) lhs rhs)
     (let ([left (eval-ast lhs icontext environment)]
           [right (eval-ast rhs icontext environment)])
       (match (list left right)
         [(list (Fail l) _) (Fail l)]
         [(list _ (Fail r)) (Fail r)]
         [(list (Ok (Nil)) (Ok (Nil)))
          (Ok (Unary (BinopT #f #f b) (Nil)))]
         [(list (Ok (Unary l (Nil))) (Ok (Nil)))
          (Ok (Unary (BinopT l #f b) (Nil)))]
         [(list (Ok (Nil)) (Ok (Unary r next)))
          (eval-ast (Unary (BinopT #f r b) next) icontext environment)]
         [(list (Ok (Unary l (Nil))) (Ok (Unary r next)))
          (eval-ast (Unary (BinopT l r b) next) icontext environment)]
         [_
          (Fail "[ERROR] multi-value expression on left side of infix operator")]))]
    [(Unary (LiteralVectorT v) next)
     (let ([nv (eval-vector v icontext environment)])
       (let ([next2 (eval-ast next icontext environment)])
         (match (list nv next2)
           [(list (Ok x) (Ok y)) (Ok (Unary (VectorT x) y))]
           [(list (Ok x) (Fail y)) (Fail y)]
           [(list (Fail x) _) (Fail x)])))]
    [(Unary v next)
     (inject-either (lambda ([x : (AST Type)])
                      (Unary v x))
                    (eval-ast next icontext environment))]))
