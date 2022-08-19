#lang typed/racket
(require "lexer.rkt")
(require "parser.rkt")
(require "ast.rkt")
(require "types.rkt")
(require "either.rkt")
(require "environment.rkt")
(require "builtin.rkt")
(provide eval-ast
         parse-statement parse-top-level add-statement build-top-level make-environment)

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

(define-type eval-signature 
  (-> (AST Type)
      (IContext Type)
      (Environment Type (AST Type))
      (Either (AST Type) String)))
  
(: eval-ast eval-signature)
(define (eval-ast ast icontext environment)
  (match ast
    [(Nil) (Ok (Nil))]
    [(Unary (IdentifierT _) _)
     (handle-identifier ast icontext environment)]
    [(Unary (LambdaT _ _ _) _)
     (handle-lambda ast icontext environment)]
    [(Unary (BuiltinT _ _ _) _)
     (handle-builtin ast icontext environment)]
    [(Unary (GroupT _) _)
     (handle-group ast icontext environment)]
    [(Unary (IfT _ _ _) _)
     (handle-if ast icontext environment)]
    [(Binary (SetT _ _) _ _)
     (handle-set ast icontext environment)]
    [(Binary (RefT _ _) _ _)
     (handle-ref ast icontext environment)]
    [(Unary (RefT _ _) _)
     (handle-ref ast icontext environment)]
    [(Unary (BinopT _ _ _) _)
     (handle-binop ast icontext environment)]
    [(Binary (BinopT _ _ _) _ _)
     (handle-binop ast icontext environment)]
    [(Unary (LiteralVectorT v) next)
     (handle-literal-vector ast icontext environment)]
    [(Unary (LiteralModuleT _) _)
     (handle-literal-module ast icontext environment)]
    [(Unary v next)
     (inject-either (lambda ([x : (AST Type)])
                      (Unary v x))
                    (eval-ast next icontext environment))]))

(: handle-identifier eval-signature)
(define (handle-identifier ast icontext environment)
  (match ast
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
                (let ([result3 (eval-ast p (hash) environment)])
                  (match result3
                    [(Ok nv)
                     (hash-set! (Environment-defs environment)
                                name
                                (Def nv #f p))
                     (eval-ast (append-ast nv next) icontext environment)]
                    [(Fail x) (Fail x)]))]))))]))

(: handle-lambda eval-signature)
(define (handle-lambda ast icontext environment)
  (match ast
    [(Unary (LambdaT (list) nic body) next)
     (let ([nb (eval-ast body (combine-icontext nic icontext) environment)]
           [next2 (eval-ast next icontext environment)])
       (match (list nb next2)
         [(list (Fail x) _) (Fail x)]
         [(list _ (Fail y)) (Fail y)]
         [(list (Ok (Unary (LambdaT args nic2 body) (Nil))) (Ok y))
          (eval-ast (append-ast (Unary (LambdaT args nic2 body) (Nil)) y)
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
         [(Ok next3)
          (let loop ([n : (AST Type) next3]
                     [a : (Listof String) args]
                     [nnic : (IContext Type) nic])
            (match (list a n)
              [(list '() next4)
               (eval-ast (Unary (LambdaT '() nnic body) n) icontext environment)]
              [(list a2 (Nil))
               (Ok (Unary (LambdaT a2 nnic body) (Nil)))]
              [(list `(,a2 . ,d) (Unary t r))
               (loop r d (hash-set nnic a2 t))]))]))]))

(: handle-builtin eval-signature)
(define (handle-builtin ast icontext environment)
  (match ast
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
                   (eval-ast (Unary (BuiltinT (second b)
                                              (third b)
                                              body)
                                    (first b))
                             icontext
                             environment))]
         [(Fail x) (Fail x)]))]))

(: handle-group eval-signature)
(define (handle-group ast icontext environment)
  (match ast
    [(Unary (GroupT g) next)
     (let ((result (eval-ast g icontext environment)))
       (match result
         [(Ok x) (let ([next2 (eval-ast next icontext environment)])
                   (match next2
                     [(Ok y) (eval-ast (append-ast x y) icontext environment)]
                     [(Fail y) (Fail y)]))]
         [(Fail x) (Fail x)]))]))

(: handle-if eval-signature)
(define (handle-if ast icontext environment)
  (match ast
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
          (eval-ast (append-ast t next) icontext environment)]))]))

(: handle-set eval-signature)
(define (handle-set ast icontext environment)
  (match ast
    ; TODO: SETTING
    [(Binary (SetT #f #f) (Binary (RefT #f #f) lhs rhs) value)
     (let ([left (eval-ast lhs icontext environment)])
       (match left
         [(Ok (Unary (ModuleT e) (Nil)))
          (match rhs
            [(Unary (IdentifierT name) next)
             (match (eval-ast value icontext environment)
               [(Fail x) (Fail x)]
               [(Ok x)
                (match (add-statement (Definition name
                                        (Unary (LambdaT '() (hash) x)
                                               (Nil)))
                                      e)
                  [(Fail x) (Fail x)]
                  [_ (match (list e environment)
                       [(list (Environment _ _ dependsM)
                              (Environment _ _ depends))
                        (let loop ([keys : (Listof String)
                                         (apply append
                                          (map (lambda ([key : String])
                                                 (get-dependants key depends))
                                               (get-dependants name dependsM)))])
                          (cond
                            [(null? keys) (Ok #t)]
                            [else (set-Def-recalc! (hash-ref (Environment-defs environment) (car keys)) #t)
                                  (loop (cdr keys))]))])
                     (Ok (Unary (ModuleT e) next))])])]
            [_
             (Fail "[ERROR] expected identifier when referencing in a module")])]
         [_
          (let ([right (eval-ast rhs icontext environment)])
            (match (list left right)
              [(list (Fail l) _) (Fail l)]
              [(list _ (Fail r)) (Fail r)]
              [(list (Ok (Unary l (Nil))) (Ok (Unary r (Nil))))
               (eval-ast (Binary (SetT #f #f) (Unary (RefT l r) (Nil)) value)
                                 icontext environment)]))]))]
    [(Binary (SetT #f #f) (Unary (RefT x y) next) value)
     (match (list x y)
       [(list (VectorT v) (NumberT n))
        (if (integer? n)
              (match (eval-ast value icontext environment)
                [(Fail x) (Fail x)]
                [(Ok x) (vector-set! v (cast n Integer) x)
                        (eval-ast (Unary (VectorT v) next)
                                  icontext environment)])
            (Fail "[ERROR] integer expected when setting vector"))]
       [_
        (Fail "[ERROR] set type mismatch")])]))

(: handle-ref eval-signature)
(define (handle-ref ast icontext environment)
  (match ast
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
        (Fail "[ERROR] reference type mismatch")])]))

(: handle-binop eval-signature)
(define (handle-binop ast icontext environment)
  (match ast
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
     (let ([result ((hash-ref binop-table b) l r icontext environment)])
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
          (Fail "[ERROR] multi-value expression on left side of infix operator")]))]))

(: handle-literal-vector eval-signature)
(define (handle-literal-vector ast icontext environment)
  (match ast
    [(Unary (LiteralVectorT v) next)
     (let ([nv (eval-vector v icontext environment)])
       (let ([next2 (eval-ast next icontext environment)])
         (match (list nv next2)
           [(list (Ok x) (Ok y)) (Ok (Unary (VectorT x) y))]
           [(list (Ok x) (Fail y)) (Fail y)]
           [(list (Fail x) _) (Fail x)])))]))

(: handle-literal-module eval-signature)
(define (handle-literal-module ast icontext environment)
  (match ast
    [(Unary (LiteralModuleT _) _)
     (Fail "[ERROR] modules can only be used in definitions")]))

;; STATEMENTS

(define-type Statement
  (U Input
     Definition
     Import))

(struct Input
  ([name : String]
   [validation-vector : (Mutable-Vectorof (AST Type))])
  #:transparent)

(struct Definition
  ([name : String]
   [body : (AST Type)])
  #:transparent)

(struct Import
  ([filename : String]
   [alias : (Option String)])
  #:transparent)

(: parse-statement (-> (Listof Token) (Either Statement String)))
(define (parse-statement tl)
  (match tl
    [`(,(Token 'input _ _) ,(Token 'identifier name _) . ())
     (Ok (Input name (vector)))]
    [`(,(Token 'input _ _) ,(Token 'identifier name _) ,(Token 'semicolon _ _) . ,d)
     (Ok (Input name (vector)))]
    [`(,(Token 'input _ _) ,(Token 'identifier name _) ,(Token 'colon _ _)
                           ,(Token 'open-square s l) . ,d)
     (match (parse (cons (Token 'open-square s l) d))
       [(Fail x) (Fail x)]
       [(Ok (Unary (LiteralVectorT vv) (Nil)))
        (Ok (Input name vv))]
       [(Ok (Unary (LiteralVectorT _) _))
        (Fail (string-append "[ERROR] invalid expression after validation vector on line "
                             (format "~a" l)))]
       [(Ok _)
        (Fail (string-append "[ERROR] invalid validation vector on line "
                             (format "~a" l)))])]
    [`(,(Token 'input _ line) . ,d)
     (Fail (string-append "[ERROR] malformed input on line " (format "~a" line)))]
    [`(,(Token 'import _ line) ,(Token 'string filename _) . ())
     (Ok (Import filename #f))]
    [`(,(Token 'import _ line) ,(Token 'string filename _) ,(Token 'semicolon _ _) . ,d)
     (Ok (Import filename #f))]
    [`(,(Token 'import _ line) ,(Token 'string filename _)
                               ,(Token 'as _ _) ,(Token 'identifier alias _) . ())
     (Ok (Import filename alias))]
    [`(,(Token 'import _ line) ,(Token 'string filename _)
                               ,(Token 'as _ _) ,(Token 'identifier alias _)
                               ,(Token 'semicolon _ _) . ,d)
     (Ok (Import filename alias))]
    [`(,(Token 'import _ line) . _)
     (Fail (string-append "[ERROR] malformed input on line " (format "~a" line)))]
    [`(,(Token 'identifier name line) . ,d)
     (match (parse-lambda d line)
       [(Fail x) (Fail x)]
       [(Ok (Unary (LambdaT args ic (Unary (LiteralModuleT mts) (Nil))) (Nil)))
        (let ([env (make-environment)])
          (match (build-top-level (parse-top-level mts) env)
            [(Fail x) (Fail x)]
            [(Ok _)
             (Ok (Definition name
                   (Unary (LambdaT args
                                   ic
                                   (Unary (ModuleT env)
                                          (Nil)))
                          (Nil))))]))]
       [(Ok x) (Ok (Definition name x))])]
    [_ (Fail "[ERROR] invalid statement")]))
    
(: split-top-level (-> (Listof Token) (Listof (Listof Token))))
(define (split-top-level tl)
  (let loop ([t : (Listof Token) tl]
             [temp : (Listof Token) '()]
             [acc : (Listof (Listof Token)) '()])
    (match t
      ['()
       (if (null? temp)
           (reverse acc)
           (reverse (cons (reverse temp) acc)))]
      [`(,(Token 'open-curl s l) . ,d)
       (match (depth-take d 0 'open-curl 'close-curl
                          (string-append "[ERROR] unmatched { on line "
                                         (format "~a" l)))
         [(Fail _) (loop '() (append (reverse (cons (Token 'open-curl s l) d)) temp) acc)]
         [(Ok (list m-tl tld))
          (loop tld
                (append
                 (reverse (append (list (Token 'open-curl s l))
                                  m-tl
                                  (list (Token 'close-curl "}" l))))
                 temp)
                acc)])]
      [`(,(Token 'semicolon _ _) . ,d)
       (if (null? temp)
           (loop d '() acc)
           (loop d '() (cons (reverse temp) acc)))]
      [`(,a . ,d)
       (loop d (cons a temp) acc)])))

(: parse-top-level (-> (Listof Token) (Listof (Either Statement String))))
(define (parse-top-level tl)
  (map parse-statement (split-top-level tl)))

(: check-input-validity (-> Statement (Environment Type (AST Type))
                            (Either True String)))
(define (check-input-validity def env)
  (match (list def env)
    [(list (Definition name (Unary (NoneT) (Nil))) _)
     (Ok #t)]
    [(list (Definition name body) (Environment inputs _ _))
     (let [(vv (hash-ref inputs name))]
       (let loop ([i : Integer 0])
         (cond
           [(>= i (vector-length vv)) (Ok #t)]
           [else (match (eval-ast (append-ast (vector-ref vv i) body)
                                  (hash)
                                  env)
                   [(Fail x)
                    (Fail (string-append "[ERROR] validation of "
                                         name
                                         " failed at validation vector index "
                                         (format "~a" i)
                                         " with error {" x "}"))]
                   [(Ok (Unary (BoolT #f) _))
                    (Fail (string-append "[ERROR] validation of "
                                         name
                                         " failed at validation vector index "
                                         (format "~a" i)))]
                   [(Ok _)
                    (loop (add1 i))])])))]))

(: add-statement (-> Statement (Environment Type (AST Type))
                     (Either True String)))
(define (add-statement s e)
  (: ast-is-function? (-> (AST Type) Boolean))
  (define (ast-is-function? ast)
    (match ast
      [(Unary (LambdaT (list) _ _) _) #f]
      [(Unary (LambdaT _ _ _) _) #t]
      [_ #f]))
  (match (list s e)
    [(list (Input name validation-vector) (Environment inputs defs depends))
     (hash-set! inputs name validation-vector)
     (hash-set! defs
                name
                (ann (Def (Nil) #t (Unary (NoneT) (Nil)))
                     (Def Type)))
     (Ok #t)]
    [(list (Import filename #f) env)
     (if (not (file-exists? filename))
         (Fail (string-append "[ERROR] "
                              filename
                              " does not exist"))
         (match (lex (string->list (file->string filename)) 1)
           [(Fail x) (Fail x)]
           [(Ok x)
            (build-top-level (parse-top-level x) env)]))]
    [(list (Import filename alias) env)
     (assert alias) ; not false due to above match condition
     (if (not (file-exists? filename))
         (Fail (string-append "[ERROR] "
                              filename
                              " does not exist"))
         (match (lex (string->list (file->string filename)) 1)
           [(Fail x) (Fail x)]
           [(Ok x)
            (let ([e (make-environment)])
              (match (build-top-level (parse-top-level x) e)
                [(Fail x) (Fail x)]
                [(Ok _)
                 (add-statement (Definition alias
                                  (Unary (LambdaT '() (hash) (Unary (ModuleT e) (Nil)))
                                         (Nil)))
                                env)]))]))]
    [(list (Definition name body) (Environment inputs defs depends))
     (cond
       [(and (not (hash-ref inputs name #f))
             (or (not (hash-ref defs name #f))
                 (ast-is-function? body)))
        (hash-set! defs name (Def (Nil) #t body))
        (let loop ([deps : (Listof String) ((depends-ast depends-type) body)])
          (cond
            [(null? deps) (void)]
            [else (let ([known-deps : (U (Setof String) #f)
                                    (hash-ref depends (car deps) #f)])
                    (if known-deps
                        (hash-set! depends (car deps) (set-add known-deps name))
                        (hash-set! depends (car deps) (set name))))
                  (loop (cdr deps))]))
        (Ok #t)]
       [(and (not (hash-ref inputs name #f))
             (hash-ref defs name #f))
        (Fail (format "[ERROR] definition of '~a' already exists as non-input" name))]
       [else
        (match (check-input-validity s e)
          [(Fail x) (Fail x)]
          [(Ok _)
           (let ([result (eval-ast body (hash) (Environment inputs defs depends))])
             (match result
               [(Fail x) (Fail x)]
               [(Ok x)
                (hash-set! defs name (Def x #f x))
                (let loop ([keys (get-dependants name depends)])
                  (cond
                    [(null? keys) (Ok #t)]
                    [else (set-Def-recalc! (hash-ref (Environment-defs e) (car keys)) #t)
                          (loop (cdr keys))]))]))])])]))

(: build-top-level (-> (Listof (Either Statement String)) (Environment Type (AST Type))
                       (Either True String)))
(define (build-top-level sl env)
  (let loop ([s : (Listof (Either Statement String)) sl])
    (match s
      ['() (Ok #t)]
      [`(,(Fail y) . ,d) (Fail y)]
      [`(,(Ok statement) . ,d)
       (match (add-statement statement env)
         [(Fail z) (Fail z)]
         [(Ok _) (loop d)])])))

(: make-environment (-> (Environment Type (AST Type))))
(define (make-environment)
  (Environment
   (ann (make-hash)
        (Mutable-HashTable String (Mutable-Vectorof (AST Type))))
   (ann (make-hash)
        (Mutable-HashTable String (Def Type)))
   (ann (make-hash)
        (Mutable-HashTable String (Setof String)))))
