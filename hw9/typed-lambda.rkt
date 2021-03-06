#lang plai-typed
(require plai-typed/s-exp-match)

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [consV (car : Value)
         (cdr : Value)]
  [closV (arg : (listof symbol))
         (body : ExprC)
         (env : Env)])

(define-type ExprC
  [numC (n : number)]
  [consC (car : ExprC)
         (cdr : ExprC)]
  [carC (cons : ExprC)]
  [cdrC (cons : ExprC)]
  [boolC (b : boolean)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [lamC (n : (listof symbol))
        (arg-type : (listof Type))
        (body : ExprC)]
  [appC (fun : ExprC)
        (arg : (listof ExprC))]
  [eqC (l : ExprC)
       (r : ExprC)]
  [ifC (cnd : ExprC)
       (thn : ExprC)
       (els : ExprC)])

(define-type Type
  [numT]
  [boolT]
  [consT (car : Type)
         (cdr : Type)]
  [arrowT (arg : (listof Type))
          (result : Type)])

(define-type Binding
  [bind (name : symbol)
        (val : Value)])

(define-type-alias Env (listof Binding))

(define-type TypeBinding
  [tbind (name : symbol)
         (type : Type)])

(define-type-alias TypeEnv (listof TypeBinding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors true))

;; parse ----------------------------------------
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-match? `true s) (boolC true)]
    [(s-exp-match? `false s) (boolC false)]
    [(s-exp-match? `NUMBER s) (numC (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idC (s-exp->symbol s))]
    [(s-exp-match? '{+ ANY ANY} s)
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{= ANY ANY} s)
     (let ([ls (s-exp->list s)])
       (eqC (parse (second ls))
            (parse (third ls))))]
    
    [(s-exp-match? '{if ANY ANY ANY} s)
     (let ([ls (s-exp->list s)])
       (ifC (parse (second ls))
            (parse (third ls))
            (parse (fourth ls))))]
    
    [(s-exp-match? '{cons ANY ANY} s)
     (let ([ls (s-exp->list s)])
       (consC (parse (second ls))
              (parse (third ls))))]
    
    [(s-exp-match? '{first ANY} s)
     (carC (parse (second (s-exp->list s))))]
    
    [(s-exp-match? '{rest ANY} s)
     (cdrC (parse (second (s-exp->list s))))]
    
    
    [(s-exp-match? '{* ANY ANY} s)
     (multC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{let {[SYMBOL : ANY ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appC (lamC (list (s-exp->symbol (first bs)))
                   (list (parse-type (third bs)))
                   (parse (third (s-exp->list s))))
             (list (parse (fourth bs)))))]
    
    [(s-exp-match? '{lambda {[SYMBOL : ANY] ...} ANY} s)
     (let* ([ls (s-exp->list s)]
            [body (parse (third ls))]
            [arglist (s-exp->list (second ls))]
            [args (map (λ (a)
                         (s-exp->symbol (first (s-exp->list a))))
                       arglist)]
            [types (map (λ (a)
                          (parse-type (third (s-exp->list a))))
                        arglist)])
       (lamC args
             types
             body))]
    [(s-exp-match? '{ANY ANY ...} s)
     (appC (parse (first (s-exp->list s)))
           (map (λ (a)
                  (parse a)) (rest (s-exp->list s))))] 
    [else (error 'parse "invalid input")]))

(define (parse-type [s : s-expression]) : Type
  (cond
    [(s-exp-match? `num s) 
     (numT)]
    [(s-exp-match? `bool s)
     (boolT)]
    [(s-exp-match? '{ANY * ANY} s)
     (consT (parse-type (first (s-exp->list s)))
            (parse-type (third (s-exp->list s))))]
    
    [(s-exp-match? `(ANY ... -> ANY) s)
     (let ([inputs (reverse (rest (rest (reverse (s-exp->list s)))))]) ;; yes...
       (arrowT (map (λ (t)
                      (parse-type t)) inputs)
               (parse-type (first (reverse (s-exp->list s))))))]
    [else (error 'parse-type "invalid input")]))

(module+ test
  (test/exn (parse '{})
            "invalid input")
  (test (parse-type '{num bool {num -> bool} -> bool})
        (arrowT (list (numT) (boolT) (arrowT (list (numT)) (boolT))) (boolT)))
  (test (parse-type '((num * bool) * (bool * num)))
        (consT (consT (numT) (boolT))
               (consT (boolT) (numT))))
  (test (parse '2)
        (numC 2))
  (test (parse `x) ; note: backquote instead of normal quote
        (idC 'x))
  (test (parse '{+ 2 1})
        (plusC (numC 2) (numC 1)))
  (test (parse '{* 3 4})
        (multC (numC 3) (numC 4)))
  (test (parse '{+ {* 3 4} 8})
        (plusC (multC (numC 3) (numC 4))
               (numC 8)))
  (test (parse '{let {[x : num {+ 1 2}]}
                  y})
        (appC (lamC (list 'x) (list (numT)) (idC 'y))
              (list (plusC (numC 1) (numC 2)))))
  (test (parse '{lambda {[x : num]} 9})
        (lamC (list 'x) (list (numT)) (numC 9)))
  (test (parse '{double 9})
        (appC (idC 'double) (list (numC 9))))
  (test (parse '{{+ 1 2}})
        (appC {plusC (numC 1) (numC 2)} empty))

  (test (parse '{lambda {[x : num] [y : bool] [z : bool]}
                  2})
        (lamC (list 'x 'y 'z) (list (numT) (boolT) (boolT)) (numC 2)))
  (test (parse '{lambda {} 2})
        (lamC empty empty (numC 2)))
  (test (parse '{f})
        (appC (idC 'f) empty))
  (test (parse '{f 1 true})
        (appC (idC 'f) (list (numC 1) (boolC true))))
  
  (test (parse-type `num)
        (numT))
  (test (parse-type `bool)
        (boolT))
  (test (parse-type `(num -> bool))
        (arrowT (list (numT)) (boolT)))
  (test/exn (parse-type '1)
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [boolC (b) (boolV b)]
    [idC (s) (lookup s env)]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]
    [lamC (ns ts body)
          (closV ns body env)]
    [appC (fun args) (type-case Value (interp fun env)
                       [closV (ns body c-env)
                              (if (= (length ns) (length args))
                                  (interp body
                                          (let* ([args-i (map (λ (a)
                                                                (interp a env)) args)]
                                                 [bound-args (map2 (λ (n a)
                                                                     (bind n a)) ns args-i)])
                                            (foldl extend-env c-env bound-args)))
                                  (error 'interp "arity mismatch"))]
                       [else (error 'interp "not a function")])]
    [eqC (l r)
         (num= (interp l env)
               (interp r env))]
    [ifC (cnd thn els)
         (type-case Value (interp cnd env)
           [boolV (b) (if b
                          (interp thn env)
                          (interp els env))]
           [else (error 'interp "not a boolean")])]
    [consC (f s)
           (consV (interp f env)
                  (interp s env))]
    [carC (cell)
          (type-case Value (interp cell env)
            [consV (car cdr) car]
            [else (error 'interp "not a cons")])]
    [cdrC (cell)
          (type-case Value (interp cell env)
            [consV (car cdr) cdr]
            [else (error 'interp "not a cons")])]))

(module+ test
  (test (interp (parse '2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse '{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse '{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse '{lambda {[x : num]} {+ x x}})
                mt-env)
        (closV (list 'x) (plusC (idC 'x) (idC 'x)) mt-env))
  (test (interp (parse '{let {[x : num 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse '{let {[x : num 5]}
                          {let {[x : num {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse '{let {[x : num 5]}
                          {let {[y : num 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse '{{lambda {[x : num]} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test (interp (parse '{{lambda {} 2}}) mt-env)
        (numV 2))
  (test (interp (parse '{{lambda {[x : num] [y : bool]}
                           {cons x y}} 2 false}) mt-env)
        (consV (numV 2) (boolV false)))
  (test (interp (parse '{first {cons 2 {cons 3 4}}})
                mt-env)
        (numV 2))
  (test (interp (parse '{rest {cons 2 {cons 3 4}}})
                mt-env)
        (consV (numV 3) (numV 4)))
  
  (test/exn (interp (parse '{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {[x : num]} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse '{let {[bad : (num -> num) {lambda {[x : num]} {+ x y}}]}
                              {let {[y : num 5]}
                                {bad 2}}})
                    mt-env)
            "free variable"))

;; num+ and num* ----------------------------------------
(define (num-op [op : (number number -> 'a)] [const : ('a -> Value)]
                [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (const (op (numV-n l) (numV-n r)))]
    [else
     (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + numV l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * numV l r))
(define (num= [l : Value] [r : Value]) : Value
  (num-op = boolV l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6))
  (test (num= (numV 2) (numV 3))
        (boolV false))
  (test (num= (numV 2) (numV 2))
        (boolV true))
  (test/exn (num= (boolV true) (numV 2))
            "not a number"))

;; lookup ----------------------------------------
(define (make-lookup [name-of : ('a -> symbol)] [val-of : ('a -> 'b)])
  (lambda ([name : symbol] [vals : (listof 'a)]) : 'b
    (cond
      [(empty? vals)
       (error 'find "free variable")]
      [else (if (equal? name (name-of (first vals)))
                (val-of (first vals))
                ((make-lookup name-of val-of) name (rest vals)))])))

(define lookup
  (make-lookup bind-name bind-val))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env))
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)))
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)))
        (numV 8)))

;; typecheck ----------------------------------------
(define (typecheck [a : ExprC] [tenv : TypeEnv])
  (type-case ExprC a
    [numC (n) (numT)]
    [boolC (b) (boolT)]
    [eqC (l r) (typecheck-nums l r boolT tenv)]
    [ifC (cnd thn els) (type-case Type (typecheck cnd tenv)
                         [boolT () (let ([thnt (typecheck thn tenv)]
                                         [elst (typecheck els tenv)])
                                     (if (equal? thnt elst)
                                         thnt
                                         (type-error thn "type mismatch")))]
                         
                         [else (type-error cnd "boolean")])]
    [plusC (l r) (typecheck-nums l r numT tenv)]
    [multC (l r) (typecheck-nums l r numT tenv)]
    [idC (n) (type-lookup n tenv)]
    [lamC (ns arg-types body)
          (arrowT arg-types
                  (typecheck body
                             (foldl extend-env  
                                    tenv (map2 (λ (n a)
                                                 (tbind n a))
                                               ns
                                               arg-types))))]
    [appC (fun args)
          (type-case Type (typecheck fun tenv)
            [arrowT (arg-types result-type)
                    (if (equal? (length args) (length arg-types))
                        (let 
                            ([args-check (foldl (λ (a b)
                                                  (and a b))
                                                true
                                                (map2 equal? arg-types
                                                      (map (λ (a)
                                                             (typecheck a tenv))
                                                           args)))])
                          (if args-check
                              result-type
                              (type-error args
                                          (to-string arg-types))))
                        (type-error fun "arity mismatch"))]
            [else (type-error fun "function")])]
    
    [consC (f s)
           (let ([ft (typecheck f tenv)]
                 [st (typecheck s tenv)])
             (consT ft st))]
    [carC (cell) (type-case Type (typecheck cell tenv)
                   [consT (f s) f]
                   [else (type-error cell "cons")])]
    [cdrC (cell) (type-case Type (typecheck cell tenv)
                   [consT (f s) s]
                   [else (type-error cell "cons")])]))

(define (typecheck-nums l r out-type tenv)
  (type-case Type (typecheck l tenv)
    [numT ()
          (type-case Type (typecheck r tenv)
            [numT () (out-type)]
            [else (type-error r "num")])]
    [else (type-error l "num")]))

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define type-lookup
  (make-lookup tbind-name tbind-type))

(module+ test
  (test/exn (interp (parse '{{lambda {[x : num]} x}}) mt-env)
            "arity mismatch")
  (test/exn (typecheck (parse '{{lambda {[x : num]} x}}) mt-env)
            "no type")
  (test (typecheck (parse '{{lambda {[x : bool] [y : num]} x} true  2}) mt-env)
        (boolT))
  (test/exn (typecheck (parse '{{lambda {[x : bool] [y : num]} x} 2 true}) mt-env)
            "no type")
  
  ;; new tests
  (test (interp (parse '{if true 1 2}) mt-env)
        (numV 1))
  (test/exn (interp (parse '{if 1 2 3}) mt-env)
            "not a boolean")
  (test (typecheck (parse '{if true {lambda {[x : num]} 7} {lambda {[x : num]} 8}}) mt-env)
        (arrowT (list (numT)) (numT)))
  (test/exn (typecheck (parse '{if true 1 true}) mt-env)
            "no type")
  (test/exn (typecheck (parse '{if 1 2 3}) mt-env)
            "no type")
  
  (test (interp (parse '{if {= 13 {if {= 1 {+ -1 2}}
                                      12
                                      13}}
                            4
                            5})
                mt-env)
        (numV 5))
  
  (test (typecheck (parse '{= 13 {if {= 1 {+ -1 2}}
                                     12
                                     13}})
                   mt-env)
        (boolT))
  
  (test/exn (typecheck (parse '{+ 1 {if true true false}})
                       mt-env)
            "no type")
  
  
  ;; Second tests
  (test/exn (typecheck (parse '{rest 1}) mt-env)
            "no type")
  (test/exn (interp (parse '{first 1}) mt-env)
            "not a cons")
  (test/exn (interp (parse '{rest 1}) mt-env)
            "not a cons")
  (test (interp (parse '{cons 10 8})
                mt-env)
        ;; Your constructor might be different than consV:
        (consV (numV 10) (numV 8)))
  
  (test (interp (parse '{first {cons 10 8}})
                mt-env)
        (numV 10))
  
  (test (interp (parse '{rest {cons 10 8}})
                mt-env)
        (numV 8))
  
  (test (typecheck (parse '{cons 10 8})
                   mt-env)
        (consT (numT) (numT)))
  
  (test (typecheck (parse '{first {cons 10 8}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse '{+ 1 {rest {cons 10 8}}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse '{lambda {[x : (num * bool)]}
                             {first x}})
                   mt-env)
        (arrowT (list (consT (numT) (boolT))) (numT)))
  
  (test (typecheck (parse '{{lambda {[x : (num * bool)]}
                              {first x}}
                            {cons 1 false}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse '{{lambda {[x : (num * bool)]}
                              {rest x}}
                            {cons 1 false}})
                   mt-env)
        (boolT))
  
  (test/exn (typecheck (parse '{first 10})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse '{+ 1 {first {cons false 8}}})
                       mt-env)
            "no type")
  
  (test/exn (typecheck (parse '{lambda {[x : (num * bool)]}
                                 {if {first x}
                                     1
                                     2}})
                       mt-env)
            "no type")
  
  ;; Third part tests
  (test (typecheck (parse '{lambda {[f : (-> num)]}
                             {f}}) mt-env)
        (arrowT (list (arrowT empty (numT))) (numT)))
  (test (interp (parse '{{lambda {}
                           10}})
                mt-env)
        (numV 10))
  
  (test (interp (parse '{{lambda {[x : num] [y : num]} {+ x y}}
                         10
                         20})
                mt-env)
        (numV 30))
  
  
  (test (typecheck (parse '{{lambda {[x : num] [y : bool]} y}
                            10
                            false})
                   mt-env)
        (boolT))
  
  (test/exn (typecheck (parse '{{lambda {[x : num] [y : bool]} y}
                                false
                                10})
                       mt-env)
            "no type")
  
  (test (typecheck (parse '{lambda {[c : bool] [x : num] [f : (num -> num)]}
                             {if c
                                 x
                                 {f x}}}) mt-env)
        (arrowT (list (boolT) (numT) (arrowT (list (numT)) (numT))) (numT)))
  
  (test (typecheck (parse '{{lambda {[c : bool] [x : num] [f : (num -> num)]}
                              {if c
                                  x
                                  {f x}}}
                            true 7 {lambda {[x : num]} {* x 2}}}) mt-env)
        (numT))
  (test (interp (parse '{{lambda {[c : bool] [x : num] [f : (num -> num)]}
                           {if c
                               x
                               {f x}}}
                         true 7 {lambda {[x : num]} {* x 2}}}) mt-env)
        (numV 7))
  (test (interp (parse '{{lambda {[c : bool] [x : num] [f : (num -> num)]}
                           {if c
                               x
                               {f x}}}
                         false 7 {lambda {[x : num]} {* x 2}}}) mt-env)
        (numV 14))

  (test (typecheck (parse '{lambda {[x : num]}
                             {lambda {[y : bool]}
                               2}}) mt-env)
        (arrowT (list (numT)) (arrowT (list (boolT)) (numT))))
  (test (parse-type '{{num -> bool} -> num})
        (arrowT (list (arrowT (list (numT)) (boolT))) (numT)))
  
  
  ;; old tests
  
  (test (typecheck (parse '10) mt-env)
        (numT))
  (test (typecheck (parse '{+ 10 17}) mt-env)
        (numT))
  (test (typecheck (parse '{* 10 17}) mt-env)
        (numT))
  (test (typecheck (parse '{lambda {[x : num]} 12}) mt-env)
        (arrowT (list (numT)) (numT)))
  (test (typecheck (parse '{lambda {[x : num]} {lambda {[y : bool]} x}}) mt-env)
        (arrowT (list (numT)) (arrowT (list (boolT))  (numT))))
  
  (test (typecheck (parse '{{lambda {[x : num]} 12}
                            {+ 1 17}})
                   mt-env)
        (numT))
  
  (test (typecheck (parse '{let {[x : num 4]}
                             {let {[f : (num -> num)
                                      {lambda {[y : num]} {+ x y}}]}
                               {f x}}})
                   mt-env)
        (numT))
  
  (test/exn (typecheck (parse '{1 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse '{{lambda {[x : bool]} x} 2})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse '{+ 1 {lambda {[x : num]} x}})
                       mt-env)
            "no type")
  (test/exn (typecheck (parse '{* {lambda {[x : num]} x} 1})
                       mt-env)
            "no type"))
