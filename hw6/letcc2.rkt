#lang plai-typed
(require plai-typed/s-exp-match)

(define-type Value
  [numV (n : number)]
  [closV (args : (listof symbol))
         (body : ExprC)
         (env : Env)]
  [contV (k : Cont)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [negC  (n : ExprC)]
  [lamC (ns : (listof symbol))
        (body : ExprC)]
  [appC (fun : ExprC)
        (args : (listof ExprC))]
  [let/ccC (n : symbol)
           (body : ExprC)]
  [if0C (cnd : ExprC)
        (thn : ExprC)
        (els : ExprC)]
  [avgC (ns  : (listof ExprC))
        (len : number)])

(define-type Binding
  [bind (name : symbol)
        (val : Value)])

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)
(define extend-env* append)

(define-type Cont
  [doneK]
  [addSecondK (r : ExprC)
              (e : Env)
              (k : Cont)]
  [doAddK (v : Value)
          (k : Cont)]
  [multSecondK (r : ExprC)
               (e : Env)
               (k : Cont)]
  [doMultK (v : Value)
           (k : Cont)]
  [negK (k : Cont)]
  [avgRestK (ns  :  (listof ExprC))
            (acc : Value)
            (len : number)
            (env : Env)
            (k   : Cont)]
  [appArgsK (a     : (listof ExprC))
            (fun-e : ExprC)
            (vals  : (listof Value))
            (env   : Env)
            (k     : Cont)]
  [if0K (thn : ExprC)
        (els : ExprC)
        (env : Env)
        (k   : Cont)]
        
  [doAppK (f : (listof Value))
          (k : Cont)])

(module+ test
  (print-only-errors true))

;; parse ----------------------------------------
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-match? `NUMBER s) (numC (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idC (s-exp->symbol s))]
    [(s-exp-match? '{+ ANY ANY} s)
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{* ANY ANY} s)
     (multC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    
    [(s-exp-match? '{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appC (lamC (list (s-exp->symbol (first bs)))
                   (parse (third (s-exp->list s))))
             (list (parse (second bs)))))]

    [(s-exp-match? '{neg ANY} s)
     (negC (parse (second (s-exp->list s))))]

    [(s-exp-match? '{avg ANY ...} s)
     (let ([ns (map parse (rest (s-exp->list s)))])
       (avgC ns (length ns)))]

    [(s-exp-match? '{if0 ANY ANY ANY} s)
     (let ([ls (s-exp->list s)])
       (if0C (parse (second ls))
             (parse (third ls))
             (parse (fourth ls))))]

    [(s-exp-match? '{lambda {SYMBOL ...} ANY} s)
     (lamC (map s-exp->symbol (s-exp->list 
                               (second (s-exp->list s))))
           (parse (third (s-exp->list s))))]
    
    [(s-exp-match? '{let/cc SYMBOL ANY} s)
     (let/ccC (s-exp->symbol (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    
    [(s-exp-match? '{ANY ANY ...} s)
     (appC (parse (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
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
  (test (parse '{let {[x {+ 1 2}]}
                  y})
        (appC (lamC (list 'x) (idC 'y))
              (list (plusC (numC 1) (numC 2)))))
  (test (parse '{lambda {x} 9})
        (lamC (list 'x) (numC 9)))
  (test (parse '{let/cc k 0})
        (let/ccC 'k (numC 0)))
  (test (parse '{double 9})
        (appC (idC 'double) (list (numC 9))))
  (test/exn (parse '{})
            "invalid input"))

;; interp & continue ----------------------------------------
(define (interp [a : ExprC] [env : Env] [k : Cont]) : Value
  (type-case ExprC a
    [numC (n) (continue k (numV n))]
    [idC (s) (continue k (lookup s env))]
    [plusC (l r) (interp l env
                         (addSecondK r env k))]
    [multC (l r) (interp l env
                         (multSecondK r env k))]
    [negC (n) (interp n env
                      (negK k))]

    [avgC (ns l) (cond
                   [(cons? ns) (interp (first ns) env
                                       (avgRestK (rest ns) (numV 0) l
                                                 env k))]
                   [else (continue k (numV 0))])]
    [lamC (ns body)
          (continue k (closV ns body env))]
    [appC (fun args)
          (cond 
          [(cons? args) (interp (first args) env
                                (appArgsK (rest args) fun empty env k))]
          [else (interp fun env
                        (doAppK empty k))])]
    [let/ccC (n body)
             (interp body
                     (extend-env (bind n (contV k))
                                 env)
                     k)]
    [if0C (cnd thn els)
          (interp cnd env
                  (if0K thn els env k))]))

(define (continue [k : Cont] [v : Value]) : Value
  (type-case Cont k
    [doneK () v]
    [addSecondK (r env next-k)
                (interp r env
                        (doAddK v next-k))]
    [doAddK (v-l next-k)
            (continue next-k (num+ v-l v))]
    [multSecondK (r env next-k)
                (interp r env
                        (doMultK v next-k))]
    [doMultK (v-l next-k)
             (continue next-k (num* v-l v))]
    [negK (k)
          (type-case Value v
            [numV (n) (continue k (numV (* -1 n)))]
            [else (error 'interp "not a number")])]
    [avgRestK (ns acc len env next-k)
              (cond
                [(cons? ns) (interp (first ns) env
                                    (avgRestK (rest ns) (num+ acc v) len
                                              env next-k))]
                [else (continue next-k (num/ (num+ acc v) (numV len)))])]
    [if0K (thn els env next-k)
          (type-case Value v
            [numV (n) (if (= 0 n)
                          (interp thn env next-k)
                          (interp els env next-k))]
            [else (error 'interp "not a number")])]
                          
    [appArgsK (args fun vals env next-k)
              (cond
                [(cons? args) (interp (first args) env
                                      (appArgsK (rest args) fun (cons v vals)
                                                env next-k))]
                [else (interp fun env
                              (doAppK (cons v vals) next-k))])]
    [doAppK (vs next-k)
            (type-case Value v ;; Closure now comes in as the continuation value
              [closV (ns body c-env)
                     (interp body
                             (extend-env*
                              (map2 bind ns (reverse vs))
                              c-env)
                             next-k)]
              [contV (k-v) (continue k-v (first vs))]
              [else (error 'interp "not a function")])]))

(define interp-expr : (ExprC -> s-expression)
  (λ (e)
    (type-case Value (interp e mt-env (doneK))
      [numV (n) (number->s-exp n)]
      [else `function])))

(module+ test
  ;; Coverage tests
  (test/exn (interp-expr (parse '{if0 {lambda {a} b} 0 2}))
            "not a number")
  (test/exn (interp-expr (parse '(neg {lambda {x} y})))
            "not a number")
  (test (interp-expr (parse '{avg}))
        '0)

  ;; Behavior tests
  (test (interp-expr (parse '{{lambda {x y z} {* x {+ y z}}} 2 3 4}))
        `14)
  (test (interp-expr (parse '{avg 1 2}))
        (number->s-exp 3/2))
  
  ;; Part 1 tests
  (test (interp-expr (parse '{neg 2}))
        '-2)
  (test (interp-expr (parse '{avg 0 6 6}))
        '4)
  (test (interp-expr (parse '{let/cc k {neg {k 3}}}))
        '3)
  (test (interp-expr (parse '{let/cc k {avg 0 {k 3} 0}}))
        '3)
  (test (interp-expr (parse '{let/cc k {avg {k 2} {k 3} 0}}))
        '2)
  (test (interp-expr (parse '{if0 1 2 3}))
        '3)
  (test (interp-expr (parse '{if0 0 2 3}))
        '2)
  (test (interp-expr (parse '{let/cc k {if0 {k 9} 2 3}}))
        '9)
  
  ;; Part 2 tests
  (test (interp-expr (parse '{{lambda {x y} {+ y {neg x}}} 10 12}))
        '2)
  (test (interp-expr (parse '{lambda {} 12}))
        `function)
  (test (interp-expr (parse '{lambda {x} {lambda {} x}}))
        `function)
  (test (interp-expr (parse '{{{lambda {x} {lambda {} x}} 13}}))
        '13)

  (test (interp-expr (parse '{let/cc esc {{lambda {x y} x} 1 {esc 3}}}))
        '3)
  (test (interp-expr (parse '{{let/cc esc {{lambda {x y} {lambda {z} {+ z y}}}
                                           1 
                                           {let/cc k {esc k}}}}
                              10}))
        '20)
  
  ;; Original tests
  (test (interp (parse '2) mt-env (doneK))
        (numV 2))
  (test/exn (interp (parse `x) mt-env (doneK))
            "free variable")
  (test (interp (parse `x)
                (extend-env (bind 'x (numV 9)) mt-env)
                (doneK))
        (numV 9))
  (test (interp (parse '{+ 2 1}) mt-env (doneK))
        (numV 3))
  (test (interp (parse '{* 2 1}) mt-env (doneK))
        (numV 2))
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                mt-env
                (doneK))
        (numV 19))
  (test (interp (parse '{lambda {x} {+ x x}})
                mt-env
                (doneK))
        (closV (list 'x) (plusC (idC 'x) (idC 'x)) mt-env))
  (test (interp (parse '{let {[x 5]}
                          {+ x x}})
                mt-env
                (doneK))
        (numV 10))
  (test (interp (parse '{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                (doneK))
        (numV 12))
  (test (interp (parse '{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                (doneK))
        (numV 5))
  (test (interp (parse '{{lambda {x} {+ x x}} 8})
                mt-env
                (doneK))
        (numV 16))

  (test (interp (parse '{let/cc k {+ 1 {k 0}}})
                mt-env
                (doneK))
        (numV 0))
  (test (interp (parse '{let {[f {let/cc k k}]}
                          {f {lambda {x} 10}}})
                mt-env
                (doneK))
        (numV 10))

  (test/exn (interp (parse '{1 2}) mt-env (doneK))
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {x} x}}) mt-env (doneK))
            "not a number")
  (test/exn (interp (parse '{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    (doneK))
            "free variable")
  ;; Eager:
  (test/exn (interp (parse '{{lambda {x} 0} {1 2}}) mt-env (doneK))
            "not a function")

  (test (continue (doneK) (numV 5))
        (numV 5))
  (test (continue (addSecondK (numC 6) mt-env (doneK)) (numV 5))
        (numV 11))
  (test (continue (doAddK (numV 7) (doneK)) (numV 5))
        (numV 12))
  (test (continue (multSecondK (numC 6) mt-env (doneK)) (numV 5))
        (numV 30))
  (test (continue (doMultK (numV 7) (doneK)) (numV 5))
        (numV 35))
  (test (continue (appArgsK empty (parse '{lambda {x} x}) empty mt-env (doneK)) (numV 5))
        (numV 5))
  (test (continue (doAppK (list (numV 8)) (doneK)) (closV (list 'x) (idC 'x) mt-env))
        (numV 8)))

;; num+ and num* ----------------------------------------
(define (num-op [op : (number number -> number)] [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (numV (op (numV-n l) (numV-n r)))]
   [else
    (error 'interp "not a number")]))
(define (num+ [l : Value] [r : Value]) : Value
  (num-op + l r))
(define (num* [l : Value] [r : Value]) : Value
  (num-op * l r))
(define (num/ [l : Value] [r : Value]) : Value
  (num-op / l r))

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : symbol] [env : Env]) : Value
  (cond
   [(empty? env) (error 'lookup "free variable")]
   [else (cond
          [(symbol=? n (bind-name (first env)))
           (bind-val (first env))]
          [else (lookup n (rest env))])]))

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