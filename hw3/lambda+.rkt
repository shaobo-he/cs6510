#lang plai-typed
(require plai-typed/s-exp-match)

;; Y combinator... such beauty
(define Y '{{lambda {y} {lambda {F} {F {lambda {x} {{{y y} F} x}}}}} 
            {lambda {y} {lambda {F} {F {lambda {x} {{{y y} F} x}}}}}})
  
(define-type Value
  [boolV (p : boolean)]
  [numV (n : number)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)]
  [delayV (body : ExprC)
          (env : Env)])

(define-type ExprC
  [numC (n : number)]
  [boolC (p : boolean)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [eqC   (l : ExprC)
         (r : ExprC)]
  [letC (n : symbol) 
        (rhs : ExprC)
        (body : ExprC)]
  [lamC (n : symbol)
        (body : ExprC)]
  [appC (fun : ExprC)
        (arg : ExprC)]
  [ifC (cond : ExprC)
       (then : ExprC)
       (else : ExprC)]
  [delayC (body : ExprC)]
  [forceC (delay : ExprC)])

(define-type Binding
  [bind (name : symbol)
        (val : Value)])

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors true))
#;(lamC (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))
#;(parse (second (s-exp->list s)))
;; parse ----------------------------------------
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-match? `true s)
        (boolC true)]
    
    [(s-exp-match? `false s)
     (boolC false)]
    
    [(s-exp-match? `NUMBER s) (numC (s-exp->number s))]
    
    [(s-exp-match? `SYMBOL s) (idC (s-exp->symbol s))]
    
    [(s-exp-match? '{+ ANY ANY} s)
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    
    [(s-exp-match? '{* ANY ANY} s)
     (multC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    
    [(s-exp-match? '{= ANY ANY} s)
     (eqC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    
    [(s-exp-match? '{if ANY ANY ANY} s)
     (ifC (parse (second (s-exp->list s)))
          (parse (third  (s-exp->list s)))
          (parse (fourth (s-exp->list s))))]
               
    [(s-exp-match? '{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (letC (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    
    [(s-exp-match? '{letrec {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (parse `{let {[,(first bs) 
                      (,Y {lambda {,(first bs)}
                            ,(second bs)})]}
                 ,(third (s-exp->list s))}))]
    
    [(s-exp-match? '{delay ANY} s)
     (delayC (parse (second (s-exp->list s))))]
    
    [(s-exp-match? '{force ANY} s)
     (forceC (parse (second (s-exp->list s))))]
    
    [(s-exp-match? '{lambda {SYMBOL SYMBOL ...} ANY} s)
  (let ([arg-list (s-exp->list (second (s-exp->list s)))])
       (foldr (λ (name body)
                (lamC (s-exp->symbol name)
                      body)) 
              (parse (third (s-exp->list s))) 
              arg-list))]
    
    [(s-exp-match? '{ANY ANY ANY ...} s)
     #;(appC (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))
     (foldl (λ (arg fun)
              (appC fun
                    (parse arg)))
            (appC (parse (first (s-exp->list s)))
                  (parse (second (s-exp->list s))))
            (rest (rest (s-exp->list s))))]
    
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse '{lambda {v1 v2 v3}
                  {+ v1 {+ v2 v3}}})
        (lamC 'v1 (lamC 'v2 (lamC 'v3 (plusC (idC 'v1) (plusC (idC 'v2) (idC 'v3)))))))
  (test (parse '{f a1 a2 a3})
        (appC 
         (appC 
          (appC (idC 'f) (idC 'a1)) (idC 'a2)) (idC 'a3)))
  (test (parse '{f a1 a2 a3})
        (appC (appC (appC (idC 'f) (idC 'a1)) (idC 'a2)) (idC 'a3)))
  (test (parse '{lambda {v1 v2 v3} {+ v1 {+ v2 v3}}})
        (lamC 'v1
              (lamC 'v2
                    (lamC 'v3
                          (plusC (idC 'v1) (plusC (idC 'v2) (idC 'v3)))))))
  (test (parse `true)
        (boolC true))
  (test (parse `false)
        (boolC false))
  (test (parse '{= 77 8})
        (eqC (numC 77) (numC 8)))
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
        (letC 'x (plusC (numC 1) (numC 2))
              (idC 'y)))
  (test (parse '{lambda {x} 9})
        (lamC 'x (numC 9)))
  (test (parse '{double 9})
        (appC (idC 'double) (numC 9)))
  (test/exn (parse '{{+ 1 2}})
            "invalid input"))

;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    
    [boolC (p) (boolV p)]
    
    [idC (s) (lookup s env)]
    
    [plusC (l r) (num+ (interp l env) (interp r env))]
    
    [multC (l r) (num* (interp l env) (interp r env))]
    
    [eqC   (l r) (num= (interp l env) (interp r env))]
    [letC (n rhs body)
          (interp body
                  (extend-env
                   (bind n (interp rhs env))
                   env))]
    [lamC (n body)
          (closV n body env)]
    
    [appC (fun arg) (type-case Value (interp fun env)
                      [closV (n body c-env)
                             (interp body
                                     (extend-env
                                      (bind n
                                            (interp arg env))
                                      c-env))]
                      [else (error 'interp "not a function")])]
    
    [delayC (body) (delayV body env)] ;; Capture the environment at the time of creation.
    
    [forceC (delayed) (type-case Value (interp delayed env) ;; Interpret the allegedly delayed expression in this env.
                        [delayV (body d-env) (interp body d-env)] ;; Evaluate the delay in its environment.
                        #;[delayV (body d-env)
                                  (interp body (append d-env env))] ;; Conceivable interpretation
                        [else (error 'interp "not a thunk")])]
    
    [ifC (c t e) (let ([condition (interp c env)])
                   (type-case Value condition
                     [boolV (p) (if p
                                    (interp t env)
                                    (interp e env))]
                     [else (error 'interp "not a boolean")]))]))

(module+ test
  (test (interp (parse '{letrec {[fib {lambda {a b n}
                                        {if {= n 0}
                                            a
                                            {fib b {+ a b} {+ n -1}}}}]}
                          {fib 0 1 6}}) mt-env)
        (numV 8))
  
  (test (interp (parse '(letrec {[fact {lambda {n}
                   {if (= n 0)
                       1
                       (* n {fact {+ n -1}})}}]}
            {fact 10})) mt-env)
        (numV 3628800))
  
  (test (let ([parsed (parse '{let {[f {lambda {x y z}
                                    {if (= x 1)
                                        {if (= y 2)
                                            {if (= z 3)
                                                true
                                                false}
                                            false}
                                        false}}]}
                          {f 1 2 3}})])
          (interp parsed mt-env))
        (interp (parse `true) mt-env))
  
  (test (interp (parse '{delay {+ 1 {lambda {x} x}}}) mt-env)
        (delayV (parse '{+ 1 {lambda {x} x}}) mt-env))
  
  (test/exn (interp (parse '{force {delay {+ 1 {lambda {x} x}}}}) mt-env)
        "not a number")
  
  (test (interp (parse '{let {[ok {delay {+ 1 2}}]}
                          {let {[bad {delay {+ 1 false}}]}
                            {force ok}}}) mt-env)
        (numV 3))
  
  (test/exn (interp (parse '{let {[ok {delay {+ 1 2}}]}
                              {let {[bad {delay {+ 1 false}}]}
                                {force bad}}}) mt-env)
            "not a number")
  
  (test/exn (interp (parse '{force 1})
                    mt-env)
            "not a thunk")
  
  (test (interp (parse '{force {if {= 8 8} {delay 7} {delay 9}}})
                mt-env)
        (interp (parse '7)
                mt-env))
  
  (test (interp (parse '{let {[d {let {[y 8]}
                                   {delay {+ y 7}}}]}
                          {let {[y 9]}
                            {force d}}})
                mt-env)
        (interp (parse '15)
                mt-env))
  
  (test/exn (interp (parse '{let {[d {let {[y 8]}
                                   {delay {+ y x}}}]}
                          {let {[x 9]}
                            {force d}}})
                mt-env)
        "free variable")
  
  #;(test (interp (parse '{let {[d {let {[y 8]}
                                   {delay {+ y x}}}]}
                          {let {[x 9]}
                            {force d}}})
                mt-env)
        (interp (parse '17)
                mt-env))
  
  (test (interp (parse '{if {= 2 {+ 1 1}} 7 8})
                mt-env)
        (interp (parse '7)
                mt-env))
  
  (test (interp (parse '{if false {+ 1 {lambda {x} x}} 9})
                mt-env)
        (interp (parse '9)
                mt-env))
  (test (interp (parse '{if true 10 {+ 1 {lambda {x} x}}})
                mt-env)
        (interp (parse '10)
                mt-env))
  (test/exn (interp (parse '{if 1 2 3})
                    mt-env)
            "not a boolean")
  (test (interp (parse '{if true {+ 1 2} 5}) mt-env)
        (numV 3))
  (test (interp (parse '{if true 0 2}) mt-env)
        (numV 0))
  (test (interp (parse '{if false 0 2}) mt-env)
        (numV 2))
  (test (interp (parse '{if true 1 {f 7}}) mt-env)
        (numV 1))
  (test (interp (parse '{if false {f 7} 2}) mt-env)
        (numV 2))
  (test/exn (interp (parse '{if 7 8 9}) mt-env)
            "not a boolean")
  (test (interp (parse '2) mt-env)
        (numV 2))
  (test (interp (parse `true) mt-env)
        (boolV true))
  (test (interp (parse `false) mt-env)
        (boolV false))
        
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env))
        (numV 9))
  (test (interp (parse '{= 77 77}) mt-env)
        (boolV true))
  (test (interp (parse '{= 77 8}) mt-env)
        (boolV false))
  
  (test/exn (interp (parse '{= 77 {lambda {x} 7}}) mt-env)
            "not a number")
  (test (interp (parse '{+ 2 1}) mt-env)
        (numV 3))
  (test (interp (parse '{* 2 1}) mt-env)
        (numV 2))
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                mt-env)
        (numV 19))
  (test (interp (parse '{lambda {x} {+ x x}})
                mt-env)
        (closV 'x (plusC (idC 'x) (idC 'x)) mt-env))
  (test (interp (parse '{let {[x 5]}
                          {+ x x}})
                mt-env)
        (numV 10))
  (test (interp (parse '{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env)
        (numV 12))
  (test (interp (parse '{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env)
        (numV 5))
  (test (interp (parse '{{lambda {x} {+ x x}} 8})
                mt-env)
        (numV 16))

  (test/exn (interp (parse '{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse '{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable")

  #;
  (time (interp (parse '{let {[x2 {lambda {n} {+ n n}}]}
                          {let {[x4 {lambda {n} {x2 {x2 n}}}]}
                            {let {[x16 {lambda {n} {x4 {x4 n}}}]}
                              {let {[x256 {lambda {n} {x16 {x16 n}}}]}
                                {let {[x65536 {lambda {n} {x256 {x256 n}}}]}
                                  {x65536 1}}}}}})
                mt-env)))

;; num+ and num* ----------------------------------------
(define (num-op [op : ('a 'a -> 'b)]
                [r-type : ('b -> Value)]
                [l : Value] [r : Value]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (r-type (op (numV-n l) (numV-n r)))]
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
        (boolV true)))

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
  