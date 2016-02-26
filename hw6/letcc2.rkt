#lang plai-typed
(require plai-typed/s-exp-match)

(define-type Value
  [numV (n : number)]
  [closV (args : (listof symbol))
         (body : ExprC)
         (env : Env)]
  [contV (k : Cont)
         (h : Handlers)]
  [errorV (msg : string)])

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
        (len : number)]
  [tryC (body    : ExprC)
        (handler : ExprC)]
  [throwC (msg : string)])
  

;; List of exception handlers.
;; It's hard to observe the "jump immediately to the handler"
;; behavior within the interpreter, but viewing an execution
;; trace for
;; {try
;;  {+ 1
;;     {+ 2
;;        {+ {try {+ 5 {lambda {} y}}
;;                {lambda {} 3}}
;;           {lambda {} 13}}}}
;;  {lambda {} 11}}
;; we get
;; >(num-op
;;   #<procedure:+>
;;   (numV 3)
;;   (closV '() (numC 13) '())
;;   (doAddK (numV 2) (doAddK (numV 1) (popHandlerK (doneK))))
;;   (list (handlerH (numC 11) '() (doneK))))
;; >(escape (errorV "not a number") (list (handlerH (numC 11) '() (doneK))))
;; >(interp (numC 11) '() (doneK) '())
;; >(continue (doneK) (numV 11) '())
;; <(numV 11)
;; we see the interpreter jumps immediately back to the exception handler.
;;
;; Exception handlers capture the current environment and continuation.
;; When an error occurs, escape interprets the handler in the captured
;; environment and continuation.
(define-type Handler
  [handlerH (handler : ExprC)
            (env     : Env)
            (k       : Cont)])

(define-type-alias Handlers (listof Handler))
(define mt-handler empty)

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
            (len : number)
            (env : Env)
            (k   : Cont)]
  [accAvgK (ns  : (listof ExprC))
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
          (k : Cont)]
  ;; Allows interp to arrange for the current handler
  ;; to be removed when no error is encountered.
  [popHandlerK (k : Cont)])

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

    [(s-exp-match? '{throw STRING} s)
     (throwC (s-exp->string (second (s-exp->list s))))]
    
    [(s-exp-match? '{try ANY {lambda {} ANY}} s)
     (tryC (parse (second (s-exp->list s)))
           (parse (third (s-exp->list (third (s-exp->list s))))))]

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
  (test (parse '{throw "ouch"})
        (throwC "ouch"))
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
  (test (parse '{try 8 {lambda {} 9}})
        (tryC (numC 8) (numC 9)))
  (test (parse '{if0 1 2 3})
        (if0C (numC 1) (numC 2) (numC 3)))
  (test (parse '{avg 1 2 3})
        (avgC (list (numC 1) (numC 2) (numC 3)) 3))
  (test (parse '{neg 2})
        (negC (numC 2)))
  (test (parse '{double 9})
        (appC (idC 'double) (list (numC 9))))
  (test (parse '{add 1 2 3})
        (appC (idC 'add) (list (numC 1) (numC 2) (numC 3))))
  (test (parse '{lambda {x y z} {+ x {* y z}}})
        (lamC (list 'x 'y 'z) (plusC (idC 'x)
                                     (multC (idC 'y) (idC 'z)))))
  (test/exn (parse '{})
            "invalid input"))

;; interp & continue ----------------------------------------
(define (interp [a : ExprC] [env : Env] [k : Cont] [h : Handlers]) : Value
  (type-case ExprC a
    [numC (n) (continue (numV n) k h)]
    [idC (s) (lookup s env k h)]
    [plusC (l r) (interp l env
                         (addSecondK r env k) h)]
    [multC (l r) (interp l env
                         (multSecondK r env k) h)]
    [negC (n) (interp n env
                      (negK k) h)]

    [avgC (ns len) (cond
                   [(cons? ns) (interp (first ns) env
                                       (avgRestK (rest ns) len
                                                 env k) h)]
                   [else (continue (numV 0) k h)])]
    [lamC (ns body)
          (continue (closV ns body env) k h)]
    [appC (fun args)
          (cond 
          [(cons? args) (interp (first args) env
                                (appArgsK (rest args) fun empty env k) h)]
          [else (interp fun env
                        (doAppK empty k) h)])]
    [let/ccC (n body)
             (interp body
                     (extend-env (bind n (contV k h))
                                 env)
                     k h)]
    [if0C (cnd thn els)
          (interp cnd env
                  (if0K thn els env k) h)]
    [tryC (body handler)
          (interp body env (popHandlerK k)
                  (cons (handlerH handler env k) h))]
    [throwC (msg)
            (escape (errorV msg) h)]))

(define (continue [v : Value] [k : Cont] [h : Handlers]) : Value
  (type-case Cont k
    [doneK () v]
    [addSecondK (r env next-k)
                (interp r env
                        (doAddK v next-k) h)]
    [doAddK (v-l next-k)
            (num+ v-l v next-k h)]
    [multSecondK (r env next-k)
                (interp r env
                        (doMultK v next-k) h)]
    [doMultK (v-l next-k)
             (num* v-l v next-k h)]
    [negK (next-k)
          (type-case Value v
            [numV (n) (continue (numV (* -1 n)) next-k h)]
            [else (escape (errorV "not a number") h)])]
    [avgRestK (ns len env next-k) ;; Accumulator comes in as v
              (cond
                [(cons? ns) (interp (first ns) env
                                    (accAvgK (rest ns) v len env next-k) h)]
                [else (num/ v (numV len) next-k h)])]
    [accAvgK (ns acc len env next-k)
             (num+ acc v (avgRestK ns len env next-k) h)]
    [if0K (thn els env next-k)
          (type-case Value v
            [numV (n) (if (= 0 n)
                          (interp thn env next-k h)
                          (interp els env next-k h))]
            [else (escape (errorV "not a number") h)])]
                          
    [appArgsK (args fun vals env next-k)
              (cond
                [(cons? args) (interp (first args) env
                                      (appArgsK (rest args) fun (cons v vals)
                                                env next-k) h)]
                [else (interp fun env
                              (doAppK (cons v vals) next-k) h)])]
    [doAppK (vs next-k)
            (type-case Value v ;; Closure now comes in as the continuation value
              [closV (ns body c-env)
                     (if (= (length vs) (length ns))
                         (interp body
                                 (extend-env*
                                  (map2 bind ns (reverse vs))
                                  c-env)
                                 next-k h)
                         (escape (errorV "arity mismatch") h))]
              [contV (k-v old-h)
                     (if (= (length vs) 1)
                         (continue (first vs) k-v old-h)
                         (escape (errorV "arity mismatch") h))]
              [else (escape (errorV "not a function") h)])]
    [popHandlerK (next-k)
                 (continue v next-k (rest h))]))

(define interp-expr : (ExprC -> s-expression)
  (λ (e)
    (type-case Value (interp e mt-env (doneK) mt-handler)
      [numV (n) (number->s-exp n)]
      [errorV (msg) (string->s-exp msg)]
      [else `function])))

(module+ test
  ;; Exception tests
  (test (interp-expr (parse '{throw "ouch"}))
        `"ouch")
  (test (interp-expr (parse '{try
                        (throw "9")
                        (lambda {} {try
                                    (throw "7")
                                    {lambda {} 13}})}))
        `13)
  (test (interp-expr (parse '{try
                              {throw "ouch"}
                              {lambda {} 7}}))
        `7)
  (test (interp-expr (parse '{try
                              {throw "ouch"}
                              {lambda {} {throw "boom"}}}))
        `"boom")
  (test (interp-expr (parse '{{lambda {x} x} 1 2}))
        `"arity mismatch")
  (test (interp-expr (parse '{let/cc k
                               {+ {k 2 3} 7}}))
        `"arity mismatch")
  (test (interp-expr (parse '{+ 2 x}))
        `"free variable")
  (test (interp-expr (parse '{try
                              {+ 2 x}
                              {lambda {} 8}}))
        `8)
  (test (interp-expr (parse '{try
                              8
                              {lambda {} 9}}))
        `8)
  (test (interp-expr {parse '{avg 1 x {lambda {} 2}}})
        `"free variable") ;; Left to right evaluation
  (test (interp-expr {parse '{avg 1 {lambda {} 2} x}})
        `"not a number")  ;; Left to right evaluation
  (test (interp-expr (parse '{{lambda {x y z} {* x {+ y z}}}
                              y {+ {lambda {} 3} 2} 8}))
        `"free variable") ;; Left to right evaluation
  (test (interp-expr (parse '{{lambda {x y z} {* x {+ y z}}}
                              {+ {lambda {} 3} 2} y 8}))
        `"not a number")  ;; Left to right evaluation
  (test (interp-expr (parse '{try {+ {lambda {} 1} 2}
                                  {lambda {} 7}}))
        `7)
  (test (interp-expr (parse '{1 2}))
        `"not a function")
  (test (interp-expr (parse '{try {+ {try {+ {lambda {} y} x}
                                          {lambda {} 7}}
                                     {lambda {} 3}}
                                  {lambda {} 2}}))
        `2)
  (test (interp-expr (parse '{+ {try {+ {lambda {} y} x}
                                     {lambda {} 7}}
                                {try {+ 2 {lambda {} z}}
                                     {lambda {} 3}}}))
        `10)
  (test (interp-expr (parse '{+ x 2}))
        `"free variable")
  
  (test (interp-expr (parse '{try
                              {try {+ {lambda {} 2} 3}
                                   {lambda {} {+ 9 {lambda {} 4}}}}
                              {lambda {} 8}}))
        `8)
  (test (interp-expr (parse '{try
                              {try
                               {try {+ {lambda {} 2} 3}
                                    {lambda {} {+ 9 {lambda {} 4}}}}
                               {lambda {} {+ {lambda {} 5}
                                             6}}}
                              {lambda {} 7}}))
        `7)
  (test (interp-expr (parse '{try
                              {let/cc done
                                {done {+ 2 {lambda {} 3}}}}
                              {lambda {} 8}}))
        `8)
  
  (test (interp-expr (parse '{+ 1
                                {+ 2
                                   {+ {try {+ 5 {lambda {} y}}
                                           {lambda {} 3}}
                                      4}}}))
        `10)
  (test (interp-expr (parse '{try
                              {+ 1
                                 {+ 2
                                    {+ {try {+ 5 {lambda {} y}}
                                            {lambda {} 3}}
                                       4}}}
                              {lambda {} 11}}))
        `10)
  (test (interp-expr (parse '{try
                              {+ 1
                                 {+ 2
                                    {+ {try {+ 5 {lambda {} y}}
                                            {lambda {} 3}}
                                       {lambda {} 13}}}}
                              {lambda {} 11}}))
        `11)
  (test (interp-expr (parse '{let/cc k
                                 {try
                                  {+ {k {lambda {} 8}} 8}
                                  {lambda {} 3}}}))
          `function)
  
  ;; Make sure call/cc restores old handler
  (test (interp-expr (parse '{try
                              {+ {let/cc k
                                   {try
                                    {+ {k {lambda {} 8}} 8}
                                    {lambda {} 3}}}
                                 9}
                              {lambda {} 11}}))
        `11)
  
  ;; Coverage tests
  (test (interp-expr (parse '{if0 {lambda {a} b} 0 2}))
        `"not a number")
  (test (interp-expr (parse '(neg {lambda {x} y})))
        `"not a number")
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
  (test (interp (parse '2) mt-env (doneK) mt-handler)
        (numV 2))
  (test (interp (parse `x) mt-env (doneK) mt-handler)
        (errorV "free variable"))
  (test (interp (parse `x)
                (extend-env (bind 'x (numV 9)) mt-env)
                (doneK) mt-handler)
        (numV 9))
  (test (interp (parse '{+ 2 1}) mt-env (doneK) mt-handler)
        (numV 3))
  (test (interp (parse '{* 2 1}) mt-env (doneK) mt-handler)
        (numV 2))
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                mt-env
                (doneK) mt-handler)
        (numV 19))
  (test (interp (parse '{lambda {x} {+ x x}})
                mt-env
                (doneK) mt-handler)
        (closV (list 'x) (plusC (idC 'x) (idC 'x)) mt-env))
  (test (interp (parse '{let {[x 5]}
                          {+ x x}})
                mt-env
                (doneK) mt-handler)
        (numV 10))
  (test (interp (parse '{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                (doneK) mt-handler)
        (numV 12))
  (test (interp (parse '{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                (doneK) mt-handler)
        (numV 5))
  (test (interp (parse '{{lambda {x} {+ x x}} 8})
                mt-env
                (doneK) mt-handler)
        (numV 16))

  (test (interp (parse '{let/cc k {+ 1 {k 0}}})
                mt-env
                (doneK) mt-handler)
        (numV 0))
  (test (interp (parse '{let {[f {let/cc k k}]}
                          {f {lambda {x} 10}}})
                mt-env
                (doneK) mt-handler)
        (numV 10))

  (test (interp (parse '{1 2}) mt-env (doneK) mt-handler)
            (errorV "not a function"))
  (test (interp (parse '{+ 1 {lambda {x} x}}) mt-env (doneK) mt-handler)
        (errorV "not a number"))
  (test (interp (parse '{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    (doneK) mt-handler)
        (errorV "free variable"))
  ;; Eager:
  (test (interp (parse '{{lambda {x} 0} {1 2}}) mt-env (doneK) mt-handler)
        (errorV "not a function"))

  (test (continue (numV 5) (doneK) mt-handler)
        (numV 5))
  (test (continue (numV 5) (addSecondK (numC 6) mt-env (doneK)) mt-handler)
        (numV 11))
  (test (continue (numV 5) (doAddK (numV 7) (doneK)) mt-handler)
        (numV 12))
  (test (continue (numV 5) (multSecondK (numC 6) mt-env (doneK)) mt-handler)
        (numV 30))
  (test (continue (numV 5) (doMultK (numV 7) (doneK)) mt-handler)
        (numV 35))
  (test (continue (numV 5) (appArgsK empty (parse '{lambda {x} x}) empty mt-env (doneK)) mt-handler)
        (numV 5))
  (test (continue (closV (list 'x) (idC 'x) mt-env)
                  (doAppK (list (numV 8)) (doneK)) mt-handler)
        (numV 8)))

;; num+ and num* ----------------------------------------
(define (num-op [op : (number number -> number)] [l : Value] [r : Value]
                [k : Cont] [h : Handlers]) : Value
  (cond
   [(and (numV? l) (numV? r))
    (continue (numV (op (numV-n l) (numV-n r))) k h)]
   [else
    (escape (errorV "not a number") h)]))
(define (num+ [l : Value] [r : Value] [k : Cont] [h : Handlers]) : Value
  (num-op + l r k h))
(define (num* [l : Value] [r : Value] [k : Cont] [h : Handlers]) : Value
  (num-op * l r k h))
(define (num/ [l : Value] [r : Value] [k : Cont] [h : Handlers]) : Value
  (num-op / l r k h))

(module+ test
  (test (num+ (numV 2) (closV (list 'x) (idC 'x) empty) (doneK) mt-handler)
        (errorV "not a number"))
  (test (num+ (numV 2)
              (closV (list 'x) (idC 'x) empty)
              (popHandlerK (doneK))
              (cons (handlerH (numC 3) mt-env (doneK))
                    mt-handler))
        (numV 3))
  (test (num+ (numV 1) (numV 2) (doneK) mt-handler)
        (numV 3))
  (test (num* (numV 2) (numV 3) (doneK) mt-handler)
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : symbol] [env : Env] [k : Cont] [h : Handlers]) : Value
  (cond
   [(empty? env) (escape (errorV "free variable") h)]
   [else (cond
          [(symbol=? n (bind-name (first env)))
           (continue (bind-val (first env)) k h)]
          [else (lookup n (rest env) k h)])]))

(module+ test
  (test (lookup 'x mt-env (doneK) mt-handler)
        (errorV "free variable"))
  (test (lookup 'x mt-env (popHandlerK (doneK)) (cons (handlerH (numC 7)
                                                                mt-env
                                                                (doneK))
                                                                mt-handler))
        (numV 7))
  (test (lookup 'x (extend-env (bind 'x (numV 8)) mt-env) (doneK) mt-handler)
        (numV 8))
  (test (lookup 'x (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'x (numV 8)) mt-env)) (doneK) mt-handler)
        (numV 9))
  (test (lookup 'y (extend-env
                    (bind 'x (numV 9))
                    (extend-env (bind 'y (numV 8)) mt-env)) (doneK) mt-handler)
        (numV 8)))

;; escape ----------------------------------------
(define (escape [v : Value] [h : Handlers]) : Value
  (cond
    [(cons? h) (type-case Handler (first h)
                 [handlerH (handlr env next-k)
                           (interp handlr env next-k (rest h))])]
    [else v]))

(module+ test
  (test (escape (errorV "yo") mt-handler)
        (errorV "yo"))
  (test (escape (errorV "") (cons (handlerH (idC 'x)
                                            (extend-env
                                             (bind 'x (numV 8)) mt-env)
                                            (doneK)) mt-handler))
        (numV 8))
  (test (escape (errorV "") (cons
                             (handlerH (idC 'x)
                                       (extend-env
                                        (bind 'x (numV 8)) mt-env)
                                       (doAddK (numV 7) (doneK))) mt-handler))
        (numV 15))
  (test (escape (errorV "") (cons
                             (handlerH (idC 'x)
                                       (extend-env
                                        (bind 'x (numV 8)) mt-env)
                                       (doAddK (numV 7) (popHandlerK (doneK))))
                             (cons
                              (handlerH (numC 9)
                                        mt-env
                                        (doneK))mt-handler)))
        (numV 15))
  (test (escape (errorV "") (cons
                             (handlerH (idC 'x)
                                       (extend-env
                                        (bind 'x (numV 8)) mt-env)
                                       (doAddK (closV (list 'x)
                                                      (idC 'x)
                                                      mt-env) (popHandlerK (doneK))))
                             (cons
                              (handlerH (numC 9)
                                        mt-env
                                        (doneK))mt-handler)))
        (numV 9)))

;(trace interp continue num-op lookup escape)