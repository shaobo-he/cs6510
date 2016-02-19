#lang plai-typed
(require plai-typed/s-exp-match)

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)]
  [consV (car : Thunk)
         (cdr : Thunk)])

(define-type Thunk
  [delay (body : ExprC)
         (env : Env)
         (done : (boxof (optionof Value)))]
  [¬delay])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [lamC (n : symbol)
        (body : ExprC)]
  [appC (fun : ExprC)
        (arg : ExprC)]
  ;; missing
  [if0C (c : ExprC)
        (t : ExprC)
        (e : ExprC)]
  ;; +
  [consC (car : ExprC)
         (cdr : ExprC)]
  ;; +
  [carC (l : ExprC)]
  ;; +
  [cdrC (l : ExprC)]
  ;; ++
  [cons?C  (l : ExprC)]
  ;; +
  [letrecC (name : symbol)
           (rhs  : ExprC)
           (body : ExprC)])

(define-type Binding
  [bind (name : symbol)
        (val : (boxof Thunk))]) ;; Obnoxious, but this allows
                                ;; us to add a binding to the
                                ;; environment in such a way
                                ;; that the thunk has this
                                ;; binding in its environment.

;; (letrec ([y y]) y) throws the interpreter into an infinite
;; loop. Can we prevent this? No. In order to detect that y is
;; undefined, we'd need to find out what y is, but then we'd
;; be evaluating the rhs which is not lazy.
;;
;; Interestingly, Haskell's equivalent program
;; let y = y in y
;; fails at runtime with a <<loop>> error. This is due to
;; runtime machinery which detects some, but not all,
;; loops.

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors true))

;; parse ----------------------------------------
(define (parse [s : s-expression]) : ExprC
  (cond
    ;; Number
    [(s-exp-match? `NUMBER s) (numC (s-exp->number s))]
    
    ;; ID
    [(s-exp-match? `SYMBOL s) (idC (s-exp->symbol s))]
    
    ;; +
    [(s-exp-match? '{+ ANY ANY} s)
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    ;; *
    [(s-exp-match? '{* ANY ANY} s)
     (multC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    
    ;; if0
    [(s-exp-match? '{if0 ANY ANY ANY} s)
     (let ([ls (s-exp->list s)])
       (if0C (parse (second ls))
             (parse (third ls))
             (parse (fourth ls))))]
    
    ;; let
    [(s-exp-match? '{let {[SYMBOL ANY]} ANY} s)
     (let ([bs (s-exp->list (first
                             (s-exp->list (second
                                           (s-exp->list s)))))])
       (appC (lamC (s-exp->symbol (first bs))
                   (parse (third (s-exp->list s))))
             (parse (second bs))))]
    
    ;; letrec
    [(s-exp-match? '{letrec {[SYMBOL ANY]} ANY} s)
     (let* ([ls (s-exp->list s)]
            [nv (s-exp->list (first (s-exp->list (second ls))))])
       (letrecC (s-exp->symbol (first nv))
                (parse (second nv))
                (parse (third ls))))]
    
    ;; λ
    [(s-exp-match? '{lambda {SYMBOL SYMBOL ...} ANY} s)
     (let* ([ls (s-exp->list s)]
            [ns (s-exp->list (second ls))])
       (foldr (λ (n lam) (lamC (s-exp->symbol n)
                               lam))
              (parse (third ls))
              ns))]
    
    ;; cons
    [(s-exp-match? '{cons ANY ANY} s)
     (let ([ls (s-exp->list s)])
       (consC (parse (second ls)) (parse (third ls))))]
    
    ;; first
    [(s-exp-match? '{first ANY} s)
     (carC (parse (second (s-exp->list s))))]
    
    ;; rest
    [(s-exp-match? '{rest ANY} s)
     (cdrC (parse (second (s-exp->list s))))]
    
    ;; cons?
    [(s-exp-match? '{cons? ANY} s)
     (cons?C (parse (second (s-exp->list s))))]
    
    ;; app
    [(s-exp-match? '{ANY ANY ANY ...} s)
     (let ([ls (s-exp->list s)])
       (foldl (λ (a acc)
                (appC acc (parse a)))
              (parse (first ls))
              (rest ls)))]
    
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
        (appC (lamC 'x (idC 'y))
              (plusC (numC 1) (numC 2))))
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
    [idC (s) (force (lookup s env))]
    [plusC (l r) (num+ (interp l env)
                       (interp r env))]
    [multC (l r) (num* (interp l env)
                       (interp r env))]
    [lamC (n body)
          (closV n body env)]
    [appC (fun arg) (type-case Value (interp fun env)
                      [closV (n body c-env)
                             (interp body
                                     (extend-env
                                      (bind n (box (delay arg env (box (none)))))
                                      c-env))]
                      [else (error 'interp "not a function")])]
    [if0C (c t e) (type-case Value (interp c env)
                    [numV (n) (if (= n 0)
                                  (interp t env)
                                  (interp e env))]
                    [else (error 'interp "not a number")])]    
    
    [consC (car cdr) (consV (delay car env (box (none)))
                            (delay cdr env (box (none))))]
    
    [carC (l) (type-case Value (interp l env)
                [consV (cart cdrt) (force cart)]
                [else (error 'interp "not a list")])]
    
    [cdrC (l) (type-case Value (interp l env)
                [consV (cart cdrt) (force cdrt)]
                [else (error 'interp "not a list")])]
    [cons?C (l) (type-case Value (interp l env)
                  [consV (cart cdrt) (numV 0)]
                  [else (numV 1)])]
    [letrecC (name rhs body)
             (let ([b (box (¬delay))])
               (let ([new-env (extend-env
                               (bind name b)
                               env)])
                 (begin
                   (set-box! b (delay rhs new-env (box (none))))
                   (interp body new-env))))]))

(define (interp-expr [a : ExprC])
  (type-case Value (interp a mt-env)
    [numV (n) (number->s-exp n)]
    [closV (ig no re) `function]
    [consV (ju nk) `cons]))

(module+ test
  (define dumb '{{lambda {x} y} 7})
  
  (test (interp-expr (parse '{cons? (cons 1 2)}))
        `0)
  (test (interp-expr (parse '{cons? 1}))
        `1)
  
  ;; Haskellish tests
  (test (interp-expr (parse `(letrec ([++ ,++])
                               (first (++ (cons 1 2)
                                          (cons 3 4))))))
        `1)
  (test (interp-expr (parse `(letrec ([++ ,++])
                               (first (rest (++ (cons 1 2) (cons 3 4)))))))
        `2)
  (test (interp-expr (parse `(letrec ([++ ,++])
                               (first (rest (rest (++ (cons 1 2) (cons 3 4))))))))
        `3)
  (test (interp-expr (parse `(letrec ([++ ,++])
                               (rest (rest (rest (++ (cons 1 2) (cons 3 4))))))))
        `4)
  
  (test (interp-expr (parse `(letrec ([++ ,++])
                               (letrec ([l (cons 1 l)])
                                 (first (rest (rest (rest (++ (cons 1 2) l)))))))))
        `1)
  
  (test (interp-expr (parse `(letrec ([map ,(mapi `map)])
                               (letrec ([l (cons 1 l)])
                                 (first (rest (rest (map (lambda (n) (+ n 1)) l))))))))
        `2)
  
  (test/exn (interp-expr (parse `(letrec ([++ ,++])
                                   (first (rest (rest (rest (++ (cons 1 2) (cons 3 4)))))))))
            "not a list")
  
  (test (interp-expr (parse `(letrec ([zipWith ,(zipWithi `zipWith)])
                               (letrec ([!! ,!!])
                                 (letrec ([fibs (cons 0 (cons 1
                                                              (zipWith (lambda (a)
                                                                         (lambda (b)
                                                                           (+ a b)))
                                                                       fibs (rest fibs))))])
                                   (!! fibs 20)))))) ;; Set to much higher than 20 to make
                                                     ;; the interpreter blow up
        `6765)
  (test (interp-expr (parse `(letrec ([zipWith ,(zipWithi `zipWith)])
                               (letrec ([onTheTake ,(take `onTheTake)])
                                 (letrec ([drop ,(drop `drop)])
                                   (letrec ([fibs (cons 0 (cons 1
                                                                (zipWith (lambda (a)
                                                                           (lambda (b)
                                                                             (+ a b)))
                                                                         fibs (rest fibs))))])
                                     ((onTheTake 1) (drop 20 fibs))))))))
        `cons)
  (test (interp-expr (parse `(letrec ([foldl ,(foldli `foldl)])
                               (foldl (lambda (acc)
                                        (lambda (n)
                                          (+ acc n)))
                                      0 (cons 1 (cons 2 3))))))
        `6)
  
  ;; New tests
  (test (interp-expr (parse '{lambda {_} x}))
        `function)
  (test (interp-expr (parse '10))
        '10)
  (test (interp-expr (parse '{+ 10 17}))
        '27)
  (test (interp-expr (parse '{* 10 7}))
        '70)
  (test (interp-expr (parse '{{lambda {x} {+ x 12}}
                              {+ 1 17}}))
        '30)
  
  (test (interp-expr (parse '{let {[x 0]}
                               {let {[f {lambda {y} {+ x y}}]}
                                 {+ {f 1}
                                    {let {[x 3]}
                                      {f 2}}}}}))
        '3)
  
  (test (interp-expr (parse '{if0 0 1 2}))
        '1)
  (test (interp-expr (parse '{if0 1 1 2}))
        '2)
  (test/exn (interp-expr (parse '{if0 {lambda {x} x} 1 2}))
            "not a number")
  (test (interp-expr (parse '{if0 0 1 {+ {lambda {x} x} 2}}))
        `1)
  (test (interp-expr (parse '{if0 1 {+ {lambda {x} x} 2} 3}))
        `3)
  (test (interp-expr (parse '{cons 1 2}))
        `cons)
  (test (interp-expr (parse '{first {cons 1 2}}))
        '1)
  (test (interp-expr (parse '{rest {cons 1 2}}))
        '2)
  (test/exn (interp-expr (parse '{first 2}))
            "not a list")
  (test/exn (interp-expr (parse '{rest 3}))
            "not a list")
  (test (interp-expr (parse '(first (rest (rest {cons 1 {cons 2 {cons 3 4}}})))))
        `3)
  
  ;; Lazy evaluation:
  (test (interp-expr (parse '{{lambda {x} 0}
                              {+ 1 {lambda {y} y}}}))
        '0)
  (test (interp-expr (parse '{let {[x {+ 1 {lambda {y} y}}]}
                               0}))
        '0)
  (test (interp-expr (parse '{first {cons 3
                                          {+ 1 {lambda {y} y}}}}))
        '3)
  (test (interp-expr (parse '{rest {cons {+ 1 {lambda {y} y}}
                                         4}}))
        '4)
  (test (interp-expr (parse '{first {cons 5
                                          ;; Infinite loop:
                                          {{lambda {x} {x x}}
                                           {lambda {x} {x x}}}}}))
        '5)
  (test (interp-expr (parse `(first (rest (rest {cons ,dumb {cons ,dumb {cons 3 ,dumb}}})))))
        `3)
  (test (interp-expr 
         (parse 
          '{let {[mkrec
                  ;; This is call-by-name mkrec
                  ;;  (simpler than call-by-value):
                  {lambda {body-proc}
                    {let {[fX {lambda {fX}
                                {body-proc {fX fX}}}]}
                      {fX fX}}}]}
             {let {[fib
                    {mkrec
                     {lambda {fib}
                       ;; Fib:
                       {lambda {n}
                         {if0 n
                              1
                              {if0 {+ n -1}
                                   1
                                   {+ {fib {+ n -1}}
                                      {fib {+ n -2}}}}}}}}]}
               ;; Call fib on 4:
               {fib 4}}}))
        '5)
  
  (test (interp-expr 
         (parse 
          '{let {[mkrec
                  ;; This is call-by-name mkrec
                  ;;  (simpler than call-by-value):
                  {lambda {body-proc}
                    {let {[fX {lambda {fX}
                                {body-proc {fX fX}}}]}
                      {fX fX}}}]}
             {let {[nats-from
                    {mkrec
                     {lambda {nats-from}
                       ;; nats-from:
                       {lambda {n}
                         {cons n {nats-from {+ n 1}}}}}}]}
               {let {[list-ref
                      {mkrec
                       {lambda {list-ref}
                         ;; list-ref:
                         {lambda {n}
                           {lambda {l}
                             {if0 n
                                  {first l}
                                  {{list-ref {+ n -1}} {rest l}}}}}}}]}
                 ;; Call list-ref on infinite list:
                 {{list-ref 4} {nats-from 2}}}}}))
        '6)
  
  ;; letrec
  (test (interp-expr 
         (parse
          '{letrec {[x 1]}
             {letrec {[y 2]}
               {+ x y}}}))
        '3)
  (test (interp-expr 
         (parse
          '{letrec {[x 1]}
             {letrec {[y y]}
               {+ x x}}}))
        '2)
  (test (interp-expr 
         (parse
          '{letrec {[l {cons 1 l}]}
             {letrec {[list-ref
                       {lambda {l}
                         {lambda {n}
                           {if0 n
                                {first l}
                                {{list-ref {rest l}} {+ n -1}}}}}]}
               {+ {{list-ref l} 42}
                  {{list-ref l} 79}}}}))
        '2)
  
  ;; Stock tests
  (test (interp (parse '2) mt-env)
        (numV 2))
  (test/exn (interp (parse `x) mt-env)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (box (delay (numC 9) mt-env (box (none))))) mt-env))
        (numV 9))
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
  
  (test (interp (parse '{{lambda {x} 5} {1 2}})
                mt-env)
        (numV 5))
  
  (test/exn (interp (parse '{1 2}) mt-env)
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {x} x}}) mt-env)
            "not a number")
  (test/exn (interp (parse '{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env)
            "free variable")
  
  #;(time (interp (parse '{let {[x2 {lambda {n} {+ n n}}]}
                            {let {[x4 {lambda {n} {x2 {x2 n}}}]}
                              {let {[x16 {lambda {n} {x4 {x4 n}}}]}
                                {let {[x256 {lambda {n} {x16 {x16 n}}}]}
                                  {let {[x65536 {lambda {n} {x256 {x256 n}}}]}
                                    {x65536 1}}}}}})
                  mt-env)))

;; force ----------------------------------------

(define (force [t : Thunk]) : Value
  (type-case Thunk t
    [delay (b e d) (type-case (optionof Value) (unbox d)
                     [none ()
                           (let ([v (interp b e)])
                             (begin
                               (set-box! d (some v))
                               v))]
                     [some (v) v])]
    [¬delay () (error 'interp "something's wrong")]))

(module+ test
  (test/exn (force (¬delay))
            "something's wrong")
  (test (force (delay (numC 8) mt-env (box (none))))
        (numV 8))
  (test (let ([v (delay (numC 8) mt-env (box (none)))])
          (begin
            (force v)
            (force v)))
        (numV 8))
  (test (force (delay (numC 8) mt-env (box (some (numV 9)))))
        (numV 9))
  (test (force (delay (idC 'x)
                      (extend-env (bind 'x (box (delay (numC 9) mt-env (box (none)))))
                                  mt-env)
                      (box (none))))
        (numV 9)))

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

(module+ test
  (test (num+ (numV 1) (numV 2))
        (numV 3))
  (test (num* (numV 2) (numV 3))
        (numV 6)))

;; lookup ----------------------------------------
(define (lookup [n : symbol] [env : Env]) : Thunk
  (cond
    [(empty? env) (error 'lookup "free variable")]
    [else (cond
            [(symbol=? n (bind-name (first env)))
             (unbox (bind-val (first env)))]
            [else (lookup n (rest env))])]))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (box (delay (numC 8) mt-env (box (none))))) mt-env))
        (delay (numC 8) mt-env (box (none))))
  (test (lookup 'x (extend-env
                    (bind 'x (box (delay (numC 9) mt-env (box (none)))))
                    (extend-env (bind 'x (box (delay (numC 8) mt-env (box (none))))) mt-env)))
        (delay (numC 9) mt-env (box (none))))
  (test (lookup 'y (extend-env
                    (bind 'x (box (delay (numC 9) mt-env (box (none)))))
                    (extend-env (bind 'y (box (delay (numC 8) mt-env (box (none))))) mt-env)))
        (delay (numC 8) mt-env (box (none)))))

;; Hakellish functions
(define (mapi [name : s-expression])
  `(lambda (f l) ;; (a -> b) -> [a] -> [b]
     (if0 (cons? l)
          (cons (f (first l))
                (,name f (rest l)))
          (f l))))

(define (zipWithi [name : s-expression])
  `(lambda (f l1 l2) ;; (a -> b -> c) -> [a] -> [b] -> [c]
     (if0 (cons? l1) ;; l1 and l2 better be the same length
          (cons (f (first l1) (first l2))
                (,name f (rest l1) (rest l2)))
          ((f l1) l2))))

(define (foldli [name : s-expression])
  `(lambda (f acc l) ;; (b -> a -> b) -> b -> [a] -> b
     (if0 (cons? l)
          (,name f (f acc (first l)) (rest l))
          (f acc l))))

(define (foldri [name : s-expression])
  `(lambda (f acc l) ;; (a -> b -> b) -> b -> [a] -> b
     (if0 (cons? l)
          (f (first l) (,name f acc (rest l)))
          (f l acc))))

(define (drop [name : s-expression]) ;; Mostly to view how bad laziness can be
  `(lambda (n l) ;; Int -> [a] -> "[a]"
     (if0 n
          l
          (if0 (cons? l)
               (,name (+ n -1) (rest l))
               1))))

(define (take [name : s-expression]) ;; See how forcing can reduce the thunks
  `(lambda (n l) ;; Int -> [a] -> "[a]"
     (if0 n
          1
          (if0 (cons? l)
               (cons (first l)
                     (,name (+ n -1) (rest l)))
               1))))
(define !!
  `(lambda (l n) ;; [a] -> Int -> a
     (letrec ([d ,(drop `d)])
       (letrec ([t ,(take `t)])
         (first ((t 1) (d n l)))))))

(define ++
  `(lambda (l1 l2) ;; [a] -> [a] -> [a]
     (letrec ([foldr ,(foldri `foldr)])
       (foldr (lambda (f)
                (lambda (acc)
                  (cons f acc)))
              l2 l1))))
