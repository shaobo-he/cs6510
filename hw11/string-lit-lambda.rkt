#lang plai-typed
(require plai-typed/s-exp-match)

(define-type (Value 'l)
  [litV (n : 'l)]
  [closV (arg : symbol)
         (body : (ExprC 'l))
         (env : Env)])

(define-type (ExprC 'l)
  [litC (n : 'l)]
  [idC (s : symbol)]
  [plusC (l : (ExprC 'l)) 
         (r : (ExprC 'l))]
  [multC (l : (ExprC 'l))
         (r : (ExprC 'l))]
  [lamC (n : symbol)
        (body : (ExprC 'l))]
  [appC (fun : (ExprC 'l))
        (arg : (ExprC 'l))])

(define-type (Binding 'l)
  [bind (name : symbol)
        (val : (Value 'l))])

(define-type-alias (Env 'l) (listof (Binding 'l)))

(define mt-env empty)
(define extend-env cons)

(module+ test
  (print-only-errors true))

;; parse ----------------------------------------
(define parse-cons : ((s-expression -> boolean)
                      (s-expression -> (ExprC 'L)) -> (s-expression -> (ExprC 'l)))
  (lambda (lit-match lit-res)
    (letrec ([parse
              (lambda (s)
                (cond
                  [(lit-match s) (lit-res s)]
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
                     (appC (lamC (s-exp->symbol (first bs))
                                 (parse (third (s-exp->list s))))
                           (parse (second bs))))]
                  [(s-exp-match? '{lambda {SYMBOL} ANY} s)
                   (lamC (s-exp->symbol (first (s-exp->list 
                                                (second (s-exp->list s)))))
                         (parse (third (s-exp->list s))))]
                  [(s-exp-match? '{ANY ANY} s)
                   (appC (parse (first (s-exp->list s)))
                         (parse (second (s-exp->list s))))]
                  [else (error 'parse "invalid input")]))])
      parse)))

(define (parse/str [s : s-expression]) : (ExprC string)
  (let ([parse (parse-cons
                (λ (s) (s-exp-match? `STRING s))
                (λ (s) (litC (s-exp->string s))))])
    (parse s)))
(define (parse/num [s : s-expression]) : (ExprC number)
  (let ([parse (parse-cons (λ (s) (s-exp-match? `NUMBER s))
                           (λ (s) (litC (s-exp->number s))))])
    (parse s)))

(module+ test
  (test (parse/str '"a")
        (litC "a"))
  (test (parse/str `x) ; note: backquote instead of normal quote
        (idC 'x))
  (test (parse/str '{+ "b" "a"})
        (plusC (litC "b") (litC "a")))
  (test (parse/str '{* "c" "d"})
        (multC (litC "c") (litC "d")))
  (test (parse/str '{+ {* "c" "d"} "e"})
        (plusC (multC (litC "c") (litC "d"))
               (litC "e")))
  (test (parse/str '{let {[x {+ "a" "b"}]}
                      y})
        (appC (lamC 'x (idC 'y))
              (plusC (litC "a") (litC "b"))))
  (test (parse/str '{lambda {x} "g"})
        (lamC 'x (litC "g")))
  (test (parse/str '{double "g"})
        (appC (idC 'double) (litC "g")))
  (test/exn (parse/str '{{+ "a" "b"}})
            "invalid input")
  (test/exn (parse/str '1)
            "invalid input")
  (test (parse/num '2)
        (litC 2))
  (test (parse/num `x) ; note: backquote instead of normal quote
        (idC 'x))
  (test (parse/num '{+ 2 1})
        (plusC (litC 2) (litC 1)))
  (test (parse/num '{* 3 4})
        (multC (litC 3) (litC 4)))
  (test (parse/num '{+ {* 3 4} 8})
        (plusC (multC (litC 3) (litC 4))
               (litC 8)))
  (test (parse/num '{let {[x {+ 1 2}]}
                      y})
        (appC (lamC 'x (idC 'y))
              (plusC (litC 1) (litC 2))))
  (test (parse/num '{lambda {x} 9})
        (lamC 'x (litC 9)))
  (test (parse/num '{double 9})
        (appC (idC 'double) (litC 9)))
  (test/exn (parse/num '{{+ 1 2}})
            "invalid input")
  (test/exn (parse/num '"a")
            "invalid input"))

(define-type-alias (Binop 'l) ((Value 'l) (Value 'l) -> (Value 'l)))

;; interp ----------------------------------------
(define interp-cons : (Binop Binop -> ((ExprC 'l) Env -> (Value 'l)))
  (lambda (lit+ lit*)
    (letrec ([interp
               (lambda (a env)
                 (type-case (ExprC 'l) a
                   [litC (n) (litV n)]
                   [idC (s) (lookup s env)]
                   [plusC (l r) (lit+ (interp l env) (interp r env))]
                   [multC (l r) (lit* (interp l env) (interp r env))]
                   [lamC (n body)
                         (closV n body env)]
                   [appC (fun arg) (type-case (Value 'l) (interp fun env)
                                     [closV (n body c-env)
                                            (interp body
                                                    (extend-env
                                                     (bind n
                                                           (interp arg env))
                                                     c-env))]
                                     [else (error 'interp "not a function")])]))])
             interp)))

(define (interp/str [a : (ExprC string)] [env : Env]) : (Value string)
  (let ([interp (interp-cons str+ str*)])
    (interp a env)))
(define (interp/num [a : (ExprC number)] [env : Env]) : (Value number)
  (let ([interp (interp-cons num+ num*)])
    (interp a env)))

(module+ test
  (test (interp/num (parse/num '2) mt-env)
        (litV 2))
  (test/exn (interp/num (parse/num `x) mt-env)
            "free variable")
  (test (interp/num (parse/num `x) 
                    (extend-env (bind 'x (litV 9)) mt-env))
        (litV 9))
  (test (interp/num (parse/num '{+ 2 1}) mt-env)
        (litV 3))
  (test (interp/num (parse/num '{* 2 1}) mt-env)
        (litV 2))
  (test (interp/num (parse/num '{+ {* 2 3} {+ 5 8}})
                    mt-env)
        (litV 19))
  (test (interp/num (parse/num '{lambda {x} {+ x x}})
                    mt-env)
        (closV 'x (plusC (idC 'x) (idC 'x)) mt-env))
  (test (interp/num (parse/num '{let {[x 5]}
                                  {+ x x}})
                    mt-env)
        (litV 10))
  (test (interp/num (parse/num '{let {[x 5]}
                                  {let {[x {+ 1 x}]}
                                    {+ x x}}})
                    mt-env)
        (litV 12))
  (test (interp/num (parse/num '{let {[x 5]}
                                  {let {[y 6]}
                                    x}})
                    mt-env)
        (litV 5))
  (test (interp/num (parse/num '{{lambda {x} {+ x x}} 8})
                    mt-env)
        (litV 16))
  
  (test/exn (interp/num (parse/num '{1 2}) mt-env)
            "not a function")
  (test/exn (interp/num (parse/num '{+ 1 {lambda {x} x}}) mt-env)
            "not a literal")
  (test/exn (interp/num (parse/num '{let {[bad {lambda {x} {+ x y}}]}
                                      {let {[y 5]}
                                        {bad 2}}})
                        mt-env)
            "free variable")
  (test (interp/str (parse/str '"b") mt-env)
        (litV "b"))
  (test/exn (interp/str (parse/str `x) mt-env)
            "free variable")
  (test (interp/str (parse/str `x) 
                    (extend-env (bind 'x (litV "g")) mt-env))
        (litV "g"))
  (test (interp/str (parse/str '{+ "b" "a"}) mt-env)
        (litV "ba"))
  (test (interp/str (parse/str '{* "b" "a"}) mt-env)
        (litV "a"))
  (test (interp/str (parse/str '{+ {* "a" "b"} {+ "c" "d"}})
                    mt-env)
        (litV "bcd"))
  (test (interp/str (parse/str '{lambda {x} {+ x x}})
                    mt-env)
        (closV 'x (plusC (idC 'x) (idC 'x)) mt-env))
  (test (interp/str (parse/str '{let {[x "e"]}
                                  {+ x x}})
                    mt-env)
        (litV "ee"))
  (test (interp/str (parse/str '{let {[x "e"]}
                                  {let {[x {+ "a" x}]}
                                    {+ x x}}})
                    mt-env)
        (litV "aeae"))
  (test (interp/str (parse/str '{let {[x "e"]}
                                  {let {[y "f"]}
                                    x}})
                    mt-env)
        (litV "e"))
  (test (interp/str (parse/str '{{lambda {x} {+ x x}} "f"})
                    mt-env)
        (litV "ff"))
  
  (test/exn (interp/str (parse/str '{"a" "b"}) mt-env)
            "not a function")
  (test/exn (interp/str (parse/str '{+ "a" {lambda {x} x}}) mt-env)
            "not a literal")
  (test/exn (interp/str (parse/str '{let {[bad {lambda {x} {+ x y}}]}
                                      {let {[y "e"]}
                                        {bad "b"}}})
                        mt-env)
            "free variable"))

;; str+ and str* ----------------------------------------
(define lit-op : (('l 'l -> 'l)
                  (Value 'l)
                  (Value 'l)
                  -> (Value 'l))
  (lambda (op l r)
    (cond
      [(and (litV? l) (litV? r))
       (litV (op (litV-n l) (litV-n r)))]
      [else
       (error 'interp "not a literal")])))

(define (str+ [l : (Value string)] [r : (Value string)]) : (Value string)
  (lit-op string-append l r))
(define (str* [l : (Value string)] [r : (Value string)]) : (Value string)
  (lit-op string-mult l r))

(define (string-mult [a : string] [b : string])
  (foldl (lambda (c r) (string-append b r))
         ""
         (string->list a)))
(define (num+ [l : (Value number)] [r : (Value number)]) : (Value number)
  (lit-op + l r))
(define (num* [l : (Value number)] [r : (Value number)]) : (Value number)
  (lit-op * l r))

(module+ test
  (test (str+ (litV "abc") (litV "de"))
        (litV "abcde"))
  (test (str* (litV "abc") (litV "de"))
        (litV "dedede")))

;; lookup ----------------------------------------
(define lookup : (symbol Env -> (Value 'l))
  (lambda (n env)
    (cond
      [(empty? env) (error 'lookup "free variable")]
      [else (cond
              [(symbol=? n (bind-name (first env)))
               (bind-val (first env))]
              [else (lookup n (rest env))])])))

(module+ test
  (test/exn (lookup 'x mt-env)
            "free variable")
  (test (lookup 'x (extend-env (bind 'x (litV "f")) mt-env))
        (litV "f"))
  (test (lookup 'x (extend-env
                    (bind 'x (litV "g"))
                    (extend-env (bind 'x (litV "f")) mt-env)))
        (litV "g"))
  (test (lookup 'y (extend-env
                    (bind 'x (litV "g"))
                    (extend-env (bind 'y (litV "f")) mt-env)))
        (litV "f")))
