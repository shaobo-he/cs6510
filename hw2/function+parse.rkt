#lang plai-typed

(define-type-alias Args (listof symbol))
(define-type-alias ExprCs (listof ExprC))

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [subC (l : ExprC)
        (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [appC (s : symbol)
        (arg : ExprCs)])

(define-type FunDefC
  [fdC (name : symbol) 
       (arg : Args) 
       (body : ExprC)])

;; An expr-S-exp is either
;; - number
;; - symbol
;; - (list '+ expr-S-expr expr-S-expr)
;; - (list '* expr-S-expr expr-S-expr)
;; - (list symbol expr-S-expr)

;; A fundef-S-exp is
;; - (list 'define (list symbol symbol) arith-S-expr)

(module+ test
  (print-only-errors true))

;; parse ----------------------------------------
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(and (s-exp-list? s) 
          (= 3 (length (s-exp->list s)))
          (s-exp-symbol? (first (s-exp->list s)))
          (eq? '+ (s-exp->symbol (first (s-exp->list s)))))
     (plusC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(and (s-exp-list? s) 
          (= 3 (length (s-exp->list s)))
          (s-exp-symbol? (first (s-exp->list s)))
          (eq? '- (s-exp->symbol (first (s-exp->list s)))))
     (subC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(and (s-exp-list? s)
          (= 3 (length (s-exp->list s)))
          (s-exp-symbol? (first (s-exp->list s)))
          (eq? '* (s-exp->symbol (first (s-exp->list s)))))
     (multC (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(and (s-exp-list? s) 
          (<= 1 (length (s-exp->list s))) ; Application may not require an arg
          (s-exp-symbol? (first (s-exp->list s))))
     (appC (s-exp->symbol (first (s-exp->list s)))
           (map parse 
                (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (contains? [name : symbol] [args : Args]) : boolean
  (cond
    [(empty? args) #f]
    [(cons? args) (or (equal? name (first args))
                      (contains? name (rest args)))]))

(define (duplicates? [args : Args]) : boolean
  (cond
    [(empty? args) #f]
    [(cons? args) (or (contains? (first args)
                                 (rest args))
                      (duplicates? (rest args)))]))

(module+ test
  (test (duplicates? empty) #f)
  (test (duplicates? (list 'x)) #f)
  (test (duplicates? (list 'x 'y)) #f)
  (test (duplicates? (list 'a 'x 'y 'x 'z)) #t))

(define (parse-fundef [s : s-expression]) : FunDefC
  (cond
    [(and (s-exp-list? s)
          (= 3 (length (s-exp->list s)))
          (s-exp-symbol? (first (s-exp->list s)))
          (eq? 'define (s-exp->symbol (first (s-exp->list s))))
          (s-exp-list? (second (s-exp->list s)))
          (<= 1 (length (s-exp->list (second (s-exp->list s))))) ; Functions have a name any number of args
          (s-exp-symbol? (first (s-exp->list (second (s-exp->list s)))))
          #;(s-exp-symbol? (second (s-exp->list (second (s-exp->list s)))))
          )
     (fdC (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
          (let [(args(map s-exp->symbol 
               (rest (s-exp->list (second (s-exp->list s))))))]
            (if (duplicates? args)
                (error 'parse-func "bad syntax")
                args))
          ;(s-exp->symbol (second (s-exp->list (second (s-exp->list s)))))
          (parse (third (s-exp->list s))))]
    [else (error 'parse-fundef "invalid input")]))

(module+ test
  (test (parse '2)
        (numC 2))
  (test (parse `x) ; note: backquote instead of normal quote
        (idC 'x))
  (test (parse '{+ 2 1})
        (plusC (numC 2) (numC 1)))
  (test (parse '{- 2 1})
        (subC (numC 2) (numC 1)))
  (test (parse '{* 3 4})
        (multC (numC 3) (numC 4)))
  (test (parse '{+ {* 3 4} 8})
        (plusC (multC (numC 3) (numC 4))
               (numC 8)))
  (test (parse '{double 9})
        (appC 'double (list (numC 9))))
  (test/exn (parse '{{+ 1 2}})
            "invalid input")

  (test (parse-fundef '{define {double x} {+ x x}})
        (fdC 'double (list 'x) (plusC (idC 'x) (idC 'x))))
  (test (parse-fundef '{define {sub x y} {+ x {* -1 y}}})
        (fdC 'sub (list 'x 'y) (plusC (idC 'x)
                                      (multC (numC -1) (idC 'y)))))
  (test (parse-fundef '{define {double2 x} {sub {quadruple x} {double x}}})
        (fdC 'double2 (list 'x) (appC 'sub (list (appC 'quadruple (list (idC 'x)))
                                                 (appC 'double (list (idC 'x)))))))
  (test (parse-fundef '{define {five} 5})
        (fdC 'five empty (numC 5)))
  (test/exn (parse-fundef '{def {f (list x)} x})
            "invalid input")
  (test (parse-fundef '{define {f x y} x})
        (fdC 'f (list 'x 'y) (idC 'x)))
  (test/exn (parse-fundef '{define {add3 a b a} '{+ a {+ b a}}})
        "bad syntax")
  

  (define double-def
    (parse-fundef '{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef '{define {quadruple x} {double {double x}}}))
  (define sub-def
    (parse-fundef '{define {sub x y} {+ x {* -1 y}}}))
  (define five-def
    (parse-fundef '{define {five} 5}))
  (define three-def
    (parse-fundef '{define {three a b c} {sub {sub a b} {sub b c}}}))
  (define double2-def
    (parse-fundef '{define {double2 x} {sub {quadruple x} {double x}}})))

;; interp ----------------------------------------
(define (interp [a : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC a
    [numC (n) n]
    [idC (s) (error 'interp "free variable")]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [subC (l r) (- (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [appC (s args) (local [(define fd (get-fundef s fds))]
                    (interp (subst (map (λ (a)
                                          (numC (interp a fds)))
                                        args)
                                   (fdC-arg fd)
                                   (fdC-body fd))
                            fds))]))

(module+ test
  (test (interp (parse '2) empty)
        2)
  (test/exn (interp (parse `x) empty)
            "free variable")
  (test (interp (parse '{+ 2 1}) empty)
        3)
  (test (interp (parse '{* 2 1}) empty)
        2)
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                empty)
        19)
  (test (interp (parse '{double 8})
                (list double-def))
        16)
  (test (interp (parse '{- 2 1}) empty)
        1)
  (test (interp (parse '{quadruple 8})
                (list double-def quadruple-def))
        32)
  (test (interp (parse '{five}) (list five-def))
        5)
  (test (interp (parse '{+ {sub 2 1} 1}) (list sub-def))
        2)
  (test (interp (parse '{three 5 1 3}) (list three-def sub-def))
        6)
  (test/exn (interp (parse '{three 4 3 2 1}) (list three-def sub-def))
            "wrong arity") ; too many
  (test/exn (interp (parse '{three 1 2}) (list three-def sub-def))
            "wrong arity") ; too few
  (test (interp (parse '{double2 8})
                (list double-def double2-def quadruple-def sub-def))
        16)
  (test (interp (parse '{f 1 2})
                (list (parse-fundef '{define {f x y} {+ x y}})))
        3)
  (test (interp (parse '{+ {f} {f}})
                (list (parse-fundef '{define {f} 5})))
        10)
  (test/exn (interp (parse '{f 1})
                    (list (parse-fundef '{define {f x y} {+ x y}})))
            "wrong arity"))

;; get-fundef ----------------------------------------
(define (get-fundef [s : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "undefined function")]
    [(cons? fds) (if (eq? s (fdC-name (first fds)))
                     (first fds)
                     (get-fundef s (rest fds)))]))

(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))

;; subst ----------------------------------------
(define (subst [what : ExprCs] [for : Args] [in : ExprC]) : ExprC
  (cond
    [(and (empty? what) (empty? for)) in]
    [(and (cons? what) (cons? for))
     ; =>
     (subst-one (first what) (first for)
                (subst (rest what) (rest for) in))]
    [else (error 'subst "wrong arity")]))


;; subst-one ----------------------------------------
(define (subst-one [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (if (eq? for s)
                 what
                 in)]
    [plusC (l r) (plusC (subst-one what for l)
                        (subst-one what for r))]
    [subC (l r) (subC (subst-one what for l)
                        (subst-one what for r))]
    [multC (l r) (multC (subst-one what for l)
                        (subst-one what for r))]
    [appC (s args) (appC s (map (λ (a)
                                  (subst-one what for a))
                                args))]))

(module+ test
  (test (subst (list (parse '8)) (list 'x) (parse '9))
        (numC 9))
  (test (subst (list (parse '8) (parse '9)) (list 'x 'y) (parse '{- x y}))
        (parse '{- 8 9}))
  (test (subst (list (parse '8) (parse '9)) (list 'x 'y) (parse `x))
        (parse '8))
  (test (subst (list (parse '1) (parse '2) (parse '3)) (list 'x 'y 'z) (parse '{- x {+ y z}}))
        (parse '{- 1 {+ 2 3}}))
  (test (subst (list (parse '8)) (list 'x) (parse `x))
        (numC 8))
  (test (subst (list (parse '8)) (list 'x) (parse `y))
        (idC 'y))
  (test (subst (list (parse '8)) (list 'x) (parse '{+ x y}))
        (parse '{+ 8 y}))
  (test (subst (list (parse '8)) (list 'x) (parse '{* y x}))
        (parse '{* y 8}))
  (test (subst (list (parse '8)) (list 'x) (parse '{double x}))
        (parse '{double 8})))
