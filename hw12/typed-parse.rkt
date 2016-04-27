#lang plai-typed
(require plai-typed/s-exp-match
         "class.rkt"
         "inherit.rkt"
         "typed-class.rkt"
         "inherit-parse.rkt")

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define (parse-t-class [s : s-expression]) : ClassT
  (cond
   [(s-exp-match? `{class SYMBOL extends SYMBOL {ANY ...} ANY ...} s)
    (classT (s-exp->symbol (second (s-exp->list s)))
            (s-exp->symbol (fourth (s-exp->list s)))
            (map parse-t-field
                 (s-exp->list (fourth (rest (s-exp->list s)))))
            (map parse-t-method 
                 (rest (rest (rest (rest (rest (s-exp->list s))))))))]
   [else (error 'parse-t-class "invalid input")]))

(define (parse-t-field [s : s-expression]) : FieldT
  (cond
   [(s-exp-match? `[SYMBOL : ANY] s)
    (fieldT (s-exp->symbol (first (s-exp->list s)))
            (parse-type (third (s-exp->list s))))]
   [else (error 'parse-t-field "invalid input")]))

(define (parse-t-method [s : s-expression]) : MethodT
  (cond
   [(s-exp-match? `{SYMBOL : ANY -> ANY ANY} s)
    (methodT (s-exp->symbol (first (s-exp->list s)))
             (parse-type (third (s-exp->list s)))
             (parse-type (fourth (rest (s-exp->list s))))
             (parse (fourth (rest (rest (s-exp->list s))))))]
   [else (error 'parse-t-method "invalid input")]))

(define (parse-type [s : s-expression]) : Type
  (cond
   [(s-exp-match? `num s)
    (numT)]
   [(s-exp-match? `SYMBOL s)
    (objT (s-exp->symbol s))]
   [else (error 'parse-type "invalid input")]))

(module+ test
  (test (parse-type `num)
        (numT))
  (test (parse-type `object)
        (objT 'object))
  (test/exn (parse-type `{})
            "invalid input")
  
  (test (parse-t-field `[x : num])
        (fieldT 'x (numT)))
  (test/exn (parse-t-field '{x 1})
            "invalid input")

  (test (parse-t-method `{m : num -> object this})
        (methodT 'm (numT) (objT 'object) (thisI)))
  (test/exn (parse-t-method `{m 1})
            "invalid input")
  
  (test (parse-t-class '{class posn3D extends posn
                               {[x : num] [y : num]}
                               {m1 : num -> num arg}
                               {m2 : num -> object this}})
        (classT 'posn3D 'posn
                (list (fieldT 'x (numT))
                      (fieldT 'y (numT)))
                (list (methodT 'm1 (numT) (numT) (argI))
                      (methodT 'm2 (numT) (objT 'object) (thisI)))))
  (test/exn (parse-t-class '{class})
            "invalid input"))

;; ----------------------------------------

;; interp which performs typechecking before running
(define interp-t-prog
  (λ (classes a)
    (interp-t-prog-real classes a true)))
;; interp which does not perform typechecking before running
(define interp-prog
  (λ (classes a)
    (interp-t-prog-real classes a false)))

;; Interps the program with or without typechecking
(define (interp-t-prog-real [classes : (listof s-expression)] [a : s-expression] [tc : boolean]) : s-expression
  (let ([pa (parse a)]
        [classes-t (map parse-t-class classes)])
    (begin
      (if tc
          (typecheck pa classes-t)
          (numT))
      (let ([v (interp-t pa
                         classes-t)])
        (type-case Value v
          [numV (n) (number->s-exp n)]
          [objV (class-name field-vals) `object])))))

;; Performs typechecking on the program
(define (typecheck-prog [classes : (listof s-expression)] [a : s-expression]) : s-expression
  (let ([pa (parse a)]
        [classes-t (map parse-t-class classes)])
    (begin
      (type-case Type (typecheck pa classes-t)
        [numT () `num]
        [objT (name) (symbol->s-exp name)]))))

(module+ test
  ;; 1. Fix arg/this tests
  (test/exn (interp-t-prog
         (list
          '{class empty extends object
                  {}})
         '{+ arg 2})
        "not allowed")
  
  (test (interp-t-prog
         (list
          '{class empty extends object
                  {}})
         '{new empty})
        `object)

 (test (interp-t-prog 
        (list
         '{class posn extends object
                 {[x : num]
                  [y : num]}
                 {mdist : num -> num
                        {+ {get this x} {get this y}}}
                 {addDist : posn -> num
                          {+ {send arg mdist 0}
                             {send this mdist 0}}}}
         
         '{class posn3D extends posn
                 {[z : num]}
                 {mdist : num -> num
                        {+ {get this z} 
                           {super mdist arg}}}})
        
        '{send {new posn3D 5 3 1} addDist {new posn 2 7}})
       '18))

;; end

