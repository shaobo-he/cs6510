#lang plai-typed

(define-type ExprC
  [numC (n : number)]
  [plusC (lhs : ExprC)
         (rhs : ExprC)]
  [multC (lhs : ExprC)
         (rhs : ExprC)]
  [argC]
  [thisC]
  [newC (class-name : symbol)
        (args : (listof ExprC))]
  [getC (obj-expr : ExprC)
        (field-name : symbol)]
  [sendC (obj-expr : ExprC)
         (method-name : symbol)
         (arg-expr : ExprC)]
  [ssendC (obj-expr : ExprC)
          (class-name : symbol)
          (method-name : symbol)
          (arg-expr : ExprC)]

  ;; instanceof
  [instanceofC (obj-expr : ExprC)
               (class-name : symbol)]

  ;; if0
  [if0C (cnd : ExprC)
        (thn : ExprC)
        (els : ExprC)]

  ;; begin
  [beginC (exprs : (listof ExprC))]

  ;; set
  [setC (obj-expr : ExprC)
        (name : symbol)
        (val-expr : ExprC)]
  )

(define-type ClassC
  [classC (name : symbol)
          (field-names : (listof symbol))
          (methods : (listof MethodC))
          (super-name : symbol)])

(define-type MethodC
  [methodC (name : symbol)
           (body-expr : ExprC)])

(define-type Value
  [numV (n : number)]
  [objV (class-name : symbol)
        (field-values : (listof (boxof Value)))])

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define (make-find [name-of : ('a -> symbol)])
  (lambda ([name : symbol] [vals : (listof 'a)]) : 'a
    (cond
     [(empty? vals)
      (error 'find "not found")]
     [else (if (equal? name (name-of (first vals)))
               (first vals)
               ((make-find name-of) name (rest vals)))])))

(define find-class : (symbol (listof ClassC) -> ClassC)
  (make-find classC-name))

(define find-method : (symbol (listof MethodC) -> MethodC)
  (make-find methodC-name))

;; A non-list pair:
(define-type (Pair 'a 'b)
  [kons (first : 'a) (rest : 'b)])

(define (get-field [name : symbol] 
                   [field-names : (listof symbol)] 
                   [vals : (listof (boxof Value))])
  ;; Pair fields and values, find by field name,
  ;; then extract value from pair
 (unbox (get-field-raw name field-names vals)))

(define (get-field-raw [name : symbol] 
                   [field-names : (listof symbol)] 
                   [vals : (listof (boxof Value))])
  ;; Pair fields and values, find by field name,
  ;; then extract value from pair
  (kons-rest ((make-find kons-first)
              name
              (map2 kons field-names vals))))

(module+ test
  (test/exn (find-class 'a empty)
            "not found")
  (test (find-class 'a (list (classC 'a empty empty 'object)))
        (classC 'a empty empty 'object))
  (test (find-class 'b (list (classC 'a empty empty 'object)
                             (classC 'b empty empty 'object)))
        (classC 'b empty empty 'object))
  (test (get-field 'a 
                   (list 'a 'b)
                   (list (box (numV 0)) (box (numV 1))))
        (numV 0)))

;; ----------------------------------------

(define interp : (ExprC (listof ClassC) Value Value -> Value)
  (lambda (a classes this-val arg-val)
    (local [(define (recur expr)
              (interp expr classes this-val arg-val))]
      (type-case ExprC a
        [numC (n) (numV n)]
        [plusC (l r) (num+ (recur l) (recur r))]
        [multC (l r) (num* (recur l) (recur r))]
        [thisC () this-val]
        [argC () arg-val]
        [newC (class-name field-exprs)
              (local [(define c (find-class class-name classes))
                      (define vals (map recur field-exprs))]
                (if (= (length vals) (length (classC-field-names c)))
                    (objV class-name (map box vals))
                    (error 'interp "wrong field count")))]
        [getC (obj-expr field-name)
              (unbox (interp-get-field obj-expr classes field-name recur))]
        [sendC (obj-expr method-name arg-expr)
               (local [(define obj (recur obj-expr))
                       (define arg-val (recur arg-expr))]
                 (type-case Value obj
                   [objV (class-name field-vals)
                         (call-method class-name method-name classes
                                      obj arg-val)]
                   [else (error 'interp "not an object")]))]
        [ssendC (obj-expr class-name method-name arg-expr)
                (local [(define obj (recur obj-expr))
                        (define arg-val (recur arg-expr))]
                  (call-method class-name method-name classes
                               obj arg-val))]
        ;; instanceof
        [instanceofC (obj-expr super-name)
                     (local [(define obj (recur obj-expr))]
                       (type-case Value obj
                         [objV (class-name field-vals)
                               (if (instance? classes class-name super-name)
                                   (numV 0)
                                   (numV 1))]
                         ;; Unnecessary copy/paste. Typechecker would have caught this
                         [else (error 'interp "not an object")]))]
        ;; if0
        [if0C (cnd thn els)
              (let ([cond (recur cnd)])
                (if (eq? (numV-n cond) 0) ;; Typecheck ftw
                    (recur thn)
                    (recur els)))]

        ;; begin
        [beginC (exprs)
                (numV 0)]
        
        ;; set. The interpreter defines the result of set
        ;; to be the old value of the set field.
        [setC (obj-expr field-name val-expr)
              (let ([bx (interp-get-field obj-expr classes field-name recur)])
                (let ([old-val (unbox bx)])
                  (begin (set-box! bx (recur val-expr))
                         old-val)))]
                       
                ))))

(define (interp-get-field obj-expr classes field-name recur)
  (type-case Value (recur obj-expr)
    [objV (class-name field-vals)
          (type-case ClassC (find-class class-name classes)
            [classC (name field-names methods super-name)
                    (get-field-raw field-name field-names 
                                   field-vals)])]
    [else (error 'interp "not an object")]))

;; instance?
(define (instance? [classes : (listof ClassC)] [class-name : symbol] [super-name : symbol]) : boolean
  (cond
    [(symbol=? 'object super-name) true]
    [(symbol=? class-name 'object) false]
    [(symbol=? class-name super-name) true]
    [else
     (instance? classes
                (get-super classes class-name)
                super-name)]))

(define (get-super [classes : (listof ClassC)] [class-name : symbol]) : symbol
  (cond
    [(empty? (rest classes)) (classC-super-name (first classes))] ;; If we've made it this far, then class-name
                                                             ;; is definitely a valid class in the program
    [else (type-case ClassC (first classes)
            [classC (name field-names methods super)
                    (if (symbol=? name class-name)
                        super
                        (get-super (rest classes) class-name))])]))
;; end instance?

(define (call-method class-name method-name classes
                     obj arg-val)
  (type-case ClassC (find-class class-name classes)
    [classC (name field-names methods super-name)
            (type-case MethodC (find-method method-name methods)
              [methodC (name body-expr)
                       (interp body-expr
                               classes
                               obj
                               arg-val)])]))

(define (num-op [op : (number number -> number)]
                [op-name : symbol] 
                [x : Value]
                [y : Value]) : Value
  (cond
    [(and (numV? x) (numV? y))
     (numV (op (numV-n x) (numV-n y)))]
    [else (error 'interp "not a number")]))

(define (num+ x y) (num-op + '+ x y))
(define (num* x y) (num-op * '* x y))

;; ----------------------------------------
;; Examples

(module+ test
  (define posn-class
    (classC 
     'posn
     (list 'x 'y)
     (list (methodC 'mdist
                    (plusC (getC (thisC) 'x) (getC (thisC) 'y)))
           (methodC 'addDist
                    (plusC (sendC (thisC) 'mdist (numC 0))
                           (sendC (argC) 'mdist (numC 0))))
           (methodC 'addX
                    (plusC (getC (thisC) 'x) (argC)))
           (methodC 'multY (multC (argC) (getC (thisC) 'y)))
           (methodC 'factory12 (newC 'posn (list (numC 1) (numC 2)))))
     'object))

  (define posn3D-class
    (classC 
     'posn3D
     (list 'x 'y 'z)
     (list (methodC 'mdist (plusC (getC (thisC) 'z)
                                  (ssendC (thisC) 'posn 'mdist (argC))))
           (methodC 'addDist (ssendC (thisC) 'posn 'addDist (argC))))
     'posn))

  (define posn27 (newC 'posn (list (numC 2) (numC 7))))
  (define posn531 (newC 'posn3D (list (numC 5) (numC 3) (numC 1))))

  (define (interp-posn a)
    (interp a (list posn-class posn3D-class) (numV -1) (numV -1))))

;; ----------------------------------------

(module+ test
  (test (interp (numC 10) 
                empty (numV -1) (numV -1))
        (numV 10))
  (test (interp (plusC (numC 10) (numC 17))
                empty (numV -1) (numV -1))
        (numV 27))
  (test (interp (multC (numC 10) (numC 7))
                empty (numV -1) (numV -1))
        (numV 70))

  (test (interp-posn (newC 'posn (list (numC 2) (numC 7))))
        (objV 'posn (list (box (numV 2)) (box (numV 7)))))

  (test (interp-posn (sendC posn27 'mdist (numC 0)))
        (numV 9))
  
  (test (interp-posn (sendC posn27 'addX (numC 10)))
        (numV 12))

  (test (interp-posn (sendC (ssendC posn27 'posn 'factory12 (numC 0))
                            'multY
                            (numC 15)))
        (numV 30))

  (test (interp-posn (sendC posn531 'addDist posn27))
        (numV 18))
  
  (test/exn (interp-posn (plusC (numC 1) posn27))
            "not a number")
  (test/exn (interp-posn (getC (numC 1) 'x))
            "not an object")
  (test/exn (interp-posn (sendC (numC 1) 'mdist (numC 0)))
            "not an object")
  (test/exn (interp-posn (ssendC (numC 1) 'posn 'mdist (numC 0)))
            "not an object")
  (test/exn (interp-posn (newC 'posn (list (numC 0))))
            "wrong field count"))