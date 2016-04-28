#lang plai-typed
(require plai-typed/s-exp-match)

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

  ;; cast
  [castC (name : symbol)
         (obj-expr : ExprC)]

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

        ;; cast
        [castC (name obj-expr) 
               (let [(obj-v (recur obj-expr))]
                 (type-case Value obj-v
                   [objV (obj-name field-vals) 
                         (cond
                           ;; Check if we are casting to a super class
                           [(member name (get-super-classes obj-name classes empty)) obj-v]
                           [else (error 'interp "could not cast")])]
                   [else (error 'interp "can only cast objects")]))]

        ;; begin
        [beginC (exprs)
                (let ([vals (map recur exprs)])
                  (first (reverse vals)))]
        
        ;; set. The interpreter defines the result of set
        ;; to be the old value of the set field.
        [setC (obj-expr field-name val-expr)
              (let ([bx (interp-get-field obj-expr classes field-name recur)])
                (let ([old-val (unbox bx)])
                  (begin (set-box! bx (recur val-expr))
                         old-val)))]
                       
                ))))

;; get-super-clases
;; Maybe I should have kept track of this... Oh well, too late now.
;; At least it's tail recursive.
(define (get-super-classes name classes supers)
  (cond
    [(eq? 'object name) (cons 'object supers)]
    [else (get-super-classes (classC-super-name (find-class name classes))
                             classes
                             (cons name supers))]))

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
  (define classposn-class
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

  (define classposn3D-class
    (classC 
     'posn3D
     (list 'x 'y 'z)
     (list (methodC 'mdist (plusC (getC (thisC) 'z)
                                  (ssendC (thisC) 'posn 'mdist (argC))))
           (methodC 'addDist (ssendC (thisC) 'posn 'addDist (argC))))
     'posn))

  (define classposn27 (newC 'posn (list (numC 2) (numC 7))))
  (define classposn531 (newC 'posn3D (list (numC 5) (numC 3) (numC 1))))

  (define (classinterp-posn a)
    (interp a (list classposn-class classposn3D-class) (numV -1) (numV -1))))

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

  (test (classinterp-posn (newC 'posn (list (numC 2) (numC 7))))
        (objV 'posn (list (box (numV 2)) (box (numV 7)))))

  (test (classinterp-posn (sendC classposn27 'mdist (numC 0)))
        (numV 9))
  
  (test (classinterp-posn (sendC classposn27 'addX (numC 10)))
        (numV 12))

  (test (classinterp-posn (sendC (ssendC classposn27 'posn 'factory12 (numC 0))
                            'multY
                            (numC 15)))
        (numV 30))

  (test (classinterp-posn (sendC classposn531 'addDist classposn27))
        (numV 18))
  
  (test/exn (classinterp-posn (plusC (numC 1) classposn27))
            "not a number")
  (test/exn (classinterp-posn (getC (numC 1) 'x))
            "not an object")
  (test/exn (classinterp-posn (sendC (numC 1) 'mdist (numC 0)))
            "not an object")
  (test/exn (classinterp-posn (ssendC (numC 1) 'posn 'mdist (numC 0)))
            "not an object")
  (test/exn (classinterp-posn (newC 'posn (list (numC 0))))
            "wrong field count"))

;; class end


;; Make all "class.rkt" definitions available here, where
;; the "class.rkt" file must be in the same directory
;; as this one:
#;(require "class.rkt")

(define-type ExprI
  [numI (n : number)]
  [plusI (lhs : ExprI)
         (rhs : ExprI)]
  [multI (lhs : ExprI)
         (rhs : ExprI)]
  [argI]
  [thisI]
  [newI (class-name : symbol)
        (args : (listof ExprI))]
  [getI (obj-expr : ExprI)
        (field-name : symbol)]
  [sendI (obj-expr : ExprI)
         (method-name : symbol)
         (arg-expr : ExprI)]
  [beginI (exprs : (listof ExprI))]
  [superI (method-name : symbol)
          (arg-expr : ExprI)]
  
  ;; p2 instance of
  [instanceofI (class-expr : ExprI)
               (super : symbol)]

  ;; p3 if0
  [if0I (cnd : ExprI)
        (thn : ExprI)
        (els : ExprI)]
  ;; p5 cast
  [castI (name : symbol)
         (obj-expr : ExprI)]

  ;; p6 set
  [setI (obj-expr : ExprI)
        (name : symbol)
        (val-expr : ExprI)]
  )

(define-type ClassI
  [classI (name : symbol)
          (super-name : symbol)
          (field-names : (listof symbol))
          (methods : (listof MethodI))])

(define-type MethodI
  [methodI (name : symbol)
           (body-expr : ExprI)])

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define (expr-i->c [a : ExprI] [super-name : symbol]) : ExprC
  (local [(define (recur expr)
            (expr-i->c expr super-name))]
    (type-case ExprI a
      [numI (n) (numC n)]
      [plusI (l r) (plusC (recur l) (recur r))]
      [multI (l r) (multC (recur l) (recur r))]
      [argI () (argC)]
      [thisI () (thisC)]
      [newI (class-name field-exprs)
            (newC class-name (map recur field-exprs))]
      [getI (expr field-name)
            (getC (recur expr) field-name)]
      [sendI (expr method-name arg-expr)
             (sendC (recur expr)
                    method-name
                    (recur arg-expr))]
      [superI (method-name arg-expr)
              (ssendC (thisC)
                      super-name
                      method-name
                      (recur arg-expr))]
      [instanceofI (class-expr super)
                   (instanceofC (expr-i->c class-expr super-name)
                                super)]
      [if0I (cnd thn els)
            (if0C (recur cnd) (recur thn) (recur els))]
      [beginI (exprs)
              (beginC (map recur exprs))]
      [setI (obj-expr name val-expr)
            (setC (recur obj-expr) name (recur val-expr))]
      [castI (name obj-expr)
             (castC name (recur obj-expr))]
      )))

(module+ test
  (test (expr-i->c (numI 10) 'object)
        (numC 10))
  (test (expr-i->c (plusI (numI 10) (numI 2)) 'object)
        (plusC (numC 10) (numC 2)))
  (test (expr-i->c (multI (numI 10) (numI 2)) 'object)
        (multC (numC 10) (numC 2)))
  (test (expr-i->c (argI) 'object)
        (argC))
  (test (expr-i->c (thisI) 'object)
        (thisC))
  (test (expr-i->c (newI 'object (list (numI 1))) 'object)
        (newC 'object (list (numC 1))))
  (test (expr-i->c (getI (numI 1) 'x) 'object)
        (getC (numC 1) 'x))
  (test (expr-i->c (sendI (numI 1) 'mdist (numI 2)) 'object)
        (sendC (numC 1) 'mdist (numC 2)))
  (test (expr-i->c (superI 'mdist (numI 2)) 'posn)
        (ssendC (thisC) 'posn 'mdist (numC 2)))
  ;; instanceof
  (test (expr-i->c (instanceofI (newI 'empty empty) 'some-class) 'object)
          (instanceofC (newC 'empty empty) 'some-class))
  ;; if0
  (test (expr-i->c (if0I (argI) (thisI) (numI 1)) 'object)
        (if0C (argC) (thisC) (numC 1)))

  ;; begin
  (test (expr-i->c (beginI (list (numI 1) (numI 2) (numI 3)
                      (thisI) (argI) (plusI (numI 1) (numI 2)))) 'object)
        (beginC (list (numC 1) (numC 2) (numC 3)
                      (thisC) (argC) (plusC (numC 1) (numC 2)))))

  ;; set
  (test (expr-i->c (setI (newI 'posn3D (list (numI 1) (numI 2) (numI 3)))
              'x
              (plusI (numI 99) (numI 42))) 'object)
        (setC (newC 'posn3D (list (numC 1) (numC 2) (numC 3)))
              'x
              (plusC (numC 99) (numC 42))))
  
  )

;; ----------------------------------------

(define (method-i->c [m : MethodI] [super-name : symbol]) : MethodC
  (type-case MethodI m
    [methodI (name body-expr) 
             (methodC name 
                      (expr-i->c body-expr super-name))]))

(module+ test
  (define ihposn3d-mdist-i-method
    (methodI 'mdist
             (plusI (getI (thisI) 'z)
                    (superI 'mdist (argI)))))
  (define ihposn3d-mdist-c-method
    (methodC 'mdist
             (plusC (getC (thisC) 'z)
                    (ssendC (thisC) 'posn 'mdist (argC)))))
  
  (test (method-i->c ihposn3d-mdist-i-method 'posn)
        ihposn3d-mdist-c-method))

;; ----------------------------------------

(define (class-i->c-not-flat [c : ClassI]) : ClassC
  (type-case ClassI c
    [classI (name super-name field-names methods)
            (classC
             name
             field-names
             (map (lambda (m) (method-i->c m super-name))
                  methods)
             super-name)]))

(module+ test
  (define ihposn3d-i-class 
    (classI 'posn3d
            'posn
            (list 'z)
            (list ihposn3d-mdist-i-method)))
  (define ihposn3d-c-class-not-flat
    (classC 'posn3d
            (list 'z)
            (list ihposn3d-mdist-c-method)
            'posn))
  
  (test (class-i->c-not-flat ihposn3d-i-class)
        ihposn3d-c-class-not-flat))

;; ----------------------------------------

(define (flatten-class [c : ClassC] 
                       [classes : (listof ClassC)] 
                       [i-classes : (listof ClassI)]) : ClassC
  (type-case ClassC c
    [classC (name field-names methods super)
            (type-case ClassC (flatten-super name classes i-classes)
              [classC (super-name super-field-names super-methods super)
                      (classC
                       name
                       (add-fields super-field-names field-names)
                       (add/replace-methods super-methods methods)
                       super-name)])]))

(define (flatten-super [name : symbol]
                       [classes : (listof ClassC)] 
                       [i-classes : (listof ClassI)]) : ClassC
  (type-case ClassI (find-i-class name i-classes)
    [classI (name super-name field-names i-methods)
            (if (equal? super-name 'object)
                (classC 'object empty empty 'object)
                (flatten-class (find-class super-name classes)
                               classes
                               i-classes))]))

(module+ test
  (define ihposn-i-class 
    (classI 'posn
            'object
            (list 'x 'y)
            (list (methodI 'mdist
                           (plusI (getI (thisI) 'x)
                                  (getI (thisI) 'y)))
                  (methodI 'addDist
                           (plusI (sendI (thisI) 'mdist (numI 0))
                                  (sendI (argI) 'mdist (numI 0)))))))
  (define ihaddDist-c-method
    (methodC 'addDist
             (plusC (sendC (thisC) 'mdist (numC 0))
                    (sendC (argC) 'mdist (numC 0)))))
  (define ihposn-c-class-not-flat
    (classC 'posn
            (list 'x 'y)
            (list (methodC 'mdist
                           (plusC (getC (thisC) 'x)
                                  (getC (thisC) 'y)))
                  ihaddDist-c-method)
            'object))
  (define ihposn3d-c-class
    (classC 'posn3d
            (list 'x 'y 'z)
            (list ihposn3d-mdist-c-method
                  ihaddDist-c-method)
            'posn))
  
  (test (flatten-class ihposn3d-c-class-not-flat
                       (list ihposn-c-class-not-flat
                             ihposn3d-c-class-not-flat)
                       (list ihposn-i-class
                             ihposn3d-i-class))
        ihposn3d-c-class))

;; ----------------------------------------

(define add-fields append)

(define (add/replace-methods [methods : (listof MethodC)]
                             [new-methods : (listof MethodC)])
  : (listof MethodC)
  (cond
    [(empty? new-methods) methods]
    [else (add/replace-methods
           (add/replace-method methods (first new-methods))
           (rest new-methods))]))

(define (add/replace-method [methods : (listof MethodC)] 
                            [new-method : MethodC])
  : (listof MethodC)
  (cond
    [(empty? methods) (list new-method)]
    [else
     (if (equal? (methodC-name (first methods))
                 (methodC-name new-method))
         (cons new-method (rest methods))
         (cons (first methods) 
               (add/replace-method (rest methods)
                                   new-method)))]))

(module+ test
  (test (add-fields (list 'x 'y) (list 'z))
        (list 'x 'y 'z))
  
  (test (add/replace-methods empty empty)
        empty)
  (test (add/replace-methods empty (list (methodC 'm (numC 0))))
        (list (methodC 'm (numC 0))))
  (test (add/replace-methods (list (methodC 'm (numC 0))) empty)
        (list (methodC 'm (numC 0))))
  (test (add/replace-methods (list (methodC 'm (numC 0)))
                             (list (methodC 'm (numC 1))))
        (list (methodC 'm (numC 1))))
  (test (add/replace-methods (list (methodC 'm (numC 0))
                                   (methodC 'n (numC 2)))
                             (list (methodC 'm (numC 1))))
        (list (methodC 'm (numC 1))
              (methodC 'n (numC 2))))
  (test (add/replace-methods (list (methodC 'm (numC 0)))
                             (list (methodC 'm (numC 1))
                                   (methodC 'n (numC 2))))
        (list (methodC 'm (numC 1))
              (methodC 'n (numC 2))))
  
  (test (add/replace-method (list (methodC 'm (numC 0)))
                            (methodC 'm (numC 1)))
        (list (methodC 'm (numC 1))))
  (test (add/replace-method (list (methodC 'm (numC 0)))
                            (methodC 'n (numC 2)))
        (list (methodC 'm (numC 0))
              (methodC 'n (numC 2)))))

;; ----------------------------------------

(define find-i-class : (symbol (listof ClassI) -> ClassI)
  (make-find classI-name))

;; ----------------------------------------

(define (interp-i [i-a : ExprI] [i-classes : (listof ClassI)]) : Value
  (local [(define a (expr-i->c i-a 'object))
          (define classes-not-flat (map class-i->c-not-flat i-classes))
          (define classes
            (map (lambda (c)
                   (flatten-class c classes-not-flat i-classes))
                 classes-not-flat))]
    (interp a classes (numV -1) (numV -1))))

(module+ test
  (test (interp-i (numI 0) empty)
        (numV 0))
  
  (test (interp-i
         (sendI (newI 'posn3d (list (numI 5) (numI 3) (numI 1)))
                'addDist
                (newI 'posn (list (numI 2) (numI 7))))
         (list ihposn-i-class
               ihposn3d-i-class))
        (numV 18)))
;(trace interp-i)

;; end

#;(require plai-typed/s-exp-match
         "class.rkt"
         "inherit.rkt")

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define (parse-class [s : s-expression]) : ClassI
  (cond
    [(s-exp-match? `{class SYMBOL extends SYMBOL {ANY ...} ANY ...} s)
     (classI (s-exp->symbol (second (s-exp->list s)))
             (s-exp->symbol (fourth (s-exp->list s)))
             (map parse-field
                  (s-exp->list (fourth (rest (s-exp->list s)))))
             (map parse-method 
                  (rest (rest (rest (rest (rest (s-exp->list s))))))))]
    [else (error 'parse-class "invalid input")]))

(define (parse-field [s : s-expression]) : symbol
  (cond
    [(s-exp-match? `SYMBOL s)
     (s-exp->symbol s)]
    [else (error 'parse-field "invalid input")]))

(define (parse-method [s : s-expression]) : MethodI
  (cond
    [(s-exp-match? `{SYMBOL ANY} s)
     (methodI (s-exp->symbol (first (s-exp->list s)))
              (parse (second (s-exp->list s))))]
    [else (error 'parse-method "invalid input")]))

(define (parse [s : s-expression]) : ExprI
  (cond
    [(s-exp-match? `NUMBER s) (numI (s-exp->number s))]
    [(s-exp-match? `arg s) (argI)]
    [(s-exp-match? `this s) (thisI)]
    [(s-exp-match? '{+ ANY ANY} s)
     (plusI (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{* ANY ANY} s)
     (multI (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? '{new SYMBOL ANY ...} s)
     (newI (s-exp->symbol (second (s-exp->list s)))
           (map parse (rest (rest (s-exp->list s)))))]
    [(s-exp-match? '{get ANY SYMBOL} s)
     (getI (parse (second (s-exp->list s)))
           (s-exp->symbol (third (s-exp->list s))))]
    [(s-exp-match? '{send ANY SYMBOL ANY} s)
     (sendI (parse (second (s-exp->list s)))
            (s-exp->symbol (third (s-exp->list s)))
            (parse (fourth (s-exp->list s))))]
    [(s-exp-match? '{super SYMBOL ANY} s)
     (superI (s-exp->symbol (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    ;; instanceof
    [(s-exp-match? '{instanceof ANY SYMBOL} s)
     (instanceofI (parse (second (s-exp->list s)))
                  (s-exp->symbol (third (s-exp->list s))))]

    ;; if0
    [(s-exp-match? '{if0 ANY ANY ANY} s)
     (let ([ls (s-exp->list s)])
       (if0I (parse (second ls))
             (parse (third ls))
             (parse (fourth ls))))]

    ;; cast
    [(s-exp-match? '{cast SYMBOL ANY} s)
     (castI (s-exp->symbol (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]

    ;; begin
    [(s-exp-match? '{begin ANY ANY ...} s)
     (beginI (map parse (rest (s-exp->list s))))]
    
    ;; set
    [(s-exp-match? '{set ANY SYMBOL ANY} s)
     (let ([ls (s-exp->list s)])
       (setI (parse (second ls))
             (s-exp->symbol (third ls))
             (parse (fourth ls))))]
     
    
    [else (error 'parse "invalid input")]))

(module+ test
  (test (parse '0)
        (numI 0))
  (test (parse `arg)
        (argI))
  (test (parse `this)
        (thisI))
  (test (parse '{+ 1 2})
        (plusI (numI 1) (numI 2)))
  (test (parse '{* 1 2})
        (multI (numI 1) (numI 2)))
  (test (parse '{new posn 1 2})
        (newI 'posn (list (numI 1) (numI 2))))
  (test (parse '{get 1 x})
        (getI (numI 1) 'x))
  (test (parse '{send 1 m 2})
        (sendI (numI 1) 'm (numI 2)))
  (test (parse '{super m 1})
        (superI 'm (numI 1)))
  (test/exn (parse `x)
            "invalid input")
  
  (test (parse-field `x)
        'x)
  (test/exn (parse-field '{x 1})
            "invalid input")
  
  (test (parse-method `{m this})
        (methodI 'm (thisI)))
  (test/exn (parse-method `{m 1 2})
            "invalid input")
  
  (test (parse-class '{class posn3D extends posn
                        {x y z}
                        {m1 arg}
                        {m2 this}})
        (classI 'posn3D 'posn
                (list 'x 'y 'z)
                (list (methodI 'm1 (argI))
                      (methodI 'm2 (thisI)))))
  (test/exn (parse-class '{class})
            "invalid input")
  ;; instanceof
  (test (parse '{instanceof this some-class})
        (instanceofI (thisI) 'some-class))
  (test (parse '{instanceof 2 something})
        (instanceofI (numI 2) 'something))

  ;; test
  (test (parse '{if0 arg this 1})
        (if0I (argI) (thisI) (numI 1)))

  ;; begin
  (test (parse '{begin 1 2 3 this arg {+ 1 2}})
        (beginI (list (numI 1) (numI 2) (numI 3)
                      (thisI) (argI) (plusI (numI 1) (numI 2)))))

  ;; set
  (test (parse '{set {new posn3D 1 2 3} x {+ 99 42}})
        (setI (newI 'posn3D (list (numI 1) (numI 2) (numI 3)))
              'x
              (plusI (numI 99) (numI 42))))
  )

;; ----------------------------------------

#;(define (interp-prog [classes : (listof s-expression)] [a : s-expression]) : s-expression
  (let ([v (interp-i (parse a)
                     (map parse-class classes))])
    (type-case Value v
      [numV (n) (number->s-exp n)]
      [objV (class-name field-vals) `object])))

#;(module+ test
  (test (interp-prog
         (list
          '{class empty extends object
             {}})
         '{new empty})
        `object)
  ;; Invalid behavior, but just testing
  (test (interp-prog
         (list
          '{class empty extends object
             {}})
         `this)
        '-1)
  
  (test (interp-prog 
         (list
          '{class posn extends object
             {[x : num] [y : num]}
             {mdist : num -> num {+ {get this x} {get this y}}}
             {addDist : posn -> num {+ {send arg mdist 0}
                         {send this mdist 0}}}}
          
          '{class posn3D extends posn
             {[z : num]}
             {mdist : num -> num {+ {get this z} 
                       {super mdist arg}}}})
         
         '{send {new posn3D 5 3 1} addDist {new posn 2 7}})
        '18))
;(trace parse)

;; end inherit-parse




#;(require "class.rkt"
         "inherit.rkt")

(define-type ClassT
  [classT (name : symbol)
          (super-name : symbol)
          (fields : (listof FieldT))
          (methods : (listof MethodT))])

(define-type FieldT
  [fieldT (name : symbol)
          (type : Type)])

(define-type MethodT
  [methodT (name : symbol)
           (arg-type : Type)
           (result-type : Type)
           (body-expr : ExprI)])

(define-type Type
  [numT]
  [objT (class-name : symbol)])

(module+ test
  (print-only-errors true))

;; ----------------------------------------

(define find-classT
  (make-find classT-name))

(define find-fieldT
  (make-find fieldT-name))

(define find-methodT
  (make-find methodT-name))

(define (type-error a msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string a)
                      (string-append " not "
                                     msg)))))

(define (get-all-field-types class-name t-classes)
  (if (equal? class-name 'object)
      empty        
      (type-case ClassT (find-classT class-name t-classes)
        [classT (name super-name fields methods)
                (append 
                 (get-all-field-types super-name t-classes)
                 (map fieldT-type fields))])))

;; ----------------------------------------

(define (make-find-in-tree find-in-list extract)
  (lambda (name t-class t-classes)
    (local [(define items (extract t-class))
            (define super-name 
              (classT-super-name t-class))]
      (if (equal? super-name 'object)
          (find-in-list name items)
          (try (find-in-list name items)
               (lambda ()
                 ((make-find-in-tree find-in-list extract)
                  name 
                  (find-classT super-name t-classes)
                  t-classes)))))))

(define find-field-in-tree
  (make-find-in-tree find-fieldT classT-fields))

(define find-method-in-tree
  (make-find-in-tree find-methodT classT-methods))

;; ----------------------------------------

(define (is-subclass? name1 name2 t-classes)
  (cond
    [(equal? name1 name2) true]
    [(equal? name1 'object) false]
    [else
     (type-case ClassT (find-classT name1 t-classes)
       [classT (name super-name fields methods)
               (is-subclass? super-name name2 t-classes)])]))

(define (is-subtype? t1 t2 t-classes)
  (type-case Type t1
    [objT (name1)
          (type-case Type t2 
            [objT (name2)
                  (is-subclass? name1 name2 t-classes)]
            [else false])]
    [else (equal? t1 t2)]))

(module+ test
  (define tca-t-class (classT 'a 'object empty empty))
  (define tcb-t-class (classT 'b 'a empty empty))

  (test (is-subclass? 'object 'object empty)
        true)
  (test (is-subclass? 'a 'b (list tca-t-class tcb-t-class))
        false)
  (test (is-subclass? 'b 'a (list tca-t-class tcb-t-class))
        true)

  (test (is-subtype? (numT) (numT) empty)
        true)
  (test (is-subtype? (numT) (objT 'object) empty)
        false)
  (test (is-subtype? (objT 'object) (numT) empty)
        false)
  (test (is-subtype? (objT 'a) (objT 'b) (list tca-t-class tcb-t-class))
        false)
  (test (is-subtype? (objT 'b) (objT 'a) (list tca-t-class tcb-t-class))
        true))

;; ----------------------------------------

(define typecheck-expr : (ExprI (listof ClassT) Type Type boolean -> Type)
  (lambda (expr t-classes arg-type this-type is-main)
    (local [(define (recur expr)
              (typecheck-expr expr t-classes arg-type this-type is-main))
            (define (typecheck-nums l r)
              (type-case Type (recur l)
                [numT ()
                      (type-case Type (recur r)
                        [numT () (numT)]
                        [else (type-error r "num")])]
                [else (type-error l "num")]))]
      (type-case ExprI expr
        [numI (n) (numT)]
        [plusI (l r) (typecheck-nums l r)]
        [multI (l r) (typecheck-nums l r)]
        [argI () (if is-main
                     (error 'typecheck "\"arg\" not allowed in main expression")
                     arg-type)]
        [thisI ()  (if is-main
                       (error 'typecheck "\"this\" not allowed in main expression")
                       this-type)]
        [newI (class-name exprs)
              (local [(define arg-types (map recur exprs))
                      (define field-types
                        (get-all-field-types class-name t-classes))]
                (if (and (= (length arg-types) (length field-types))
                         (foldl (lambda (b r) (and r b))
                                true
                                (map2 (lambda (t1 t2) 
                                        (is-subtype? t1 t2 t-classes))
                                      arg-types
                                      field-types)))
                    (objT class-name)
                    (type-error expr "field type mismatch")))]
        [getI (obj-expr field-name)
              (type-case Type (recur obj-expr)
                [objT (class-name)
                      (local [(define t-class
                                (find-classT class-name t-classes))
                              (define field
                                (try
                                 (find-field-in-tree field-name
                                                    t-class
                                                    t-classes)
                                 (λ () (type-error obj-expr "field not found"))))]
                        (type-case FieldT field
                          [fieldT (name type) type]))]
                [else (type-error obj-expr "object")])]
        [sendI (obj-expr method-name arg-expr)
               (local [(define obj-type (recur obj-expr))
                       (define arg-type (recur arg-expr))]
                 (type-case Type obj-type
                   [objT (class-name)
                         (typecheck-send class-name method-name
                                         arg-expr arg-type
                                         t-classes)]
                   [else
                    (type-error obj-expr "object")]))]
        [superI (method-name arg-expr)
                (local [(define arg-type (recur arg-expr))
                        (define this-class
                          (find-classT (objT-class-name this-type)
                                       t-classes))]
                  (typecheck-send (classT-super-name this-class)
                                  method-name
                                  arg-expr arg-type
                                  t-classes))]
        ;; instanceofc
        [instanceofI (obj-expr super-name)
                     (local [(define obj-type (recur obj-expr))]
                         (type-case Type obj-type
                           [numT () (type-error obj-expr "numbers are not objects")]
                           [else (numT)]))]

        ;; if0
        [if0I (cnd thn els)
              (let ([cnd-type (recur cnd)])
                (type-case Type cnd-type
                  [numT () (let ([thn-type (recur thn)]
                                 [els-type (recur els)])
                             ;; Find subtype (if there is one)
                             (cond
                               [(is-subtype? thn-type els-type t-classes) els-type]
                               [(is-subtype? els-type thn-type t-classes) thn-type]
                               [else
                                (type-case (optionof Type) (if0-find-common-type thn-type
                                                                                els-type
                                                                                t-classes)
                                  [some (t) t]
                                  [none () (type-error thn "incompatible if types")])]))]
                  [else (type-error cnd "not a number")]))]

        ;; cast
        [castI (name obj-expr)
               (let ([obj-t (recur obj-expr)])
                 (type-case Type obj-t
                   [objT (class-name) (cond
                                        [(is-subtype? obj-t (objT name) t-classes) (objT name)]
                                        [(is-subtype? (objT name) obj-t t-classes) (objT name)]
                                        [else (type-error obj-expr (string-append "could not cast to "
                                                                                  (symbol->string name)))])]
                   [else (type-error obj-expr "can only cast objects")]))]

        ;; begin - typecheck all expressions, then return the type of the final
        ;; expression as the expression type
        [beginI (exprs)
                (let ([exprs-t (map recur exprs)]) ;; We want typecheck side-effects
                  (first (reverse exprs-t)))] ;; Return type of last expression
        
        ;; Set
        [setI (obj-expr field-name val-expr)
              (let ([obj-t (recur obj-expr)]
                    [val-t (recur val-expr)])
                (type-case Type obj-t
                  [numT () (type-error obj-expr "cannot assign")]
                  [objT (target-class)
                        (local [(define t-class
                                  (find-classT target-class t-classes))
                                (define field
                                  (try
                                   (find-field-in-tree field-name
                                                       t-class
                                                       t-classes)
                                   (λ () (type-error obj-expr "field not found"))))]
                          (type-case FieldT field
                            [fieldT (name type)
                                    (cond
                                      [(and (numT? type) (numT? val-t)) type]
                                      [(is-subtype? val-t type t-classes) type]
                                      [else (type-error val-t "cannot assign")])]))]))]))))

(define (if0-find-common-type [thn-t : Type] [els-t : Type] [t-classes : (listof ClassT)]) : (optionof Type)
  (let* ([class-types (map (λ (x)
                             [objT (classT-name x)]) t-classes)]
         [thn-supers (filter (λ (x)
                               (is-subtype? thn-t x t-classes)) class-types)]
         [els-supers (filter (λ (x)
                               (is-subtype? els-t x t-classes)) class-types)]
         [common-supers (filter (λ (x)
                                  (member x els-supers)) thn-supers)])
    (if (eq? (length common-supers) 0)
        (none)
        (some (if0-LUB common-supers t-classes)))))

(define (if0-LUB (common-supers : (listof Type)) (t-classes : (listof ClassT))) : Type
  (cond
    [(eq? (length common-supers) 1) (first common-supers)]
    [else (if0-LUB (filter (λ (x)
                             (if0-has-super x common-supers t-classes)) common-supers)
                   t-classes)])
 )

(define (if0-has-super (t : Type) (supers : (listof Type)) (t-classes : (listof ClassT))) : boolean
  (foldl (λ (x y)
           (and x y))
         true
         (map (λ (x)
                (is-subtype? t x t-classes)) supers)))

(define (typecheck-send [class-name : symbol]
                        [method-name : symbol]
                        [arg-expr : ExprI]
                        [arg-type : Type]
                        [t-classes : (listof ClassT)])
  (type-case MethodT (find-method-in-tree
                      method-name
                      (find-classT class-name t-classes)
                      t-classes)
    [methodT (name arg-type-m result-type body-expr)
             (if (is-subtype? arg-type arg-type-m t-classes)
                 result-type
                 (type-error arg-expr (to-string arg-type-m)))]))

(define (typecheck-method [method : MethodT]
                          [this-type : Type]
                          [t-classes : (listof ClassT)]) : ()
  (type-case MethodT method
    [methodT (name arg-type result-type body-expr)
             (if (is-subtype? (typecheck-expr body-expr t-classes
                                              arg-type this-type false)
                              result-type
                              t-classes)
                 (values)
                 (type-error body-expr (to-string result-type)))]))

(define (check-override [method : MethodT]
                        [this-class : ClassT]
                        [t-classes : (listof ClassT)])
  (local [(define super-name 
            (classT-super-name this-class))
          (define super-method
            (try
             ;; Look for method in superclass:
             (find-method-in-tree (methodT-name method)
                                  (find-classT super-name t-classes)
                                  t-classes)
             ;; no such method in superclass:
             (lambda () method)))]
    (if (and (equal? (methodT-arg-type method)
                     (methodT-arg-type super-method))
             (equal? (methodT-result-type method)
                     (methodT-result-type super-method)))
        (values)
        (error 'typecheck (string-append
                           "bad override of "
                           (to-string (methodT-name method)))))))

(define (typecheck-class [t-class : ClassT] [t-classes : (listof ClassT)])
  (type-case ClassT t-class
    [classT (name super-name fields methods)
            (map (lambda (m)
                   (begin
                     (typecheck-method m (objT name) t-classes)
                     (check-override m t-class t-classes)))
                 methods)]))

(define (typecheck [a : ExprI] [t-classes : (listof ClassT)]) : Type(begin
    (map (lambda (t-class)
           (typecheck-class t-class t-classes))
         t-classes)
    (typecheck-expr a t-classes (numT) (objT 'bad) true)))

;; ----------------------------------------

(module+ test
  (define tcposn-t-class
    (classT 'posn 'object
            (list (fieldT 'x (numT)) (fieldT 'y (numT)))
            (list (methodT 'mdist (numT) (numT) 
                           (plusI (getI (thisI) 'x) (getI (thisI) 'y)))
                  (methodT 'addDist (objT 'posn) (numT)
                           (plusI (sendI (thisI) 'mdist (numI 0))
                                  (sendI (argI) 'mdist (numI 0)))))))

  (define tcposn3D-t-class 
    (classT 'posn3D 'posn
            (list (fieldT 'z (numT)))
            (list (methodT 'mdist (numT) (numT)
                           (plusI (getI (thisI) 'z) 
                                  (superI 'mdist (argI)))))))

  (define tcsquare-t-class 
    (classT 'square 'object
            (list (fieldT 'topleft (objT 'posn)))
            (list)))

  (define (tctypecheck-posn a)
    (typecheck a
               (list tcposn-t-class tcposn3D-t-class tcsquare-t-class)))
  
  (define tcposn27 (newI 'posn (list (numI 2) (numI 7))))
  (define tcposn531 (newI 'posn3D (list (numI 5) (numI 3) (numI 1))))

  (test (tctypecheck-posn (sendI tcposn27 'mdist (numI 0)))
        (numT))
  (test (tctypecheck-posn (sendI tcposn531 'mdist (numI 0)))
        (numT))  
  (test (tctypecheck-posn (sendI tcposn531 'addDist tcposn27))
        (numT))  
  (test (tctypecheck-posn (sendI tcposn27 'addDist tcposn531))
        (numT))

  (test (tctypecheck-posn (newI 'square (list (newI 'posn (list (numI 0) (numI 1))))))
        (objT 'square))
  (test (tctypecheck-posn (newI 'square (list (newI 'posn3D (list (numI 0) (numI 1) (numI 3))))))
        (objT 'square))
  
  (test (typecheck (multI (numI 1) (numI 2))
                   empty)
        (numT))
  
  (test/exn (tctypecheck-posn (sendI (numI 10) 'mdist (numI 0)))
            "no type")
  (test/exn (tctypecheck-posn (sendI tcposn27 'mdist tcposn27))
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'object empty))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (newI 'object empty) (numI 1))
                       empty)
            "no type")
  (test/exn (typecheck (plusI (numI 1) (newI 'object (list (numI 1))))
                       empty)
            "no type")
  (test/exn (typecheck (getI (numI 1) 'x)
                       empty)
            "no type")
  (test/exn (typecheck (numI 10)
                       (list tcposn-t-class 
                             (classT 'other 'posn
                                     (list)
                                     (list (methodT 'mdist 
                                                    (objT 'object) (numT)
                                                    (numI 10))))))
            "bad override")
            
  (test/exn (typecheck-method (methodT 'm (numT) (objT 'object) (numI 0)) (objT 'object) empty)
            "no type")
  (test/exn (typecheck (numI 0)
                       (list tcsquare-t-class
                             (classT 'cube 'square
                                     empty
                                     (list
                                      (methodT 'm (numT) (numT)
                                               ;; No such method in superclass:
                                               (superI 'm (numI 0)))))))
            "not found"))

;; ----------------------------------------

(define strip-types : (ClassT -> ClassI)
  (lambda (t-class)
    (type-case ClassT t-class
      [classT (name super-name fields methods)
              (classI
               name 
               super-name
               (map fieldT-name fields)
               (map (lambda (m)
                      (type-case MethodT m
                        [methodT (name arg-type result-type body-expr)
                                 (methodI name body-expr)]))
                    methods))])))

(define interp-t : (ExprI (listof ClassT) -> Value)
  (lambda (a t-classes)
    (interp-i a
              (map strip-types t-classes))))

(module+ test
  (define (tcinterp-t-posn a)
    (interp-t a
              (list tcposn-t-class tcposn3D-t-class)))
  
  (test (tcinterp-t-posn (sendI tcposn27 'mdist (numI 0)))
        (numV 9))  
  (test (tcinterp-t-posn (sendI tcposn531 'mdist (numI 0)))
        (numV 9))
  (test (tcinterp-t-posn (sendI tcposn531 'addDist tcposn27))
        (numV 18))
  (test (tcinterp-t-posn (sendI tcposn27 'addDist tcposn531))
        (numV 18)))
;(trace typecheck)

;; end type-class

#;(require plai-typed/s-exp-match
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

#;(require plai-typed/s-exp-match
         "../typed-class.rkt"
         "../typed-parse.rkt")

(module+ test
  ;; 1. Fix this/arg
  
  ;; Relevant changes are in typed-class.rkt. An 'is-main' argument is added which determines
  ;; if argI or thisI are allowed in the expression. If true, these are disallowed, otherwise,
  ;; we're in a method and they're ok.
  (define p1classes (list
                     '{class posn extends object
                        {[x : num] [y : num]}
                        {mdist : num -> num {+ {get this x} {get this y}}}
                        {addDist : posn -> num {+ {send arg mdist 0}
                                                  {send this mdist 0}}}
                        {sety-x : num -> posn {begin
                                                {set this y {get this x}}
                                                this}}}
                     '{class posn3D extends posn
                        {[z : num]}
                        {mdist : num -> num {+ {get this z} 
                                               {super mdist arg}}}}))
  (test/exn (typecheck-prog p1classes
               '{get {new posn 1 2} w})
            "not found")
  (test/exn (typecheck-prog p1classes
                            `this)
            "not allowed")
  
  (test/exn (typecheck-prog p1classes
                            `arg)
            "not allowed")
  
  (test (typecheck-prog p1classes
                        '(* 1 2))
        `num)

  (test/exn (typecheck-prog p1classes
                        '(* this 2))
        "not allowed")
  
  (test/exn (typecheck-prog p1classes
                        '(* 1 arg))
        "not allowed"))

;; End 1. Fix arg/this


#;(require plai-typed/s-exp-match
         "../typed-parse.rkt")


(module+ test
  ;; 2. instanceof form
  (define p2posn-class
    '{class posn extends object
             {[x : num] [y : num]}
             {mdist : num -> num {+ {get this x} {get this y}}}
             {addDist : posn -> num {+ {send arg mdist 0}
                                      {send this mdist 0}}}})
  
  (define p2posn3D-class
    '{class posn3D extends posn
             {[z : num]}
             {mdist : num -> num {+ {get this z} 
                                  {super mdist arg}}}})
  
  (define p2posn4D-class
    '{class posn4D extends posn3D
             {[w : num]}
             {mdist : num -> num {+ {get this w} 
                       {super mdist arg}}}})
  
  (define p2empty-class '{class empty extends object {}})
  
  ;; Implementation details

  ;; Tests
  (test/exn (interp-t-prog empty
                           '{instanceof 2 object})
            "number")
  (test/exn (interp-prog empty
                           '{instanceof 2 object})
            "not")
  (test (interp-t-prog 
         (list p2empty-class p2posn4D-class p2posn-class p2posn3D-class)
         '{instanceof {new empty} posn3D})
        '1)
  (test (interp-t-prog 
         (list p2posn4D-class p2posn-class p2posn3D-class)
         '{instanceof {new posn4D 5 3 4 2} object})
        '0)
    (test (interp-t-prog 
         (list p2posn4D-class p2posn-class p2posn3D-class)
         '{instanceof {new posn4D 5 3 4 2} posn3D})
        '0)
  (test (interp-t-prog 
         (list p2posn4D-class p2posn-class p2posn3D-class)
         '{instanceof {new posn4D 5 3 4 2} posn})
        '0)
  (test (interp-t-prog 
         (list p2posn-class p2posn3D-class)
         '{instanceof {new posn 5 3} posn})
        '0)

  (test (interp-t-prog 
         (list p2posn-class p2posn3D-class)         
         '{instanceof {new posn 5 3} object})
        '0)
  (test (interp-t-prog 
         (list p2posn-class p2posn3D-class)
         '{instanceof {new posn 5 3} posn3D})
        '1)
  (test (interp-t-prog 
         (list p2posn-class p2posn3D-class)
         '{instanceof {new posn3D 5 3 1} posn})
        '0)

  ;; Typecheck
  (test/exn (typecheck-prog empty
                           '{instanceof 2 object})
            "number")
  (test (typecheck-prog 
         (list p2empty-class p2posn4D-class p2posn-class p2posn3D-class)
         '{instanceof {new empty} posn3D})
        `num)
  (test (typecheck-prog 
         (list p2posn4D-class p2posn-class p2posn3D-class)
         '{instanceof {new posn4D 5 3 4 2} object})
        `num)
    (test (typecheck-prog 
         (list p2posn4D-class p2posn-class p2posn3D-class)
         '{instanceof {new posn4D 5 3 4 2} posn3D})
        `num)
  (test (typecheck-prog 
         (list p2posn4D-class p2posn-class p2posn3D-class)
         '{instanceof {new posn4D 5 3 4 2} posn})
        `num)
  (test (typecheck-prog 
         (list p2posn-class p2posn3D-class)
         '{instanceof {new posn 5 3} posn})
        `num)

  (test (typecheck-prog 
         (list p2posn-class p2posn3D-class)         
         '{instanceof {new posn 5 3} object})
        `num)
  (test (typecheck-prog 
         (list p2posn-class p2posn3D-class)
         '{instanceof {new posn 5 3} posn3D})
        `num)
  (test (typecheck-prog 
         (list p2posn-class p2posn3D-class)
         '{instanceof {new posn3D 5 3 1} posn})
        `num)
  
  )

;; End 2. instanceof form

#;(require plai-typed/s-exp-match
         "../typed-parse.rkt")

#;(module+ test
    (print-only-errors true))

(module+ test
  ;; 3. if0 form
  (define p3posn-class
    '{class posn extends object
       {[x : num] [y : num]}
       {mdist : num -> num {+ {get this x} {get this y}}}
       {addDist : posn -> num {+ {send arg mdist 0}
                                 {send this mdist 0}}}})
  
  (define p3posn3D-class
    '{class posn3D extends posn
       {[z : num]}
       {mdist : num -> num {+ {get this z} 
                              {super mdist arg}}}})
  
  (define p3posn4D-class
    '{class posn4D extends posn3D
       {[w : num]}
       {mdist : num -> num {+ {get this w} 
                              {super mdist arg}}}})
  
  (define empty-class '{class empty extends object {}})
  
  ;; Implementation details
  
  ;; Tests
  (test (interp-t-prog 
         (list p3posn-class p3posn3D-class)         
         '(if0 {instanceof {new posn 5 3} object}
               1
               2))
        '1)
  
  (test (interp-t-prog 
         (list p3posn-class p3posn3D-class)
         '(if0 {instanceof {new posn 5 3} posn3D}
               1
               2))
        '2)
  
  (test/exn (interp-t-prog 
             (list p3posn-class p3posn3D-class)
             '{if0 {new posn3D 5 3 1}
                   1
                   42})
            "number")
  
  (test/exn (interp-t-prog 
             (list p3posn-class p3posn3D-class)
             '{if0 0
                   {new posn 1 2}
                   42})
            "incompatible")
  (test (interp-t-prog 
         (list p3posn-class p3posn3D-class)
         '{if0 0
               {new posn 1 2}
               {new posn3D 4 5 6}})
        `object)
  
  (test (interp-t-prog 
         (list p3posn-class p3posn3D-class)
         '{get {if0 0
                    {new posn 1 2}
                    {new posn3D 4 5 6}} x})
        `1)
  (test (interp-t-prog 
         (list p3posn-class p3posn3D-class)
         '{instanceof {if0 0
                           {new posn3D 4 5 6}
                           {new posn 1 2}} posn})
        `0)
  (test (interp-t-prog 
         (list p3posn-class p3posn3D-class)
         '{instanceof {if0 0
                           {new posn 1 2}
                           {new posn3D 4 5 6}} posn})
        `0)
  ;; Types...
    (test (typecheck-prog 
         (list p3posn-class p3posn3D-class)         
         '(if0 {instanceof {new posn 5 3} object}
               1
               2))
        `num)
  
  (test (typecheck-prog
         (list p3posn-class p3posn3D-class)
         '(if0 {instanceof {new posn 5 3} posn3D}
               1
               2))
        `num)
  
  (test/exn (typecheck-prog 
             (list p3posn-class p3posn3D-class)
             '{if0 {new posn3D 5 3 1}
                   1
                   42})
            "number")
  
  (test/exn (typecheck-prog
             (list p3posn-class p3posn3D-class)
             '{if0 0
                   {new posn 1 2}
                   42})
            "incompatible")
  (test (typecheck-prog 
         (list p3posn-class p3posn3D-class)
         '{if0 0
               {new posn 1 2}
               {new posn3D 4 5 6}})
        `posn)
  
  (test (typecheck-prog
         (list p3posn-class p3posn3D-class)
         '{get {if0 0
                    {new posn 1 2}
                    {new posn3D 4 5 6}} x})
        `num)
  (test (typecheck-prog 
         (list p3posn-class p3posn3D-class)
         '{instanceof {if0 0
                           {new posn3D 4 5 6}
                           {new posn 1 2}} posn})
        `num)
  (test (typecheck-prog 
         (list p3posn-class p3posn3D-class)
         '{instanceof {if0 0
                           {new posn 1 2}
                           {new posn3D 4 5 6}} posn})
        `num)
  )

;; End 3. 


#;(require plai-typed/s-exp-match
         "../typed-parse.rkt")

#;(module+ test
  (print-only-errors true))

(module+ test
  ;; 4. generalized if types. Tests supersede part 3 tests.
  
  ;; Classes
  (define p4A '{class A extends object{}})
  (define p4AA '{class AA extends A{}})
  (define p4AAA '{class AAA extends AA{}})
  (define p4B '{class B extends AAA{}})
  (define p4BB '{class BB extends B{}})
  (define p4BA '{class BA extends B{}})
  (define p4C '{class C extends AAA{}})
  (define p4CC '{class CC extends C{}})
  (define p4CA '{class CA extends C{}})
  (define p4empty '{class empty extends object {}})
  (define p4classes (list p4A p4AA p4AAA p4B p4BB p4BA p4C p4CC p4CA p4empty))
  
  ;; Programs
  (define p4prog1 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new A}
                             {new AA}})))
  (define p4prog2 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new C}
                             {new B}})))
  (define p4prog3 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new CC}
                             {new BA}})))
  (define p4prog4 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new B}
                             {new BA}})))
  (define p4prog5 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new A}
                             {new CA}})))
  (define p4prog6 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new AA}
                             {new B}})))
  (define p4prog7 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new AA}
                             {new empty}})))
  (define p4prog8 (λ (r)
                    (r p4classes
                       '{if0 0
                             2
                             {new B}})))
  (define p4prog9 (λ (r)
                    (r p4classes
                       '{if0 0
                             2
                             9})))
  
  ;; implementation: typed-class.rkt
  ;; if0-LUB if0-has-super if0-find-common-type
  
  ;; Tests
  (test (p4prog1 typecheck-prog)
        `A)
  (test (p4prog2 typecheck-prog)
        `AAA)
  (test (p4prog3 typecheck-prog)
        `AAA)
  (test (p4prog4 typecheck-prog)
        `B)
  (test (p4prog5 typecheck-prog)
        `A)
  (test (p4prog6 typecheck-prog)
        `AA)
  (test/exn (p4prog7 typecheck-prog)
            "compatible")
  (test/exn (p4prog8 typecheck-prog)
            "compatible")
  (test (p4prog9 typecheck-prog)
        `num)
  
  )

;; End 4. 

#;(require plai-typed/s-exp-match
         "../typed-parse.rkt")

#;(module+ test
    (print-only-errors true))

(module+ test
  ;; 5. cast form
  
  ;; Implementation details
  
  ;; Classes
  (define p5A '{class A extends object{}})
  (define p5AA '{class AA extends A{}})
  (define p5AAA '{class AAA extends AA{}})
  (define p5B '{class B extends AAA{}})
  (define p5BB '{class BB extends B{}})
  (define p5BA '{class BA extends B{}})
  (define p5C '{class C extends AAA{}})
  (define p5CC '{class CC extends C{}})
  (define p5CA '{class CA extends C{}})
  (define p5empty '{class empty extends object {}})
  (define p5classes (list p5A p5AA p5AAA p5B p5BB p5BA p5C p5CC p5CA p5empty))
  
  ;; Tests programs
  (define p5prog1 (λ (r)
                    (r p5classes
                       '{cast A {new AA}})))
  (define p5prog2 (λ (r)
                    (r p5classes
                       '{cast A {new BB}})))
  (define p5prog3 (λ (r)
                    (r p5classes
                       '{cast C {new BB}})))
  (define p5prog4 (λ (r)
                    (r p5classes
                       '{cast A 2})))
  (define p5prog5 (λ (r)
                    (r p5classes
                       '{cast AA {new CA}})))
  (define p5prog6 (λ (r)
                    (r p5classes
                       '{cast object {new BB}})))
  (define p5prog7 (λ (r)
                    (r p5classes
                       '{cast A {new A}})))
  (define p5prog8 (λ (r)
                    (r p5classes
                       '{cast CA {new AA}})))
  ;; Runtime tests
  (test (p5prog1 interp-prog)
        `object)
  (test (p5prog2 interp-prog)
        `object)
  (test/exn (p5prog3 interp-prog)
            "cast")
  (test/exn (p5prog4 interp-prog)
            "cast")
  (test (p5prog5 interp-prog)
        `object)
  (test (p5prog6 interp-prog)
        `object)
  (test (p5prog7 interp-prog)
        `object)
  (test/exn (p5prog8 interp-prog)
            "cast")
  ;; Typecheck tests
  (test (p5prog1 typecheck-prog)
        `A)
  (test (p5prog2 typecheck-prog)
        `A)
  (test/exn (p5prog3 typecheck-prog)
            "cast")
  (test/exn (p5prog4 typecheck-prog)
            "cast")
  (test (p5prog5 typecheck-prog)
        `AA)
  (test (p5prog6 typecheck-prog)
        `object)
  (test (p5prog7 typecheck-prog)
        `A)
  (test (p5prog8 typecheck-prog)
        `CA)
  )

;; End 5. 


#;(require plai-typed/s-exp-match
         "../typed-class.rkt"
         "../typed-parse.rkt")

#;(module+ test
  (print-only-errors true))

(module+ test
  ;; 6. Imperative set
  (define p6posn-class
    '{class posn extends object
       {[x : num] [y : num]}
       {mdist : num -> num {+ {get this x} {get this y}}}
       {addDist : posn -> num {+ {send arg mdist 0}
                                 {send this mdist 0}}}
       {sety-x : num -> posn {begin
                               {set this y {get this x}}
                               this}}})
  
  (define p6posn3D-class
    '{class posn3D extends posn
       {[z : num]}
       {mdist : num -> num {+ {get this z} 
                              {super mdist arg}}}})
  
  (define p6posn4D-class
    '{class posn4D extends posn3D
       {[w : num]}
       {mdist : num -> num {+ {get this w} 
                              {super mdist arg}}}})
  
  (define p6posn-pair-class
    '{class posn-pair extends object
       {[p1 : posn]
        [p2 : posn]}
       {addDist : num -> num {send {get this p1} addDist {get this p2}}}
       {setp2 : posn -> posn-pair
              {begin
                {set this p2 arg}
                this}}})
  
  (define p6posn-pair2-class
    '{class posn-pair2 extends object
       {[p1 : posn3D]
        [p2 : posn3D]}
       {addDist : num -> num {send {get this p1} addDist {get this p2}}}
       {setp2 : posn -> posn-pair
              {begin
                {set this p2 arg}
                this}}})
  
  (define set-class
    '{class setclass extends object
       {[x : num]}
       {terminates? : num -> num {if0 {get this x}
                                      {get this x}
                                      {begin
                                        {set this x {+ {get this x} -1}}
                                        {send this terminates? arg}}}}})
  
  (define p6empty-class '{class empty extends object {}})
  
  ;; Programs
  (define p6prog1 (λ (r)
                    (r (list set-class)
                       '{send {new setclass 3}
                              terminates? 0}))) ;; Fun program
  (define p6prog2 (λ (r)
                    (r (list p6posn-class)
                       '{get {send {new posn 3 2} sety-x 0} y}))) ;; Easier to understand
  (define p6prog3 (λ (r)
                    (r (list p6posn-class)
                       '(set {new posn 1 2} w 7)))) ;; Checks field is available for assignment
  (define p6prog4 (λ (r)
                    (r (list p6posn-class p6posn3D-class p6posn-pair-class)
                       '{send
                         {send
                          {new posn-pair {new posn 1 2} {new posn 3 4}}
                          setp2
                          {new posn3D 5 6 7}}
                         addDist
                         0}))) ;; Set field to subclass
  (define p6prog5 (λ (r)
                    (r (list set-class p6posn-class p6posn3D-class p6posn-pair-class)
                       '{send
                         {send
                          {new posn-pair {new posn 1 2} {new posn 3 4}}
                          setp2
                          {new setclass 2}}
                         addDist
                         0}))) ;; Set field to non subclass
  (define p6prog6 (λ (r)
                    (r (list set-class p6posn-class p6posn3D-class p6posn-pair-class)
                       '{send
                         {send
                          {new posn-pair {new posn 1 2} {new posn 3 4}}
                          setp2
                          2}
                         addDist
                         0}))) ;; set obj field to num
  (define p6prog8 (λ (r)
                    (r (list set-class p6posn-class p6posn3D-class p6posn-pair2-class)
                       '{send
                         {send
                          {new posn-pair2 {new posn3D 1 2 3} {new posn3D 4 5 6}}
                          setp2
                          {new posn 7 8}}
                         addDist
                         0}))) ;; Attempt to set a super to a sub type
  (define p6prog7 (λ (r)
                    (r (list p6posn-class)
                       '(set {new posn 1 2} x {new posn 1 2})))) ;; Attempt to assign an obj to num
  (define p6prog9 (λ (r)
                    (r (list p6posn-class)
                       '(set 3 x {new posn 1 2})))) ;; Attempt to assign an obj to num
  
  (define p6prog10 (λ (r)
                     (r (list p6posn-pair-class p6posn-class p6posn3D-class p6posn-class)
                        '{set {new posn-pair
                                   {new posn 1 2}
                                   {new posn 3 4}} p1 {new posn3D 5 6 7}})))
  
  ;; Implementation
  
  ;; Tests
  
  (test (p6prog1 interp-t-prog)
        '0)
  (test (p6prog1 typecheck-prog)
        `num)
  
  (test (p6prog2 interp-t-prog)
        '3)
  (test (p6prog2 typecheck-prog)
        `num)
  
  (test/exn (p6prog3 interp-t-prog)
            "not found")
  (test/exn (p6prog3 typecheck-prog)
            "not found")
  
  (test (p6prog4 interp-t-prog)
        `21)
  (test (p6prog4 typecheck-prog)
        `num)
  
  (test/exn (p6prog5 interp-t-prog)
            "no type")
  (test/exn (p6prog5 typecheck-prog)
            "no type")
  
  (test/exn (p6prog6 interp-t-prog)
            "no type")
  (test/exn (p6prog6 typecheck-prog)
            "no type")
  
  (test/exn (p6prog7 interp-t-prog)
            "no type")
  (test/exn (p6prog7 typecheck-prog)
            "no type")
  
  (test/exn (p6prog8 interp-t-prog)
            "no type")
  (test/exn (p6prog8 typecheck-prog)
            "no type")
  
  (test/exn (p6prog9 interp-t-prog)
            "no type")
  (test/exn (p6prog9 typecheck-prog)
            "no type")
  
  (test (p6prog10 typecheck-prog)
        `posn)
  
  
  )

;; End 6. 

