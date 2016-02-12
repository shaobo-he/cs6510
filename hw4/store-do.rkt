#lang plai-typed
(module+ test
  (print-only-errors true))

(require plai-typed/s-exp-match)

(define-type-alias Location number)

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)]
  [boxV (l : Location)]
  [recV (handler : ExprC)
        (env : Env)
        (fields : (listof FieldVal))])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) 
         (r : ExprC)]
  [multC (l : ExprC)
         (r : ExprC)]
  [letC (n : symbol) 
        (rhs : ExprC)
        (body : ExprC)]
  [lamC (n : symbol)
        (body : ExprC)]
  [appC (fun : ExprC)
        (arg : ExprC)]
  [boxC (arg : ExprC)]
  [unboxC (arg : ExprC)]
  [setboxC (bx : ExprC)
           (val : ExprC)]
  [beginC (l : ExprC)
          (r : ExprC)]
  [recC   (handler : ExprC)
          (fields  : (listof FieldExp))]
  [getC   (rec : ExprC)
          (name : symbol)]
  [setC   (rec : ExprC)
          (name : symbol)
          (val : ExprC)]
  [errC   (msg : string)])

(define-type FieldExp
  [fieldExp (name : symbol)
            (exp  : ExprC)])

(define-type FieldVal
  [fieldVal (name : symbol)
            (l  : Location)])

(define-type Binding
  [bind (name : symbol)
        (val : Value)])

(define-type-alias Env (listof Binding))

(define mt-env empty)
(define extend-env cons)

(define-type Storage
  [cell (location : Location) 
        (val : Value)])

(define-type-alias Store (listof Storage))

(define mt-store empty)

(define (override-store [new : Storage] [sto : Store]) : Store
  (cond
    [(empty? sto) (list new)]
    [else
     (if (eq? (cell-location new)
              (cell-location (first sto)))
         (cons new (rest sto))
         (cons (first sto) (override-store new (rest sto))))]))

;; override-store tests ---------------------------------------
(module+ test
  (test (override-store (cell 1 (numV 2)) mt-store)
        (list (cell 1 (numV 2))))
  (test (override-store (cell 1 (numV 2))
                        (override-store (cell 3 (numV 4))
                                        mt-store))
        (list (cell 3 (numV 4))
              (cell 1 (numV 2))))
  (test (override-store (cell 1 (numV 2))
                        (override-store (cell 1 (numV 3))
                                        mt-store))
        (list (cell 1 (numV 2)))))


(define-type (Result 'a)
  [v*s (v : 'a) (s : Store)])



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
       (letC (s-exp->symbol (first bs))
             (parse (second bs))
             (parse (third (s-exp->list s)))))]
    [(s-exp-match? '{lambda {SYMBOL} ANY} s)
     (lamC (s-exp->symbol (first (s-exp->list 
                                  (second (s-exp->list s)))))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? '{box ANY} s)
     (boxC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{unbox ANY} s)
     (unboxC (parse (second (s-exp->list s))))]
    [(s-exp-match? '{set-box! ANY ANY} s)
     (setboxC (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    
    [(s-exp-match? '{record {SYMBOL ANY} ...} s)
     (parse `{record/handle {error "no such field"} ,@(rest (s-exp->list s))})]
    
    [(s-exp-match? '{record/handle ANY {SYMBOL ANY} ...} s)
     (let ([ls (s-exp->list s)])
       (let ([fields (map (λ (f)
                    (fieldExp (s-exp->symbol (first (s-exp->list f)))
                              (parse (second (s-exp->list f)))))
                  (rest (rest ls)))])
         (if (check-rec-names fields)
             (recC (parse (second ls))
                   fields)
             (error 'parse "duplicate field names"))))]
                   
    
    [(s-exp-match? '{get ANY SYMBOL} s)
     (let ([ls (s-exp->list s)])
       (getC (parse (second ls))
             (s-exp->symbol (third ls))))]
    
    [(s-exp-match? '{set ANY SYMBOL ANY} s)
     (let ([ls (s-exp->list s)])
       (setC (parse (second ls))
             (s-exp->symbol (third ls))
             (parse (fourth ls))))]
    
    [(s-exp-match? '{error STRING} s)
     (errC (s-exp->string (second (s-exp->list s))))]
    
    [(s-exp-match? '{begin ANY ANY} s)
     (beginC (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    [(s-exp-match? '{ANY ANY} s)
     (appC (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(module+ test
  (test/exn (parse '(record {x 1} {x 1}))
            "duplicate field names")
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
  (test (parse '{box 0})
        (boxC (numC 0)))
  (test (parse '{unbox b})
        (unboxC (idC 'b)))
  (test (parse '{set-box! b 0})
        (setboxC (idC 'b) (numC 0)))
  (test (parse '{begin 1 2})
        (beginC (numC 1) (numC 2)))
  (test/exn (parse '{{+ 1 2}})
            "invalid input"))

;; do ----------------------------------------
(define-syntax do
  (syntax-rules (<-)
    [(do () last)
     last]
    [(do ([v-rhs <- rhs] clause ...) last)
     (lambda (sto)
       (type-case (Result 'a) (rhs sto)
         [v*s (v-rhs sto-rhs)
              ((do (clause ...) last)
               sto-rhs)]))]))

;; interp* ----------------------------------------
(define interp* : (ExprC Env -> (Store -> (Result Value)))
  (lambda (a env)
    (type-case ExprC a
      [numC (n) (return* (numV n))]
      [idC (s) (return* (lookup s env))]
      [plusC (l r)
             (do ([v-l <- (interp* l env)]
                  [v-r <- (interp* r env)])
               (return* (num+ v-l v-r)))]
      [multC (l r)
             (do ([v-l <- (interp* l env)]
                  [v-r <- (interp* r env)])
               (return* (num* v-l v-r)))]
      [letC (n rhs body)
            (do ([v-rhs <- (interp* rhs env)])
              (interp* body
                       (extend-env
                        (bind n v-rhs)
                        env)))]
      [lamC (n body)
            (return* (closV n body env))]
      
      [appC (fun arg) 
            (do ([v-f <- (interp* fun env)]
                 [v-a <- (interp* arg env)])
              (type-case Value v-f
                [closV (n body c-env)
                       (interp* body
                                (extend-env
                                 (bind n v-a)
                                 c-env))]
                [else (error 'interp 
                             "not a function")]))]
      [boxC (a)
            (do ([v <- (interp* a env)]
                 [l <- (new-loc*)]
                 [ign <- (override-store* (cell l v))])
              (return* (boxV l)))]
      
      [unboxC (a)
              (do ([bx <- (interp* a env)]
                   [val <- (unbox* bx)])
                (return* val))]
      
      [setboxC (bx val)
               (do ([v-bx <- (interp* bx env)]
                    [v-v <- (interp* val env)]
                    [l <- (get-loc* v-bx)]
                    [ignored <- (override-store*
                                     (cell l v-v))])
                     (return* v-v))]
      
      
      [recC (h fes) (do ([fvs <- (process-fields* fes env empty)])
                     (return* (recV h env fvs)))]
      
      [getC (recx name) (do ([v-rec <- (interp* recx env)])
                          (type-case Value v-rec
                            [recV (h r-env fs)
                                  (let ([l (get-field name v-rec)])
                                    (type-case (optionof Location) l
                                      [some (loc) (fetch* loc)]
                                      [none () (interp* h r-env)]))]
                            [else (error 'interp "not a record")]))]
      
      [setC (recx name valx) (do ([v-rec <- (interp* recx env)]
                                  [v-val <- (interp* valx env)]
                                  [l <- (get-field* name v-rec)])
                               (type-case (optionof Location) l
                                 [some (loc) (do ([ign <- (override-store*
                                                           (cell loc v-val))])
                                               (return* v-val))]
                                 [none () (error 'interp "no such field")]))]
      
      [errC (msg) (error 'interp msg)]
      
      [beginC (l r)
              (do ([v <- (interp* l env)])
                (interp* r env))])))

(define interp : (ExprC Env Store -> (Result Value))
  (lambda (a env sto)
    ((interp* a env) sto)))

(define interp-expr : (ExprC -> s-expression)
  (λ (e)
    (let ([result (interp e mt-env mt-store)])
      (type-case (Result Value) result
        [v*s (v store) 
             (type-case Value v
               [numV (n) (number->s-exp n)]
               [closV (n b e) `function]
               [boxV (l) `box]
               [recV (h e f) `record])]))))

(module+ test
  ;; Error coverage
  (test/exn (interp-expr (parse '{get 7 x}))
            "not a record")
  (test/exn (interp-expr (parse '{set 7 x 7}))
            "not a record")
  (test/exn (interp-expr (parse '{set {record} x 7}))
            "no such field")
  
  ;; interp-expr coverage
  (test (interp-expr (parse '{lambda {x} {x x}}))
        `function)
  (test (interp-expr (parse '{box 2}))
        `box)
  
  ;; override-store is not functional
  (test (interp (parse '{let {[b {box 1}]}
                          {begin
                            {set-box! b 2}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 2)
             (override-store (cell 1 (numV 2))
                             mt-store)))
  
  ;; record is evaluated at creation
  (test (v*s-v (interp (parse '{let {[x {box 0}]}
                                 {let {[r {record {a {unbox x}}}]}
                                   {begin
                                     {set-box! x 1}
                                     {get r a}}}}) mt-env mt-store))
               (numV 0))
  
  ;; records with store/interp-expr
  (test (interp-expr (parse '{+ 1 4}))
        '5)
  
  (test (interp-expr (parse '{record {a 10} {b {+ 1 2}}}))
        `record)
  
  (test (interp-expr (parse '{get {record {a 10} {b {+ 1 0}}} b}))
        '1)
  
  (test/exn (interp-expr (parse '{get {record {a 10}} b}))
            "no such field")
  
  (test (interp-expr (parse '{get {record {r {record {z 0}}}} r}))
        `record)
  
  (test (interp-expr (parse '{get {get {record {r {record {z 0}}}} r} z}))
        '0)
  
  ;; mutating records
  (test (interp-expr (parse '{let {[b {box 0}]}
                               {let {[r {record {x {begin
                                                     {set-box! b {+ {unbox b} 1}}
                                                     {unbox b}}}
                                                {y {begin
                                                     {set-box! b {+ {unbox b} 1}}
                                                     {unbox b}}}
                                                {z {begin
                                                     {set-box! b {+ {unbox b} 1}}
                                                     {unbox b}}}}]}
                                 {unbox b}}}))
        `3)
  (test (interp-expr (parse '{let {[r {record {x 1}}]}
                               {get r x}}))
        '1)

  (test (interp-expr (parse '{let {[r {record {x 1}}]}
                               {begin
                                 {set r x 5}
                                 {get r x}}}))
        '5)

  (test (interp-expr (parse '{let {[r {record {x 1}}]}
                               {let {[get-r {lambda {x} r}]}
                                 {begin
                                  {set {get-r 0} x 6}
                                  {get r x}}}}))
        '6)

  (test (interp-expr (parse '{let {[g {lambda {r} {get r a}}]}
                               {let {[s {lambda {r} {lambda {v} {set r b v}}}]}
                                 {let {[r1 {record {a 0} {b 2}}]}
                                   {let {[r2 {record {a 3} {b 4}}]}
                                     {+ {get r1 b}
                                        {begin
                                          {{s r1} {g r2}}
                                          {+ {begin
                                               {{s r2} {g r1}}
                                               {get r1 b}}
                                             {get r2 b}}}}}}}}))
        '5)
  (test (interp-expr (parse '{let {[r {record {x 5}}]}
                               {set r x 2}}))
        `2)
  (test (interp-expr (parse '{let {[r {record {x 5}}]}
                               {begin
                                 {set r x 2}
                                 {get r x}}}))
        `2)
  (test (interp-expr (parse '{let {[x 7]}
                               {let {[r {record/handle x {y 9}}]}
                                 {let {[x 8]}
                                   {get r x}}}}))
        `7)
  
  ;; Records with no such field handlers
  (test (interp-expr (parse '{let {[r {record/handle 5 {x 1}}]}
                               {get r x}}))
        '1)
  
  (test (interp-expr (parse '{let {[r {record/handle 5 {x 1}}]}
                               {get r y}}))
        '5)
  
  (test/exn (interp-expr (parse '{let {[r {record/handle {error "ouch"} {x 1}}]}
                                   {get r y}}))
            "ouch")
  
  (test (interp-expr (parse '{let {[r {record/handle {error "ouch"} {x 1}}]}
                               {get r x}}))
        '1)
  
  ;; Coverage for original solution
  (test/exn (interp (parse '{error "this is an error"}) mt-env mt-store)
            "this is an error")
  (test/exn (interp (parse '{unbox 7}) mt-env mt-store)
            "not a box")
  (test/exn (interp (parse '{set-box! 7 7}) mt-env mt-store)
            "not a box")
  
  ;; Original tests
  (test (interp (parse '2) mt-env mt-store)
        (v*s (numV 2) 
             mt-store))
  (test/exn (interp (parse `x) mt-env mt-store)
            "free variable")
  (test (interp (parse `x) 
                (extend-env (bind 'x (numV 9)) mt-env)
                mt-store)
        (v*s (numV 9)
             mt-store))
  (test (interp (parse '{+ 2 1}) mt-env mt-store)
        (v*s (numV 3)
             mt-store))
  (test (interp (parse '{* 2 1}) mt-env mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse '{+ {* 2 3} {+ 5 8}})
                mt-env
                mt-store)
        (v*s (numV 19)
             mt-store))
  (test (interp (parse '{lambda {x} {+ x x}})
                mt-env
                mt-store)
        (v*s (closV 'x (plusC (idC 'x) (idC 'x)) mt-env)
             mt-store))
  (test (interp (parse '{let {[x 5]}
                          {+ x x}})
                mt-env
                mt-store)
        (v*s (numV 10)
             mt-store))
  (test (interp (parse '{let {[x 5]}
                          {let {[x {+ 1 x}]}
                            {+ x x}}})
                mt-env
                mt-store)
        (v*s (numV 12)
             mt-store))
  (test (interp (parse '{let {[x 5]}
                          {let {[y 6]}
                            x}})
                mt-env
                mt-store)
        (v*s (numV 5)
             mt-store))
  (test (interp (parse '{{lambda {x} {+ x x}} 8})
                mt-env
                mt-store)
        (v*s (numV 16)
             mt-store))
  (test (interp (parse '{box 5})
                mt-env
                mt-store)
        (v*s (boxV 1)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse '{unbox {box 5}})
                mt-env
                mt-store)
        (v*s (numV 5)
             (override-store (cell 1 (numV 5))
                             mt-store)))
  (test (interp (parse '{set-box! {box 5} 6})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             (override-store (cell 1 (numV 5))
                                             mt-store))))
  (test (interp (parse '{begin 1 2})
                mt-env
                mt-store)
        (v*s (numV 2)
             mt-store))
  (test (interp (parse '{let {[b (box 5)]}
                          {begin
                            {set-box! b 6}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 6)
             (override-store (cell 1 (numV 6))
                             (override-store (cell 1 (numV 5))
                                             mt-store))))
  
  (test/exn (interp (parse '{1 2}) mt-env mt-store)
            "not a function")
  (test/exn (interp (parse '{+ 1 {lambda {x} x}}) mt-env mt-store)
            "not a number")
  (test/exn (interp (parse '{let {[bad {lambda {x} {+ x y}}]}
                              {let {[y 5]}
                                {bad 2}}})
                    mt-env
                    mt-store)
            "free variable"))

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

;; box operations ------------------------------------------
(define unbox* 
  (lambda (bx)
    (type-case Value bx
      [boxV (l)
            (fetch* l)]
      [else (error 'unbox "not a box")])))

(define get-loc*
  (λ (v)
    (λ (sto)
      (v*s (get-loc v) sto))))

(define (get-loc [box : Value]) : Location
  (type-case Value box
    [boxV (l) l]
    [else (error 'get-loc "not a box")]))

;; record operations ---------------------------------------
(define (check-rec-names [fs : (listof FieldExp)]) : boolean
  (cond
    [(empty? fs) true]
    [(cons? fs) (let ([name (fieldExp-name (first fs))])
                  (and (not (member name (map (λ (f)
                                          (fieldExp-name f))
                                        (rest fs))))
                       (check-rec-names (rest fs))))]))

(define get-field*
  (λ (n r)
    (λ (sto)
      (v*s (get-field n r) sto))))

(define (get-field [name : symbol] [rec : Value]) : (optionof Location)
  (type-case Value rec
    [recV (h e fs) (get-field-r name fs)]
    [else (error 'get-field "not a record")]))

(define (get-field-r [name : symbol] [fields : (listof FieldVal)]) : (optionof Location)
  (cond
    [(empty? fields) (none)]
    [(cons? fields) (type-case FieldVal (first fields)
                      [fieldVal (n v)
                                (if (eq? n name)
                                    (some v)
                                    (get-field-r name (rest fields)))])]))

(define (process-fields* [fes : (listof FieldExp)] 
                         [env : Env]
                         [fvs : (listof FieldVal)]) : (Store -> (Result (listof FieldVal)))
  (cond
    [(empty? fes) (return* fvs)]
    [(cons? fes) 
     (type-case FieldExp (first fes)
       [fieldExp
        (name exp)
        (do ([fv <- (interp* exp env)]
             [l <- (new-loc*)]
             [ign <- (override-store* (cell l fv))])
          (process-fields* (rest fes)
                           env 
                           (cons (fieldVal name l)
                                 fvs)))])]))

;; store operations ----------------------------------------
    
(define (return* v)
  (lambda (sto)
    (v*s v sto)))

(define override-store*
  (lambda (c)
    (lambda (sto)
      (v*s (void) (override-store c sto)))))

(define (new-loc [sto : Store]) : Location
  (+ 1 (max-address sto)))

(define (max-address [sto : Store]) : Location
  (cond
    [(empty? sto) 0]
    [else (max (cell-location (first sto))
               (max-address (rest sto)))]))

(define new-loc*
  (lambda ()
    (lambda (sto)
      (v*s (new-loc sto) sto))))

(define (fetch [l : Location] [sto : Store])
  (cond
    [(empty? sto) (error 'interp "unallocated location")]
    [else (if (equal? l (cell-location (first sto)))
              (cell-val (first sto))
              (fetch l (rest sto)))]))

(define fetch*
  (lambda (l)
    (lambda (sto)
      (v*s (fetch l sto) sto))))

(module+ test
  (test (max-address mt-store)
        0)
  (test (max-address (override-store (cell 2 (numV 9))
                                     mt-store))
        2)
  
  (test (fetch 2 (override-store (cell 2 (numV 9))
                                 mt-store))
        (numV 9))
  (test (fetch 2 (override-store (cell 2 (numV 10))
                                 (override-store (cell 2 (numV 9))
                                                 mt-store)))
        (numV 10))
  (test (fetch 3 (override-store (cell 2 (numV 10))
                                 (override-store (cell 3 (numV 9))
                                                 mt-store)))
        (numV 9))
  (test/exn (fetch 2 mt-store)
            "unallocated location"))

