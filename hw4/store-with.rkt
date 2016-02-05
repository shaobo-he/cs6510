#lang plai-typed
(require plai-typed/s-exp-match)

(define-type-alias Location number)

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol)
         (body : ExprC)
         (env : Env)]
  [boxV (l : Location)]
  [recordV (handle : ExprC)
           (env    : Env)
           (fs : (listof FieldVal))])

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
  [recordC 
   (handle : ExprC)
   (fs : (listof FieldExp))]
  [getC (name : symbol)
        (rec  : ExprC)]
  [setC (name : symbol)
        (rec  : ExprC)
        (val  : ExprC)]
  [errC (msg  : string)])

(define-type FieldExp
  [fieldC (name : symbol)
          (exp : ExprC)])

(define-type FieldVal
  [fieldV (name : symbol)
          (val : Value)])

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

(define (override-store [c : Storage] [sto : Store]) : Store
  (cond
    [(cons? sto) (if (eq? (cell-location c) 
                          (cell-location (first sto)))
                       (cons c (rest sto))
                       (cons (first sto)
                             (override-store c (rest sto))))]
    [(empty? sto) (list c)]))

(define-type Result
  [v*s (v : Value) (s : Store)])

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
    
    [(s-exp-match? '{record {SYMBOL ANY} ...} s)
     (parse `{record/handle {error "no such field"}
                            ,@(rest (s-exp->list s))})]
    
    [(s-exp-match? '{record/handle ANY {SYMBOL ANY} ...} s)
     (let ([ls (s-exp->list s)])
       (recordC
        (parse (second ls))
        (map (λ (p)
               (fieldC (s-exp->symbol (first (s-exp->list p)))
                       (parse (second (s-exp->list p)))))
             (rest (rest (s-exp->list s))))))]
    
    [(s-exp-match? '{get ANY SYMBOL} s)
     (let ([sl (s-exp->list s)])
       (getC (s-exp->symbol (third sl))
             (parse (second sl))))]
    
    [(s-exp-match? '{error STRING} s)
     (errC (s-exp->string (second (s-exp->list s))))]
    
    [(s-exp-match? '{set ANY SYMBOL ANY} s)
     (let ([sl (s-exp->list s)])
       (setC (s-exp->symbol (third sl))
             (parse (second sl))
             (parse (fourth sl))))]
    
    [(s-exp-match? '{set-box! ANY ANY} s)
     (setboxC (parse (second (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? '{begin ANY ANY} s)
     (beginC (parse (second (s-exp->list s)))
             (parse (third (s-exp->list s))))]
    [(s-exp-match? '{ANY ANY} s)
     (appC (parse (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
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

;; with form ----------------------------------------
(define-syntax-rule
  (with [(v-id sto-id) call]
    body)
  (type-case Result call
    [v*s (v-id sto-id) body]))
                                
;; interp ----------------------------------------
(define (interp [a : ExprC] [env : Env] [sto : Store]) : Result
  (type-case ExprC a
    [numC (n) (v*s (numV n) sto)]
    [idC (s) (v*s (lookup s env) sto)]
    [plusC (l r)
           (with [(v-l sto-l) (interp l env sto)]
             (with [(v-r sto-r) (interp r env sto-l)]
               (v*s (num+ v-l v-r) sto-r)))]
    [multC (l r)
           (with [(v-l sto-l) (interp l env sto)]
             (with [(v-r sto-r) (interp r env sto-l)]
               (v*s (num* v-l v-r) sto-r)))]
    [letC (n rhs body)
          (with [(v-rhs sto-rhs) (interp rhs env sto)]
            (interp body
                    (extend-env
                     (bind n v-rhs)
                     env)
                    sto-rhs))]
    [lamC (n body)
          (v*s (closV n body env) sto)]
    [appC (fun arg)
          (with [(v-f sto-f) (interp fun env sto)]
            (with [(v-a sto-a) (interp arg env sto-f)]
              (type-case Value v-f
                [closV (n body c-env)
                       (interp body
                               (extend-env
                                (bind n v-a)
                                c-env)
                               sto-a)]
                [else (error 'interp "not a function")])))]
    [boxC (a)
          (with [(v sto-v) (interp a env sto)]
            (let ([l (new-loc sto-v)])
              (v*s (boxV l) 
                   (override-store (cell l v) 
                                   sto-v))))]
    [unboxC (a)
            (with [(v sto-v) (interp a env sto)]
              (type-case Value v
                [boxV (l) (v*s (fetch l sto-v) 
                               sto-v)]
                [else (error 'interp "not a box")]))]
    [setboxC (bx val)
             (with [(v-b sto-b) (interp bx env sto)]
               (with [(v-v sto-v) (interp val env sto-b)]
                 (type-case Value v-b
                   [boxV (l)
                         (v*s v-v
                              (override-store (cell l v-v)
                                              sto-v))]
                   [else (error 'interp "not a box")])))]
    [beginC (l r)
            (with [(v-l sto-l) (interp l env sto)]
              (interp r env sto-l))]
    
    [recordC (h fs) (local ([define-values [fields new-store]
                            (process-fields fs env sto)])
                    (v*s (recordV h env fields) new-store))]
    
    [getC (name expr) (with ([rec-v rec-sto] (interp expr env sto))
                            (type-case Value rec-v
                              [recordV (h r-env fs) 
                                       (let ([b (get-record name fs)])
                                         (if (some? b)
                                             (v*s (fetch (boxV-l (some-v b)) rec-sto)
                                                  rec-sto)
                                             (interp h r-env rec-sto)))]
                              [else (error 'interp "not a record")]))]
    
    [setC (name rec val) (with ([val-v val-sto] (interp val env sto))
                               (with ([rec-r rec-sto] (interp rec env val-sto))
                                     (type-case Value rec-r
                                       [recordV (h r-env fs) 
                                                (let ([rec-cell (get-record name fs)])
                                                  (if (some? rec-cell)
                                                      (v*s val-v
                                                           (override-store 
                                                            (cell (boxV-l (some-v rec-cell)) val-v)
                                                            rec-sto))
                                                      (error 'interp "no such field")))]
                                       [else (error 'interp "not a record")])))]
    [errC (msg) (error 'interp msg)]))
                               

(define (process-fields [fs : (listof FieldExp)] [env : Env] [sto : Store]) : ((listof FieldVal) * Store)
  (cond
    [(empty? fs) (values empty sto)]
    [(cons? fs) 
     (local [(define-values [r-fields r-store]
              (process-fields (reverse (rest fs)) env sto))]
       (type-case FieldExp (first fs)
         [fieldC (name exp)
                 (with ([exp-v exp-sto] (interp exp env r-store))
                       (let ([c (cell (new-loc exp-sto) exp-v)])
                         (pair (cons (fieldV 
                                      name
                                      (boxV (cell-location c)))
                                     r-fields)
                               (override-store c exp-sto))))]))]))

(define (get-record [name : symbol] [fields : (listof FieldVal)]) : (optionof Value)
  (cond
    [(empty? fields) (none)]
    [(cons? fields) (type-case FieldVal (first fields)
                      [fieldV (n val)
                              (if (eq? name n)
                                  (some val)
                                  (get-record name (rest fields)))])]))

;; interp-expr
(define (interp-expr [e : ExprC]) : s-expression
  (with ([val sto] (interp e mt-env mt-store))
        (type-case Value val
          [numV (n) (number->s-exp n)]
          [closV (n b e) `function]
          [recordV (h r-env fs) `record]
          [boxV (v) `box])))

(module+ test
  (test/exn (interp-expr (parse '{set {record} x 1}))
            "no such field")
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
 
  (test/exn (interp-expr (parse '{error "yo"}))
            "yo")
  (test/exn (interp-expr (parse '{set 7 y 7}))
            "not a record")
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
  (test/exn (interp-expr (parse '{get 7 x}))
            "not a record")
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
  (test (interp-expr (parse '{box 8}))
        `box)
  (test (interp-expr (parse '{lambda {x} {x x}}))
        `function)
  (test (interp-expr (parse '{get {get {record {r {record {z 0}}}} r} z}))
        '0)
  (test (interp-expr (parse '{let {[x {box 0}]}
                               {let {[r {record {a {unbox x}}}]}
                                 {begin
                                   {set-box! x 1}
                                   {get r a}}}}))
        '0)
  
  (test (interp (parse '{let {[a {box 1}]}
                          {let {[b {box 2}]}
                            {let {[c {box 3}]}
                              {unbox b}}}}) mt-env mt-store)
        (v*s (numV 2)
             (override-store (cell 3 (numV 3))
                             (override-store (cell 2 (numV 2))
                                             (override-store (cell 1 (numV 1))
                                                             mt-env)))))
  (test/exn (interp (parse '{unbox 7}) mt-env mt-store)
        "not a box")
  (test/exn (interp (parse '{set-box! 7 8}) mt-env mt-store)
            "not a box")
  (test (interp (parse '{let {[b {box 1}]}
                          {begin
                            {set-box! b 2}
                            {unbox b}}})
                mt-env
                mt-store)
        (v*s (numV 2)
             (override-store (cell 1 (numV 2))
                             mt-store)))
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
  
;; store operations ----------------------------------------

(define (new-loc [sto : Store]) : Location
  (+ 1 (max-address sto)))

(define (max-address [sto : Store]) : Location
  (cond
   [(empty? sto) 0]
   [else (max (cell-location (first sto))
              (max-address (rest sto)))]))

(define (fetch [l : Location] [sto : Store]) : Value
  (cond
   [(empty? sto) (error 'interp "unallocated location")]
   [else (if (equal? l (cell-location (first sto)))
             (cell-val (first sto))
             (fetch l (rest sto)))]))

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

