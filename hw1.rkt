#lang plai-typed
(print-only-errors true)
(define-type Tree
  [leaf (val : number)]
  [node (val : number)
        (left : Tree)
        (right : Tree)])

(define sum : (Tree -> number)
  (λ (t)
    (type-case Tree t
      [leaf (v) v]
      [node (v l r) (+ v (+ (sum l) (sum r)))])))

(module+ test
  (test (sum (node 5 (leaf 6) (leaf 7))) 18)
  (test (sum (leaf 3)) 3)
  (test (sum (node 1 (node 2 (leaf 3) (leaf 4)) ( node 5 (leaf 6) (leaf 7)))) 28))

(define neg : (number -> number)
  (λ (n)
    (* -1 n)))

(define negate : (Tree -> Tree)
  (λ (t)
    (type-case Tree t
      [leaf (v) (leaf (neg v))]
      [node (v l r) (node (neg v) (negate l) (negate r))])))

(module+ test
  (test (negate (node 5 (leaf 6) (leaf 7))) (node -5 (leaf -6) (leaf -7)))
  (test (negate (leaf 7)) (leaf -7))
  (test (negate (node 5 (leaf 3) (leaf -2))) (node -5 (leaf -3) (leaf 2))))

(define contains? : (Tree number -> boolean)
  (λ (t c)
    (type-case Tree t
      [leaf (v) (equal? v c)]
      [node (v l r) (or (equal? v c)
                        (contains? l c)
                        (contains? r c))])))
(module+ test
  (test (contains? (node 5 (leaf 7) (leaf 6)) 6) #t)
  (test (contains? (leaf 8) 8) #t)
  (test (contains? (node 5 (leaf 30) (node 0 (leaf 4) (leaf 3))) 4) #t)
  (test (contains? (node 5 (leaf 30) (node 0 (leaf 4) (leaf 3))) 7) #f))

(define big-leaves? : (Tree -> boolean)
  (λ (t)
    (big-leaves-real? t 0)))

(define big-leaves-real? : (Tree number -> boolean)
  (λ (t acc)
    (type-case Tree t
      [leaf (v) (> v acc)]
      [node (v l r) (let [(acc-new (+ acc v))]
                      (and (big-leaves-real? l acc-new)
                           (big-leaves-real? r acc-new)))])))

(module+ test
  (test (big-leaves? (node 5 (node 3 (leaf 8) (leaf 6)) (node 5 (leaf 20) (leaf 20))))
        #f)
  (test (big-leaves? (node 5 (node 3 (leaf 8) (leaf 10)) (node 5 (leaf 20) (leaf 20))))
        #f)
  (test (big-leaves? (node 5 (node 3 (leaf 10) (leaf 20)) (node 5 (leaf 20) (leaf 20))))
        #t)
  (test (big-leaves? (leaf 7)) #t)
  (test (big-leaves? (node 5 (node 3 (leaf 200) (leaf 200)) (node 5 (leaf 3) (leaf 20))))
        #f)
  (test (big-leaves? (node 5 (node 3 (leaf 200) (leaf 200)) (node 5 (leaf 20) (leaf -1))))
        #f)
  )

(define-type Record
  [sortr (sorted : boolean)
         (left   : number )
         (right  : number )])

(define sorted? : (Tree -> boolean)
  (λ (T)
    (type-case Tree T
      [leaf (v) true]
      [node (v l r) (type-case Record (sorted-real? T)
                      [sortr (sorted a b) sorted])])))

(define sorted-real? : (Tree -> Record)
  (λ (T)
    (type-case Tree T
      [leaf (v) (sortr #t v v)]
      [node (v l r) (let [(l-result 
                           (sorted-real? l))
                          (r-result
                           (sorted-real? r))]
                      (type-case Record l-result
                        [sortr (s-l ll lr)
                               (type-case Record r-result
                                 [sortr (s-r rl rr)
                                        (sortr (and s-l (>= v lr)
                                                    s-r (<= v rl))
                                               ll rr)])]))])))

(module+ test
  (test (sorted? (node 1 
                       (node 2 
                             (leaf 3) 
                             (leaf 4)) 
                       (node 5 
                             (leaf 6) 
                             (leaf 7)))) #f)
  (test (sorted? (leaf 1)) #t)
  (test (sorted? (node 2 
                       (leaf 1) 
                       (leaf 3))) #t)
  (test (sorted? (node 2 
                       (leaf 1) 
                       (node 3 
                             (leaf 3) 
                             (leaf 3)))) #t)
  (test (sorted? (node 4 
                       (node 2
                             (leaf -1) 
                             (node 3
                                   (leaf 3) 
                                   (leaf 3)))
                       (leaf 5))) #t)
  (test (sorted? (node 4
                       (node 3
                             (leaf 1)
                             (leaf 2)) ; err
                       (node 6
                             (leaf 5)
                             (leaf 7)))) false)
  (test (sorted? (node 4
                       (node 2
                             (leaf 1)
                             (leaf 3))
                       (node 3 ; err
                             (leaf 5)
                             (leaf 7)))) false))
