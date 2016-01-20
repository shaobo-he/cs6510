#lang plai-typed
(print-only-errors true)
(define-type Tree
  [leaf (num-bound : number)]
  [node (num-bound : number)
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
      [node (v l r) (let [(acc+v (+ acc v))]
                      (and (big-leaves-real? l acc+v)
                           (big-leaves-real? r acc+v)))])))

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

#;(define sorted? : (Tree -> boolean)
  (λ (T)
    (type-case Tree T
      [leaf (v) true]
      [node (v l r) (local [(define-values 
                              (t dont care)
                              (sorted-real? T))]
                      t)])))

;; Based on the observation that an in-order traversal is sorted iff
;; the left and right subtrees are sorted and the right most leaf (max)
;; of the left subtree is <= the node value and the left most leaf (min)
;; of the right subtree is >= the node value. Leaf nodes are obviously 
;; sorted.
#;(define sorted-real? : (Tree -> (boolean * number * number))
  (λ (t)
    (type-case Tree t
      [leaf (v) (values #t v v)]
      [node (v l r) (let [(l-result (sorted-real? l))
                          (r-result (sorted-real? r))]
                      (local  
                        [(define-values (l-sorted l-min l-max) l-result)
                         (define-values (r-sorted r-min r-max) r-result)]
                        (values (and l-sorted (>= v l-max)
                                     r-sorted (<= v r-min))
                                l-min r-max)))])))

(define-type Bound
  [-inf] ; <= to every number
  [+inf] ; >= to every number
  [bound (v : number)])
  
(define bound-<= : (Bound number -> boolean)
  (λ (b n)
    (type-case Bound b
      [-inf  ()  #t]
      [+inf  ()  #f]
      [bound (v) (<= v n)])))

(define bound->= : (Bound number -> boolean)
  (λ (b n)
    (type-case Bound b
      [-inf  ()  #f]
      [+inf  ()  #t]
      [bound (v) (>= v n)])))

(module+ test
  (test (bound-<= (-inf) -200) true)
  (test (bound->= (-inf) -200) false)
  (test (bound-<= (+inf) -200) false)
  (test (bound->= (+inf) -200) true)
  (test (bound-<= (bound 0) 0) true)
  (test (bound->= (bound 0) 0) true)
  (test (bound-<= (bound 0) 1) true)
  (test (bound->= (bound 0) 1) false)
  (test (bound-<= (bound 1) 0) false)
  (test (bound->= (bound 1) 0) true))

(define in-range? : (Bound number Bound -> boolean)
  (λ (lb n ub)
    (and (bound-<= lb n) (bound->= ub n))))

(define sorted? : (Tree -> boolean)
  (λ (t)
    (sorted-real? (-inf) t (+inf))))

(define sorted-real? : (Bound Tree Bound -> boolean)
  (λ (lb t ub)
    (type-case Tree t
      [leaf (v) (in-range? lb v ub)]
      [node (v l r) (and (in-range? lb v ub)
                         (sorted-real? lb  l (bound v))
                         (sorted-real? (bound v) r ub))])))

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
                             (leaf 7)))) false)
  (test (sorted? (node 14 
                       (node 5 
                             (node 2 
                                   (leaf 1) 
                                   (leaf 4)) 
                             (node 8 
                                   (leaf 7) 
                                   (node 11 
                                         (leaf 10) 
                                         (leaf 13)))) 
                       (leaf 16))) true)
  (test (sorted? (node 5 
                       (node 3 (node 1 (leaf 0) (leaf 9)) (leaf 4)) 
                       (node 2 (node 7 (leaf 6) (leaf 8)) (node 11 (leaf 10) (leaf 12))))) false)
  (test (sorted? (node 5 
                       (node 3 (node 1 (leaf 0) (leaf 10)) (leaf 4)) 
                       (node 9 (node 7 (leaf 6) (leaf 8)) (node 11 (leaf 2) (leaf 12))))) false)
  (test (sorted? (node 5 (leaf 5) (leaf 5))) true)
  (test (sorted? (node 0 
                       (node 3 (node 4 (leaf 7) (leaf 2)) (node 8 (leaf 1) (leaf 6))) 
                       (node 11 (node 9 (leaf 5) (leaf 10)) (node 13 (leaf 12) (leaf 14))))) false)
  (test (sorted? (node 9 
                       (node 7 (node 11 (node 1 (leaf 0) (leaf 10)) (node 5 (leaf 4) (leaf 6))) (leaf 8)) 
                       (node 17 
                             (node 20 (node 3 (leaf 2) (leaf 12)) (node 15 (leaf 14) (leaf 16))) 
                             (node 19 (leaf 18) (leaf 13))))) #f)
  (test (sorted? (node 5 
                       (leaf 1) 
                       (node 7 
                             (leaf 3) ; Violation
                             (leaf 8)))) #f))