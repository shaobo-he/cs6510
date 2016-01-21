#lang plai-typed
;; Mark S. Baranowski - u0485301

(print-only-errors true)
(define-type Tree
  [leaf (num-bound : number)]
  [node (num-bound : number)
        (left : Tree)
        (right : Tree)])

;; sum ------------------------------------------------------------------
(define sum : (Tree -> number)
  (λ (t)
    (type-case Tree t
      [leaf (v) v]
      [node (v l r) (+ v (+ (sum l) (sum r)))])))

(module+ test
  (test (sum (node 5 (leaf 6) (leaf 7))) 18)
  (test (sum (leaf 3)) 3)
  (test (sum (node 1 (node 2 (leaf 3) (leaf 4)) ( node 5 (leaf 6) (leaf 7)))) 28)
  (test (sum (node -inf.0 (leaf 1) (leaf 2))) -inf.0)
  (test (sum (node -inf.0 (leaf +inf.0) (leaf +inf.0))) +nan.0)
  (test (sum (node 0+7i (leaf 3) (leaf 4))) 7+7i))

;; negate ----------------------------------------------------------------
(define negate : (Tree -> Tree)
  (λ (t)
    (type-case Tree t
      [leaf (v) (leaf (* -1 v))]
      [node (v l r) (node (* -1 v) (negate l) (negate r))])))

(module+ test
  (test (negate (node 5 (leaf 6) (leaf 7))) (node -5 (leaf -6) (leaf -7)))
  (test (negate (leaf 7)) (leaf -7))
  (test (negate (leaf -inf.0)) (leaf +inf.0))
  (test (negate (node 5 (leaf 3) (leaf -2))) (node -5 (leaf -3) (leaf 2))))

;; contains? --------------------------------------------------------------
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

;; big-leaves? --------------------------------------------------------------
(define big-leaves? : (Tree -> boolean)
  (λ (t)
    (big-leaves-real? t 0)))

(define big-leaves-real? : (Tree number -> boolean)
  (λ (t acc)
    (type-case Tree t
      [leaf (v) (> v acc)]
      [node (v l r) (let [(curr-acc (+ acc v))]
                      (and (big-leaves-real? l curr-acc)
                           (big-leaves-real? r curr-acc)))])))

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
        #f))
;; in-range? --------------------------------------------------------------
(define in-range? : (number number number -> boolean)
  (λ (lb n ub)
    (and (<= lb n) (<= n ub))))

(module+ test
  (test (in-range? -inf.0 -inf.0 +inf.0) #t)
  (test (in-range? -inf.0 0 +inf.0) #t)
  (test (in-range? -1 0 0) #t)
  (test (in-range? -1 -2 5) #f)
  (test (in-range? 0 0 0) #t))

;; sorted? ----------------------------------------------------------------
(define sorted? : (Tree -> boolean)
  (λ (t)
    (sorted-real? -inf.0 t +inf.0))) ; All trees must be in this range

;; Based on the observation that an in-order traversal is sorted iff
;; the left and right subtrees are sorted and the right most leaf (max)
;; of the left subtree is <= the node value and the left most leaf (min)
;; of the right subtree is >= the node value. Leaf nodes are obviously 
;; sorted.
(define sorted-real? : (number Tree number -> boolean)
  (λ (lb t ub)
    (type-case Tree t
      [leaf (v) (in-range? lb v ub)]
      [node (v l r) (and (in-range? lb v ub)
                         (sorted-real? lb l v)
                         (sorted-real? v r ub))])))

(module+ test
  (test (sorted? (node 1 
                       (node 2 
                             (leaf 3) 
                             (leaf 4)) 
                       (node 5 
                             (leaf 6) 
                             (leaf 7)))) #f)
  (test (sorted? (node -inf.0
                       (leaf -inf.0)
                       (leaf +inf.0)))
        #t)
  (test (sorted? (node +inf.0
                       (leaf -inf.0)
                       (leaf -inf.0)))
        #f)
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