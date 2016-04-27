#lang plai-typed
(require plai-typed/s-exp-match
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


