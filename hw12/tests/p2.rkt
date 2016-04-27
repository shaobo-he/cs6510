#lang plai-typed
(require plai-typed/s-exp-match
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
