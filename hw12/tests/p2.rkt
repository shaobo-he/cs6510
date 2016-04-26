#lang plai-typed
(require plai-typed/s-exp-match
         "../class.rkt"
         "../inherit.rkt"
         "../typed-class.rkt"
         "../inherit-parse.rkt"
         "../typed-parse.rkt")


(module+ test
  ;; 2. instanceof form
  (define posn-class
    '{class posn extends object
             {[x : num] [y : num]}
             {mdist : num -> num {+ {get this x} {get this y}}}
             {addDist : posn -> num {+ {send arg mdist 0}
                                      {send this mdist 0}}}})
  
  (define posn3D-class
    '{class posn3D extends posn
             {[z : num]}
             {mdist : num -> num {+ {get this z} 
                                  {super mdist arg}}}})
  
  (define posn4D-class
    '{class posn4D extends posn3D
             {[w : num]}
             {mdist : num -> num {+ {get this w} 
                       {super mdist arg}}}})
  
  (define empty-class '{class empty extends object {}})
  
  ;; Implementation details

  ;; Tests
  (test/exn (interp-t-prog empty
                           '{instanceof 2 object})
            "number")
  (test (interp-t-prog 
         (list empty-class posn4D-class posn-class posn3D-class)
         '{instanceof {new empty} posn3D})
        '1)
  (test (interp-t-prog 
         (list posn4D-class posn-class posn3D-class)
         '{instanceof {new posn4D 5 3 4 2} object})
        '0)
    (test (interp-t-prog 
         (list posn4D-class posn-class posn3D-class)
         '{instanceof {new posn4D 5 3 4 2} posn3D})
        '0)
  (test (interp-t-prog 
         (list posn4D-class posn-class posn3D-class)
         '{instanceof {new posn4D 5 3 4 2} posn})
        '0)
  (test (interp-t-prog 
         (list posn-class posn3D-class)
         '{instanceof {new posn 5 3} posn})
        '0)

  (test (interp-t-prog 
         (list posn-class posn3D-class)         
         '{instanceof {new posn 5 3} object})
        '0)
  (test (interp-t-prog 
         (list posn-class posn3D-class)
         '{instanceof {new posn 5 3} posn3D})
        '1)
  (test (interp-t-prog 
         (list posn-class posn3D-class)
         '{instanceof {new posn3D 5 3 1} posn})
        '0)  
  )

;; End 2. instanceof form
