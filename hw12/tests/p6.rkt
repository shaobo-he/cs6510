#lang plai-typed
(require plai-typed/s-exp-match
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

