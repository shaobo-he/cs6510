#lang plai-typed
(require plai-typed/s-exp-match
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


