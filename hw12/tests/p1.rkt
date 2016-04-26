#lang plai-typed
(require plai-typed/s-exp-match
         "../class.rkt"
         "../inherit.rkt"
         "../typed-class.rkt"
         "../inherit-parse.rkt"
         "../typed-parse.rkt")

(module+ test
  ;; 1. Fix this/arg
  
  ;; Relevant changes are in typed-class.rkt. An 'is-main' argument is added which determines
  ;; if argI or thisI are allowed in the expression. If true, these are disallowed, otherwise,
  ;; we're in a method and they're ok.
  (define classes1 (list
                   (classT
                    'posn
                    'object
                    (list (fieldT 'x (numT)) (fieldT 'y (numT)))
                    (list
                     (methodT 'mdist (numT) (numT) (plusI (getI (thisI) 'x) (getI (thisI) 'y)))
                     (methodT 'addDist (objT 'posn) (numT) (plusI (sendI (argI) 'mdist (numI 0))
                                                                  (sendI (thisI) 'mdist (numI 0))))))
                   (classT
                    'posn3D
                    'posn
                    (list (fieldT 'z (numT)))
                    (list (methodT 'mdist (numT) (numT) (plusI (getI (thisI) 'z)
                                                               (superI 'mdist (argI))))))))
  (test/exn (typecheck (thisI)
                       empty)
            "not allowed")
  
  (test/exn (typecheck (argI)
                       empty)
            "not allowed")
  
  (test/exn (typecheck (argI)
                       classes1)
            "not allowed")
  
  (test/exn (typecheck (thisI)
                       classes1)
            "not allowed")
  
  (test (typecheck (multI (numI 1) (numI 2))
                   classes1)
        (numT))
  
  (test/exn (typecheck (multI (thisI) (numI 2))
                       classes1)
            "not allowed")
  
  (test/exn (typecheck (multI (numI 1) (argI))
                       classes1)
            "not allowed"))
  
  ;; End 1. Fix arg/this
