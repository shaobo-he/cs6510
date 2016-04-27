#lang plai-typed
(require plai-typed/s-exp-match
         "../typed-parse.rkt")

#;(module+ test
  (print-only-errors true))

(module+ test
  ;; 4. generalized if types. Tests supersede part 3 tests.
  
  ;; Classes
  (define p4A '{class A extends object{}})
  (define p4AA '{class AA extends A{}})
  (define p4AAA '{class AAA extends AA{}})
  (define p4B '{class B extends AAA{}})
  (define p4BB '{class BB extends B{}})
  (define p4BA '{class BA extends B{}})
  (define p4C '{class C extends AAA{}})
  (define p4CC '{class CC extends C{}})
  (define p4CA '{class CA extends C{}})
  (define p4empty '{class empty extends object {}})
  (define p4classes (list p4A p4AA p4AAA p4B p4BB p4BA p4C p4CC p4CA p4empty))
  
  ;; Programs
  (define p4prog1 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new A}
                             {new AA}})))
  (define p4prog2 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new C}
                             {new B}})))
  (define p4prog3 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new CC}
                             {new BA}})))
  (define p4prog4 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new B}
                             {new BA}})))
  (define p4prog5 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new A}
                             {new CA}})))
  (define p4prog6 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new AA}
                             {new B}})))
  (define p4prog7 (λ (r)
                    (r p4classes
                       '{if0 0
                             {new AA}
                             {new empty}})))
  (define p4prog8 (λ (r)
                    (r p4classes
                       '{if0 0
                             2
                             {new B}})))
  (define p4prog9 (λ (r)
                    (r p4classes
                       '{if0 0
                             2
                             9})))
  
  ;; implementation: typed-class.rkt
  ;; if0-LUB if0-has-super if0-find-common-type
  
  ;; Tests
  (test (p4prog1 typecheck-prog)
        `A)
  (test (p4prog2 typecheck-prog)
        `AAA)
  (test (p4prog3 typecheck-prog)
        `AAA)
  (test (p4prog4 typecheck-prog)
        `B)
  (test (p4prog5 typecheck-prog)
        `A)
  (test (p4prog6 typecheck-prog)
        `AA)
  (test/exn (p4prog7 typecheck-prog)
            "compatible")
  (test/exn (p4prog8 typecheck-prog)
            "compatible")
  (test (p4prog9 typecheck-prog)
        `num)
  
  )

;; End 4. 

