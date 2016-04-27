#lang plai-typed
(require plai-typed/s-exp-match
         "../typed-parse.rkt")

#;(module+ test
    (print-only-errors true))

(module+ test
  ;; 5. cast form
  
  ;; Implementation details
  
  ;; Classes
  (define p5A '{class A extends object{}})
  (define p5AA '{class AA extends A{}})
  (define p5AAA '{class AAA extends AA{}})
  (define p5B '{class B extends AAA{}})
  (define p5BB '{class BB extends B{}})
  (define p5BA '{class BA extends B{}})
  (define p5C '{class C extends AAA{}})
  (define p5CC '{class CC extends C{}})
  (define p5CA '{class CA extends C{}})
  (define p5empty '{class empty extends object {}})
  (define p5classes (list p5A p5AA p5AAA p5B p5BB p5BA p5C p5CC p5CA p5empty))
  
  ;; Tests programs
  (define p5prog1 (λ (r)
                    (r p5classes
                       '{cast A {new AA}})))
  (define p5prog2 (λ (r)
                    (r p5classes
                       '{cast A {new BB}})))
  (define p5prog3 (λ (r)
                    (r p5classes
                       '{cast C {new BB}})))
  (define p5prog4 (λ (r)
                    (r p5classes
                       '{cast A 2})))
  (define p5prog5 (λ (r)
                    (r p5classes
                       '{cast AA {new CA}})))
  (define p5prog6 (λ (r)
                    (r p5classes
                       '{cast object {new BB}})))
  (define p5prog7 (λ (r)
                    (r p5classes
                       '{cast A {new A}})))
  (define p5prog8 (λ (r)
                    (r p5classes
                       '{cast CA {new AA}})))
  ;; Runtime tests
  (test (p5prog1 interp-prog)
        `object)
  (test (p5prog2 interp-prog)
        `object)
  (test/exn (p5prog3 interp-prog)
            "cast")
  (test/exn (p5prog4 interp-prog)
            "cast")
  (test (p5prog5 interp-prog)
        `object)
  (test (p5prog6 interp-prog)
        `object)
  (test (p5prog7 interp-prog)
        `object)
  (test/exn (p5prog8 interp-prog)
            "cast")
  ;; Typecheck tests
  (test (p5prog1 typecheck-prog)
        `A)
  (test (p5prog2 typecheck-prog)
        `A)
  (test/exn (p5prog3 typecheck-prog)
            "cast")
  (test/exn (p5prog4 typecheck-prog)
            "cast")
  (test (p5prog5 typecheck-prog)
        `AA)
  (test (p5prog6 typecheck-prog)
        `object)
  (test (p5prog7 typecheck-prog)
        `A)
  (test (p5prog8 typecheck-prog)
        `CA)
  )

;; End 5. 


