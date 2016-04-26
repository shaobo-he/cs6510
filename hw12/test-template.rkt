#lang plai-typed
(require plai-typed/s-exp-match
         "class.rkt"
         "inherit.rkt"
         "typed-class.rkt"
         "inherit-parse.rkt"
         "typed-parse.rkt")

(module+ test
  (print-only-errors true))

(module+ test
  ;; 1. Fix this/arg
  
  ;; Relevant changes are in typed-class.rkt. An 'is-main' argument is added which determines
  ;; if argI or thisI are allowed in the expression. If true, these are disallowed, otherwise,
  ;; we're in a method and they're ok.
)
  
  ;; End 1. Fix arg/this
