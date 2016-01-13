#lang plai-typed
(print-only-errors true)

(define 3rd-power : (number -> number)
  (λ (n)
    (* n (* n n))))

(module+ test
  (test (3rd-power -3) -27)
  (test (3rd-power -1) -1)
  (test (3rd-power -2) -8)
  (test (3rd-power 1) 1)
  (test (3rd-power 0) 0)
  (test (3rd-power 2) 8)
  (test (3rd-power 3) 27)
  (test (3rd-power 4) 64)
  (test (3rd-power 5) 125)
  (test (3rd-power 6) 216)
  (test (3rd-power 7) 343)
  (test (3rd-power 8) 512)
  (test (3rd-power 9) 729)
  (test (3rd-power 10) 1000)
  (test (3rd-power 11) 1331)
  (test (3rd-power 12) 1728)
  (test (3rd-power 13) 2197)
  (test (3rd-power 14) 2744)
  (test (3rd-power 15) 3375)
  (test (3rd-power 16) 4096)
  (test (3rd-power 17) 4913))

(define 42nd-power : (number -> number)
  (λ (n)
    (let* [(third (3rd-power n))
           (sixth (* third third))
           (ninth (* sixth third))
           (twelfth (* ninth third))
           (twenty-first (* twelfth ninth))]
      (* twenty-first twenty-first))))

(module+ test
  (test (42nd-power -3) 109418989131512359209)
  (test (42nd-power -2) 4398046511104)
  (test (42nd-power -1) 1)
  (test (42nd-power 0) 0)
  (test (42nd-power 1) 1)
  (test (42nd-power 2) 4398046511104)
  (test (42nd-power 3) 109418989131512359209)
  (test (42nd-power 4) 19342813113834066795298816)
  (test (42nd-power 5) 227373675443232059478759765625)
  (test (42nd-power 6) 481229803398374426442198455156736)
  (test (42nd-power 7) 311973482284542371301330321821976049)
  (test (42nd-power 8) 85070591730234615865843651857942052864)
  (test (42nd-power 9) 11972515182562019788602740026717047105681)
  (test (42nd-power 10) 1000000000000000000000000000000000000000000)
  (test (42nd-power 11) 54763699237492901685126120802225273763666521)
  (test (42nd-power 12) 2116471057875484488839167999221661362284396544)
  (test (42nd-power 13) 61040881526285814362156628321386486455989674569)
  (test (42nd-power 14) 1372073885318497127491074758162987278899500548096)
  (test (42nd-power 15) 24878997722115027320114677422679960727691650390625)
  (test (42nd-power 16) 374144419156711147060143317175368453031918731001856)
  (test (42nd-power 17) 4773695331839566234818968439734627784374274207965089))

(define my-empty-num-list (rest (list 1)))

(define last : ((listof 'a) -> 'a)
  (λ (l)
    (first (reverse l))))

(module+ test
  (test (last (list 1 2 3)) 3)
  (test (last (list #\a #\b #\c)) #\c))

(define start : ((listof 'a) -> (listof 'a))
  (λ (l)
    (if (empty? l)
        l
        (reverse (rest (reverse l))))))
  
(module+ test
  (test (start (list 1 2 3)) (list 1 2))
  (test (start (list 1)) my-empty-num-list)
  (test (start my-empty-num-list) my-empty-num-list))   

(define plural : (string -> string)
  (λ (s)
    (if (equal? s "")
        "s"
        (list->string
         (let [(s (string->list s))]
           (cond
             [(equal? #\y (last s)) 
              ;=>
              (append (start s) (string->list "ies"))]
             [else (append s (string->list "s"))]))))))

(module+ test
  (test (plural "baby") "babies")
  (test (plural "fish") "fishs")
  (test (plural "") "s")
  (test (plural "ian") "ians")
  (test (plural "y") "ies"))

(define-type Light
    [bulb (watts : number)
          (technology : symbol)]
    [candle (inches : number)])

(define energy-usage : (Light -> number)
  (λ (l)
    (type-case Light l
      [bulb (w t) (/ (* 24 w) 1000)]
      [candle (i) 0.0])))

(module+ test
  (test (energy-usage (bulb 100.0 'halogen)) 2.4)
  (test (energy-usage (bulb -100.0 'fantasy)) -2.4)
  (test (energy-usage (bulb 0 'negolah)) 0)
  (test (energy-usage (candle 10)) 0))

(define use-for-one-hour : (Light -> Light)
  (λ (l)
    (type-case Light l
      [bulb (w t) l]
      [candle (i) (if (> (- i 1) 0)
                      (candle (- i 1))
                      (candle 0) ; Used up.
                      )])))

(module+ test
  (test (use-for-one-hour (bulb 0.0 'what)) (bulb 0.0 'what))
  (test (use-for-one-hour (bulb 20 'huh?)) (bulb 20 'huh?))
  (test (use-for-one-hour (candle 10)) (candle 9))
  (test (use-for-one-hour (candle 1/2)) (candle 0))
  (test (use-for-one-hour (candle 0)) (candle 0))
  (test (use-for-one-hour (candle 1.0)) (candle 0))
  (test (use-for-one-hour (candle 11/10)) (candle 1/10))
  (test (use-for-one-hour (candle -1/10)) (candle 0)) ; Used up a long time ago
  )