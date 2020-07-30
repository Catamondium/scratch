#lang racket

(define *k* #f)
(define prompt call-with-continuation-prompt)
(define (undelimited)
  ; -> (sqrt (+ 1 2 3 []))
  (sqrt
   ; no prompt, full stack is framed
   (+ 1 2 3
      (call/cc
       (λ (k) ; k -> cont fn
         (set! *k* k)
         0)))))
(define (delimited)
  ; (+ 1 2 3 [])
  (sqrt ; unreachable from prompt
   ; set to default frame delimiter
   (prompt
    (λ ()
      (+ 1 2 3
         (call/cc; equiv let/cc k
          (λ (k) ;
            (set! *k* k)
            0)))))))