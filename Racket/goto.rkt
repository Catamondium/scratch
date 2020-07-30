#lang racket

(define-syntax label
  ; define a new continuation point
  (syntax-rules ()
    ((_ name)
     (begin
       (define name #f)
       (call-with-current-continuation (λ (c) (set! name c)))))))

(define (goto label) (label)) ; call into the continuation

(define (main)
  ; while (< i 3) (display i) (newline)
  (define i 0)
  (label start)
  (display i) (newline)
  (set! i (+ i 1))
  (if (< i 3) (goto start) #f)
  (display "done") (newline))

(main)