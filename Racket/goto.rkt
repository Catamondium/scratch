#lang racket

(define-syntax (label stx)
  (syntax-case stx ()
    [(_ name)
     (if (identifier? #'name)
         ; define a new continuation point
         #'(begin
             (define name #f)
             (call/cc (λ (c) (set! name c))))
         (raise-syntax-error #f "not an identifier" #'name))]))

(define (goto label) (label)) ; call into the continuation

(define (main)
  ; while (< i 3) (display i) (newline)
  (define i 0)
  (label start)
  (displayln i)
  (set! i (+ i 1))
  (if (< i 3) (goto start) #f)
  (displayln "done"))

(main)