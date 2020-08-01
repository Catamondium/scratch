#lang racket

(define-syntax-rule (swap x y)
  ; simple single-template macro
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))

(define-syntax rotate
  ; multi-rule template macro
  (syntax-rules ()
    [(rotate a b) (swap a b)]
    [(rotate a b c ...) (begin ; prog, c ... rest param
                          (swap a b)
                          (rotate b c ...))]))

(define-syntax (swap-c stx)
  ; full syntax-case impl
  ; provides error checking at eval + correct error expansion
;(swap-c a 1)
;swap-c: not an identifier in: 1
  (syntax-case stx ()
    [(swap-c x y)
     (if (and (identifier? #'x)
              (identifier? #'y))
         #'(let ([tmp x])
             (set! x y)
             (set! y tmp))
         (raise-syntax-error #f
                             "not an identifier"
                             stx
                             (if (identifier? #'x)
                                 #'y
                                 #'x)))]))