(defmacro unit-of-time (value unit) ; example product
  `(*
    ,value
    ,(case unit
      ((s) 1)
      ((m) 60)
      ((h) 3600)
      ((d) 86400)
      ((ms) 1/1000)
      ((us) 1/1000000)))) ; microseconds

; http://www.gigamonkeys.com/book/macros-defining-your-own.html
(defun primep (number)
  (when (> number 1)
    (loop
      for fac from 2 to (isqrt number)
      never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop
    for n from number
      when (primep n)
      return n))

; non-macro defined macro
(defmacro do-primes-old ((var start end) &body body)
  (let
    ((ending-value-name (gensym)))
    `(do
      ((,var (next-prime ,start)
       (next-prime (1+ ,var)))
       (,ending-value-name ,end))

      ((> ,var ,ending-value-name))

      ,@body))) ; ,@x evaluates to copy spliced list from x

; macro defined version
(defmacro with-gensyms_C ((&rest names) &body body) ; helper
  `(let
    ,(loop for n in names collect `(,n (gensym)))
    ,@body))

(defmacro do-primes ((var start end) &body body)
  (with-gensyms_C (ending-value-name)
    `(do
      ((,var (next-prime ,start)
       (next-prime (1+ ,var)))
       (,ending-value-name ,end))

      ((> ,var ,ending-value-name))

      ,@body)))
