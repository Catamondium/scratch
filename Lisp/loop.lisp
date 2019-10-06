(defun make-multiples (n m)
  (loop
    for i from 0 upto n ; i = 0..n
    collect             ; (*) i for each elem and collect
      (loop for x in m collect (* x i))))

(setq i 5)
(let
  ((fmt
    (concatenate 'string
      "~S:~%~{ (~{~"
      (write-to-string (+ 1 (floor (log (* i i) 10))))
      ",'0D~^ ~})~&~}~&")))

    (format t fmt
      `(make-multiples ,i ,(loop for x from 0 upto i collect x))
      (make-multiples i  (loop for x from 0 upto i collect x))))
