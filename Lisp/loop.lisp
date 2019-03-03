(defun make-multiples (n m)
 (loop
  for i from 1 upto n ; i = 0..n
  collect ; (*) i for each elem and collect
  	(loop for x in m collect (* x i))))

(format t "~S:~%~{(~{~2,'0D~^ ~})~&~}~&"
 '(make-multiples 5 '(1 2 3 4 5))
 (make-multiples 5 '(1 2 3 4 5)))
