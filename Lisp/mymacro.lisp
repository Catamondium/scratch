(defmacro ^ (x y &body body)
  "Print (expt x y) and execute body form"
  `(progn
     (format t "~D ~%" (expt ,x ,y))
     ,@body))

(defun escaping-reader (stream sub-char numarg)
  "Return escaped version of stream until <doublequote-sharp> is encountered"
 (declare (ignore sub-char numarg))
  (let (chars)
    (do
      ((prev (read-char stream) curr)
       (curr (read-char stream) (read-char stream)))
      ((and (char= prev #\") (char= curr #\#)))
      (push prev chars))
    (coerce (nreverse chars) 'string)))

(set-dispatch-macro-character #\# #\" #'escaping-reader)

(defun <=> (a b)
  "3-way binary comparison:
  (< a b) returns -1
  (= a b) returns 0
  (> a b) returns 1"
  (typecase a
    (number
      (cond
        ((= a b) (return-from <=> 0))
        ((< a b) (return-from <=> -1))
        ((> a b) (return-from <=> 1))
        (t (return-from <=> nil))))
    (string
      (cond
        ((string= a b) (return-from <=> 0))
        ((string< a b) (return-from <=> -1))
        ((string> a b) (return-from <=> 1))
        (t (return-from <=> nil))))))

(setf (get '<=> 'cckey) "Mykey") ; apply 'cckey' property
(format t "~S ~%"(get '<=> 'cckey)) ; display 'cckey' property

; anaphoric macros
(defmacro awhile (expr &body body)
  "Anaphoric while loop, binding expr result to 'it'"
  `(do
    ((it ,expr ,expr))
    ((not it))
    ,@body))

(defmacro alamda (params &body body)
  "Anaphoric lamda, binding lamda to 'self'"
  `(labels ((self ,params ,@body))
    #'self))

(defmacro aif (test Tclause &optional Fclause)
  "Anaphoric if, binding truth to bol. :then to process return bol"
  `(let ((bol ,test))
     (if bol ,Tclause ,Fclause)))

(^ 10 2 (format t "~D ~%" 20))

(format t "WHILE x < 20, print it, x++~%")
(let ((x 0))
  (awhile (< x 20)
    (format t "  ~02D ~S~%" x it)
    (incf x)))

(funcall (alamda (x)
  (format t "ALAMDA: R: ~D~%~S~%"
    (+ 2 x) #'self))
    20)

(aif (atom pi)
     (format t "atomic pi? ~S~%" bol))

(loop for x from -2 to 2
      do (format t "<=>(~02D 0) --> ~D~%" x (<=> x 0)))

(format t #"<=>("a" "b") --> ~D~%"# (<=> "a" "b"))
(format t #"<=>("b" "b") --> ~D~%"# (<=> "b" "b"))
(format t #"<=>("c" "b") --> ~D~%"# (<=> "c" "b"))
