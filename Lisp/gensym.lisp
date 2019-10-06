(defmacro with_gensyms ( (&optional (prefix "G")) (&rest names) &body body) ; recieve name list & body in context
  `(let
    ,(loop for name in names
      collect `(,name (gensym ,prefix))) ; let a gensym for each name in names
    ,@body))

(defmacro test ()
  (with_gensyms ("TEST") (a b c)
    `(let ; none * version lets in parallel
      ((,a 1)
       (,b 2)
       (,c 3))
        
      (format nil "(~D ~D ~D)" ,a ,b ,c))))

(format t "gsyms: ~D~&" (macroexpand-1 '(test)))
(format t "evals: ~D~&" (test))
