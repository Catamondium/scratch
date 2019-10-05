(defmacro with_gensyms ((&rest names) &body body) ; recieve name list & body in context
  `(let
    ,(loop for name in names collect `(,name (gensym))) ; let a gensym for each name in names
    ,@body))

(defmacro test ()
  (with_gensyms (a b c)
    `(let* 
       ((,a 1)
        (,b 2)
        (,c 3))
        
     (format nil "(~D ~D ~D)" ,a ,b ,c))))

(format t "gsyms: ~D~&" (macroexpand-1 '(test)))
(format t "evals: ~D~&" (test))
