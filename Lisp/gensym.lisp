(defmacro with_gensyms ((&rest labels) &body body)
    `(let ,(loop for n in labels collect `(,n (gensym)))
        ,@body
     )
)

(defmacro test ()
    (with_gensyms (a b c)
        `(let* 
            ((,a 1)
             (,b 2)
             (,c 3))

            (format nil "(~D ~D ~D)" ,a ,b ,c)
        )
    )
)

(format t "gsyms: ~D~&" (macroexpand-1 '(test)))
(format t "evals: ~D~&" (test))