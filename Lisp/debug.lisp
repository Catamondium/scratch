(defun inspect-call (f &rest args)
    "Prints function call site e.g (* 2 4) ==> 8 and forwards it's return values."
    (let ((ret (apply f args)))
        (format t "(~S ~{~S~^ ~}) ==> ~S~%" f args ret)
        ret))

(defmacro inspector-gadget (f)
 "Generates DEBUG-F for function F, wrapping it in INSPECT-CALL"
    (let ((observed-func (intern (concatenate   ; DEBUG-F
                                    'string
                                    (symbol-name :debug-)
                                    (write-to-string f)))))
        `(defun ,observed-func (&rest args)
            ,(format nil "instrumented ~A, wrapped in ~A.~&~A: ~A~&~A: ~A"
                f 'inspect-call f (documentation f 'function) 'inspect-call (documentation 'inspect-call 'function))
            (apply 'inspect-call ',f args))))

(format t "~S~%" (macroexpand-1 '(inspector-gadget +)))
(inspector-gadget +) ; DEBUG-+ returned to global scope
(format t "Forwarded return: ~D~%~%" (debug-+ 2 2 2))

(defun add (&rest args) "Aliases + function." (apply '+ args))
(inspector-gadget add)
(format t "Doc: ~S" (documentation 'debug-add 'function))
