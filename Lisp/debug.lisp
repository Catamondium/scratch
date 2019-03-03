(defun inspect-call (f &rest args)
    "Prints function call site and forwards it's return values"
    (let ((ret (apply f args)))
        (format t "(~S ~{~S~^ ~}) -> ~S~%" f args ret)
        ret))

(defmacro inspector-gadget (f)
 "Generates DEBUG-F for function F, wrapping it in INSPECT-CALL"
    (let ((observed-func (intern (concatenate   ; DEBUG-F
                                'string
                                (symbol-name :debug-)
                                (write-to-string f)))))
        `(defun ,observed-func (&rest args)     ; (defun DEBUG-F (&rest args)
            (apply #'inspect-call ',f args))))  ; 	(apply #'inspect-call 'F args))

(inspector-gadget +) ; DEBUG-+ returned to global scope
(format t "Forwarded return: ~D~%" (debug-+ 2 2 2))
(format t "~S~%" (documentation 'debug-+ 'function))
