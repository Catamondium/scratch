;;;; Replicating @debug/@monitor decorator from Python

(defun inspect-call (f &rest args)
  "Prints function call site e.g (* 2 4) ==> 8 and forwards it's return values"
  (let
    ((ret (apply f args)))
    (format t "(~S ~{~S~^ ~}) ==> ~S~%" f args ret)
    ret))

(defun genwrap_gendoc (wrapper wrapee)
  (concatenate 'string
    (format nil "~A wrapped in ~A~&" wrapee wrapper)
    (format nil "~A: ~A~&" wrapee (documentation wrapee 'function))
    (format nil "~A: ~A" wrapper (documentation wrapper 'function))))

(defmacro genwrap (wrapper wrapee)
  "Generates DEBUG-WRAPEE for function WRAPEE, wrapping it in function WRAPEE"
  (let
      ((observed-func 
          (intern
            (concatenate   ; DEBUG-F
              'string
              (write-to-string wrapper)
              (symbol-name :-)
              (write-to-string wrapee)))))
      `(defun ,observed-func (&rest args)
        ,(genwrap_gendoc wrapper wrapee)
        (apply ',wrapper ',wrapee args))))

(format t "~S~%" (macroexpand-1 '(genwrap inspect-call +)))
(genwrap inspect-call +) ; inspect-call-+ returned to global scope
(format t "Forwarded return: (~{~S~^ ~}) ==> ~D~%~%" '(inspect-call-+ 2 2 2) (inspect-call-+ 2 2 2))

(defun add (&rest args) "Aliases + function" (apply '+ args))
(genwrap inspect-call add)
(format t "Doc inspect-call-add:~%~S" (documentation 'inspect-call-add 'function))
