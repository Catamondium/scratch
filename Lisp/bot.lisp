(defmacro define-entity (name &body slots)
  (let ((*print-case* (readtable-case *readtable*))
		(data (gensym "DATA"))
		(value (gensym "VALUE")))
	`(progn
	   (defclass ,name (entity)
		 ,(loop for (slot . options) in slots
				collect `(,slot :initarg ,(intern (string slot) "KEYWORD")
								:initform ,(if (getf options :nullable)
											 NIL
											 `(error ,(format NIL "~a required for a ~a." slot name)))
								:accessor ,slot)))
	   (defmethod decode-entity ((,name ,name) ,data)
		 ,@(loop for (slot . options) in slots
				 for field = (getf options :field #1='#.(make-symbol "no value"))
				 collect `(let ((,value ,(cond ((null field) data)
											   ((eq field #1#) `(getj ,data ,(translate-key slot)))
											   (T `(getj ,data ,field)))))
							(setf (slot-value ,name ',slot)
								  (when ,value
									(funcall ,(or (getf options :translate-with)
												  '#'identity)
											 ,value)))))
		 ,name)
	   
	   (defun ,(intern (format NIL "~a-~a" 'decode name)) (,data)
		 (decode-entity ',name ,data)))))

(define-entity application
			   (name)
			   (website :nullable T)
			   )
