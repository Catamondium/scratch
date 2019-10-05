; attempt to do arrow operator threading from clojure

(defun <>p (form)
 "Identify '<>' symbol"
 (and
  (symbolp form)        ; is a symbol
  (string= form "<>"))) ; and '<>'

(defmacro defarrow (name comment reducing-agent inserter)
 "Define arrow operator with (ARROW COMMENT REDUCER INSERTER)"
 `(defmacro ,name (initial-form &rest forms)
	 ,comment
	 (reduce
	  (,reducing-agent #',inserter)
	  forms
	  :initial-value initial-form)))

(defun simple-reducer (insert-fun)
 "Create reducing function from function inserter"
 (lambda (acc next)              ; return reducing functor
  (if (listp next)                ; if list
   (funcall insert-fun acc next)  ; call the inserter
   (list next acc))))             ; else make new list

(defun insert-first (arg surround)
 "Prepend ARG to function call SURROUND"
 (list*            ; make new list
  (car surround)   ; function name
  arg              ; -first parameter
  (cdr surround))) ; remaining parameters

(defun insert-last (arg surround)
 "Append param to function call form"
 (append surround (list arg))) ; append to params

(defun diamond-reducer (insert-fun)
 "Create reducing function by substituting '<>' in chaining"
 (simple-reducer (lambda (acc next)
				  (case (count-if #'<>p next)
				   (0 (funcall insert-fun acc next))  ; if none, use insert-fun
				   (1 (substitute-if acc #'<>p next)) ; if 1, substitute next into accumulator '<>'
				   (t (let ((r (gensym "R")))         ; if many, subsitute the once-evaluated form in each
					   `(let ((,r ,acc))
						   ,(substitute-if r #'<>p next))))))))

(defarrow ->
 "Chain forms my first param"
 simple-reducer
 insert-first)

(defarrow ->>
 "Chain forms by last param"
  simple-reducer
  insert-last) ; reduce by appending


(defarrow -<>
 "Chain forms together by '<>', otherwise chain by first param"
 diamond-reducer
 insert-first)

(defarrow -<>>
 "Chain forms together by '<>' symbols, chain by last param if unspecified"
  diamond-reducer
  insert-last)

; main

(format t "~S~%"
 (macroexpand-1 '(-<>
  5         ;      3  2     arg
  (+ 10 <>) ; (+ 2 (* (+ 10 5) 3))
  (* 3)     ; append to 1st param
  (+ 2 <>))))

 (format t "~S~%"
 (macroexpand-1 '(-<>
  5
  (+ <> <>)))) ; more complex print, duplicate eval


 (format t "5 + 5 = ~D~%" (-<> 5 (+ <> <>)))

