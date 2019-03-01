; attempt to do arrow operator threading from clojure

(defun <>p (form)
 "Identify '<>' symbol"
 (and
  (symbolp form)        ; is a symbol
  (string= form "<>"))) ; and '<>'

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
				   (1 (substitute-if acc #'<>p next)) ; if 1, substitute accumulator '<>' into next
				   (t (let ((r (gensym "R")))         ; if many, subsitute the once-evaluated form in each
					   `(let ((,r ,acc))
						   ,(substitute-if r #'<>p next))))))))

; actual operator defs
(defmacro -> (initial-form &rest forms)
 "Chain forms by first param"
 (reduce
  (simple-reducer #'insert-first) ; reduce by prepending
  forms
  :initial-value initial-form))

(defmacro ->> (initial-form &rest forms)
 "Chain forms by last param"
 (reduce
  (simple-reducer #'insert-last) ; reduce by appending
  forms
  :initial-value initial-form))

(defmacro -<> (initial-form &rest forms)
 "Chain forms together by '<>' symbols, chain by first param if unspecified"
 (reduce
  (diamond-reducer #'insert-first)
  forms
  :initial-value initial-form))

(defmacro -<>> (initial-form &rest forms)
 "Chain forms together by '<>' symbols, chain by last param if unspecified"
 (reduce
  (diamond-reducer #'insert-last)
  forms
  :initial-value initial-form))


(format t "~S~%"
 (macroexpand-1 '(-<>
 5         ;      3  2     arg
 (+ 10 <>) ; (- 2 (* (+ 10 5) 3))
 (* 3)
 (- 2 <>))))

(format t "~S~%"
 (insert-first 1 '("f" 2 3)))
(format t "~S~%"
 (insert-last 1 '("f" 2 3)))
