#|
Function sources
https://stackoverflow.com/questions/48977728/usage-of-defsetf
Places have the forms:
  (setf (f x) y)
  (f x) -> y
  where (f x) behaves as a bound location
|#

(setq *db* (make-hash-table))
(setf (gethash 'a *db*) "First") ; uses place notation
(print (gethash 'a *db*)) ; -> "First" retrieved
(fresh-line)
;; Getter
(defun kth (lst k)
  (cond ((> 0 k) nil)
    ((equal 0 k) (car lst))
    ((< 0 k) (kth-elt (cdr lst) (- k 1)))))

;; Setter
(defun kth-upd (lst k new)
  (cond ((> 0 k) nil)
    ((equal 0 k) (setf  (car lst) new))
    ((< 0 k) (kth-upd (cdr lst) (- k 1) new))))

(defsetf kth kth-upd)

(setq *lst* '('a 'b 'c 'd))
(setf (kth *lst* 0) 20)
(format t "~D: ~D~&k = 0: ~D" '*lst* *lst* (kth *lst* 0))