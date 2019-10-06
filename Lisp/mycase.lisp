(defun tp (sym)
  (and
    (symbolp sym)
    (string= sym "T")))

(defmacro mycase (val &rest stuff)
  (let*
    ((elem (pop stuff))
     (oval (car `,elem))
     (oprog (cdr `,elem)))
    (if (not (null `oval))
      `,oprog             ; TODO ((print Other)) -> (print Other)
      `(if (= ,val ,oval)
        ,@oprog
        (mycase `,,val ,@stuff)))))

(setq n 3)
(format t "~D~&"
  (car 
    (case n
      (0 '("Zero"))
      (1 '("One"))
      (2 '("Two"))
      (t '("Other")))))


(format t "~D~&"
  (macroexpand
    '(mycase n
      ;(0 (print "Zero"))
      ;(1 (print "One"))
      ;(2 (print "Two"))
      (t (print "Other")))))