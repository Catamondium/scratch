(defun otherwise-clause-p (sym)
  (and
    (symbolp sym)
    (or
      (string= sym "OTHERWISE")
      (string= sym "T"))))

(defmacro mycase (val &rest stuff)
  (let*
    ;; Ease implementation w/ named parts
    ((elem (pop stuff))
     (oval (car `,elem))
     (oprog (cdr `,elem)))
    ;; if final, run it, else generate recursive check
    (if (otherwise-clause-p `,oval)
      `(progn ,@oprog)
      `(if (= ,val ,oval)
        ,@oprog
        (mycase `,,val ,@stuff)))))

(loop for n from 0 upto 4
  (format t "~D~&"
    (car 
      (case n
        (0 '("Zero"))
        (1 '("One"))
        (2 '("Two"))
        (t '("Other")))))


  (format t "~D~&"
    (car
        (mycase n
          (0 '("Zero"))
          (1 '("One"))
          (2 '("Two"))
          (t '("Other"))))))