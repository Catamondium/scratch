(defun otherwise-clause-p (sym)
  "Test for OTHERWISE / T clause"
  (and
    (symbolp sym)
    (or
      (string= sym "OTHERWISE")
      (string= sym "T"))))

(defmacro mycase (val &rest stuff)
  "Reimplementation of CASE macro"
  (let* ; * version lets sequentially
    ;; Ease implementation w/ named parts
    ((elem (pop stuff))
     (oval (car `,elem))
     (oprog (subseq `,elem 1)))
    ;; if final, run it, else generate recursive check
    (if (otherwise-clause-p `,oval)
      `(progn ,@oprog)
      `(if (eql ,val ,oval)
        ,@oprog
        (mycase `,,val ,@stuff)))))

(loop for n from 0 upto 4 do
  (format t "Builtin: ~D~&"
    (car 
      (case n
        (0 '("Zero"))
        (1 '("One"))
        (2 '("Two"))
        (t '("Other")))))


  (format t "Custom: ~1T~D~&"
    (car
      (mycase n
        (0 '("Zero"))
        (1 '("One"))
        (2 '("Two"))
        (t '("Other"))))))