(defmacro mycase (val &rest stuff)
  "Reimplementation of CASE macro"
  (flet
    ((otherwise-clause-p (sym)
      "Test for OTHERWISE / T clause"
      (and
        (symbolp sym)
        (or
          (string= sym "T")
          (string= sym "OTHERWISE")))))
    ;; Ease implementation w/ named parts
    (destructuring-bind ((oval &rest oprog) &rest next) stuff
      (if (otherwise-clause-p `,oval)
        `(progn ,@oprog)
        `(if (eql ,val ,oval)
          ,@oprog
          (mycase `,,val ,@next))))))

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