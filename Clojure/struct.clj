; basic POD
(defrecord Person [fname lname address])
(println (:fname (Person. "A" "B" "C")))

; Protocol
(defprotocol Printer
  (myprint [_])) ; 'print' nullary interface

(defrecord Myobj [x]
  Printer ; interface list
  (myprint [self] (format "Myobj(%s)" x))) ; method

(->>
 (Myobj. 555)
 (myprint)
 (println))