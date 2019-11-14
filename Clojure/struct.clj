; basic POD
(defrecord Person [fname lname address])
(println (:fname (Person. "A" "B" "C"))) ; field access

; Protocol
(defprotocol Printer
  "printing protocol example"
  (myprint [_])) ; 'print' nullary interface

(defrecord Myobj [x]
  Printer ; interface list
  (myprint [this] (format "Myobj(%s)" x))) ; method

(defn multiprinter
  "Concatenating multiprinter, w/ ', ' separator"
  [& printers]
  ; ad-hoc / closured protocol implementation
  (reify Printer
    (myprint [_]
      (->>
       printers
       (map myprint)
       (interpose ", ")
       (reduce str)
       (format "multi[%s]")))))

(println "-------\nSimple test")
(->>
 (Myobj. 555)
 (myprint)
 (println))

(println "-------\nReify test")
(->>
 ["eub" 200]
 (mapv ->Myobj)
 (apply multiprinter)
 (myprint)
 (println))
