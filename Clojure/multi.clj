; multimethod tests
(defmulti sum (fn [& more] (class (first more))))

(defmethod sum :default
  ;"Default addition multimethod"
  [& more]
  (println "DEFAULT")
  (apply clojure.core/+ more))

(defrecord Time [^Integer hrs ^Integer mins])

(defn abs [^Time t]
  (+ (:mins t) (* 60 (:hrs t))))

(defmethod sum Time
  ;"Time addition multimethod"
  [& more]
  (println "TIME")
  (apply ->Time (->
                 (transduce (map abs) sum more)
                 ((juxt quot rem) 60))))

(assert (= 15 (sum 5 5 5)))

(->>
 [(->Time 1 30) (->Time 0 30)]
 (apply sum) ; despatched to sum Time
 (= (->Time 2 0))
 (assert))