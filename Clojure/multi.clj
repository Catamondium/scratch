; multimethod tests
(defmulti add (fn [& more] (class (first more))))

(defmethod add :default
  ;"Default 'add' multimethod"
  [& more]
  (println "DEFAULT")
  (apply clojure.core/+ more))

(defrecord Time [^Integer hrs ^Integer mins])

(defmethod add Time 
  ;"Time addition multimethod"
  [& more]
  (println "TIME")
  (apply ->Time ((juxt quot rem)
                 (->> 
                  more
                  (map #(+ (:mins %1) (* 60 (:hrs %1))))
                  (reduce +)) 60)))

(->
 [5 5 5]
 (apply add) ; despatched to :default
 (prn)
 )

(->>
 [(->Time 1 30) (->Time 0 30)]
 (apply add) ; despatched to add Time
 (prn))