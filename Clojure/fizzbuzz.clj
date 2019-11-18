; Seq based mapped fizzbuzz impl

(def src {3 :fizz 5 :buzz 7 :baz})

(defn naturals
  ([] (naturals 0))
  ([n] (lazy-seq (cons n (naturals (inc n))))))

(defn fizzbuzz [x]
  (->>
   src
   (seq)
   (filter (fn [[k _]] (= 0 (mod x k))))
   (map (fn [[_ v]] v))
   (#(if (empty? %) x %))))

(def enumerated (partial map vector (naturals)))

(->>
 (naturals)
 (map fizzbuzz)
 (enumerated)
 (drop 1)
 (take (* 3 5 7))
 (clojure.pprint/pprint))