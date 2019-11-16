; transducer testing

(defn tap
  "Observe a seq/transduction process, see Ruby"
  ([f coll] ; Seq format
   (f coll)
   coll
   [])
  ([f] ; Transducing format
   (fn [rf] ; The Transducer
     (fn 
       ([] (rf)) ; init uninteresting
       ([result] ; reduced step
        (f result)
        (rf result))
       ([result input] ; reducing step
        (f result input)
        (rf result input))))))

(defn monitor
  "Example 'Tap' monitoring fn"
  [phase]
  (fn
    ([result]
    (->>
     [phase result]
     (apply prn)))
    ([result input]
     (->>
      [phase result input]
      (apply prn)))))

(let [rf (comp
          (map inc)
          (tap (monitor "inc"))
          (filter even?)
          (tap (monitor "filter")))]
  (->>
   (range 10)
   (eduction rf)
   (reduce +)
   (println "FINAL")))