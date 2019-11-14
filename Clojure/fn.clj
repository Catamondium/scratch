(defn divmod
  "Equiv to Python's divmod, returns: [(quot n d) (rem n d)]"
  [n d] ; arity list, ~~CL Lambda list
  {:pre [(not= 0 d)]} ; 'prepost-map' pre/post conditions
    ;; BODY
  ((juxt quot rem) n d)) ; applies quot,rem in sequence to input, creating result vector

(defn arity
  "Return arity in use"
  ([] "nullary")
  ([x] "unary")
  ([x y] "binary")
  ; 'discard' readmacro
  #_(Won't accept [& args], cannot resolve
           alongside [x] [x y])
  ([x y & args]
   (-> ; pre-insert threading
    (count args)
    (+ 2)
    (str "-ary"))))

(println (divmod 100 50))
(doseq [x (range 5)]
  (println (apply arity (range x))))