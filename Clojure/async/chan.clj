(require '[clojure.core.async :as async])

(def inxf (map (partial + 5)))

; Sliding buffer drops oldest elements w/ saturated
(def ch (async/chan (async/sliding-buffer 3) inxf))

(async/go
  (doseq [x (range 6)] ; should only recv last 3
    (async/>! ch x))
  (async/close! ch))

(loop []
  (let [y (async/<!! ch)]
    (if (nil? y)
      nil
      (do
        (prn "OUT" y)
        (recur)))))