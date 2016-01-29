(ns clojure-iptools.ip-lookup)

(defn ip-cmp
  [ip [[start end] _]]
  (cond (< ip start) :lt
        (> ip end) :gt
        :else :in))

(defn binary-search
  [low high ip kvs]
  (if (< high low)
    nil
    (let [pivot-index (+ low (quot (- high low) 2))
          pivot (nth kvs pivot-index)]
      (case (ip-cmp ip pivot)
            :gt (recur (+ pivot-index 1) high ip kvs)
            :lt (recur low (- pivot-index 1) ip kvs) 
            :in pivot
            ))))

(defn lookup-ip
  [key-vals ip]
  (let [[_ payload] (binary-search 0 (- (count key-vals) 1) ip key-vals)]
    payload))
;  (let [[_ payload] (first (filter (partial ip-in ip) key-vals))]
;    payload))
  

(defn create-lookup
  [key-vals]
  (fn [ip] (lookup-ip key-vals ip)))

