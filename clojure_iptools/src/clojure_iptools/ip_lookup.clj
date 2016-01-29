(ns clojure-iptools.ip-lookup)

(defn ip-in
  [ip [[start end] _]]
  (and (>= ip start) (<= ip end)))

(defn lookup-ip
  [key-vals ip]
  (let [[_ payload] (first (filter (partial ip-in ip) key-vals))]
    payload))

(defn create-lookup
  [key-vals]
  (fn [ip] (lookup-ip key-vals ip)))

