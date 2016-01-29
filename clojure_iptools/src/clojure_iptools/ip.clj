(ns clojure-iptools.ip)

(defn create-ip-range
  [start end]
  [start end])

(defn netmask-to-bitmask
  [nm]
  (bit-and 0xffffffff (bit-shift-left 0xffffffff (- 32 nm))))

(defn parse-ip
  [ip-string]
  (let [addr (map #(Integer/parseInt %) (clojure.string/split ip-string #"\."))]
    (reduce #(+ %2 (bit-shift-left %1 8)) 0 (map #(bit-and 0xff (int %1)) addr))))

(defn parse-cidr
  [cidr]
  (let [[ip netmask] (clojure.string/split cidr #"/")]
    [(parse-ip ip) (Integer. netmask)]))

(defn ip-range-from-cidr
  [cidr]
  (let [[ip nm] (parse-cidr cidr)
        mask (netmask-to-bitmask nm)
        start (bit-and ip mask)
        end (+ start (bit-xor 0xffffffff mask))]
    (create-ip-range start end)))
