(ns clojure-iptools.maxmind.core
  (:require [clojure-iptools.maxmind.city :as city]
            [clojure-iptools.maxmind.asn :as asn]
            [clojure-iptools.ip :refer [parse-ip]]
            [clojure.string :refer [join]]
            [clojure.java.io :refer [file]]
            [mount.core :refer [defstate]]))

(defn create-lookup
  []
  (let [dir "/Users/jturner/projects/iptools/python/.data"
        city-lookup (city/parse-cities (file dir "GeoCity" "GeoLite2-City-Locations-en.csv") (file dir "GeoCity" "GeoLite2-City-Blocks-IPv4.csv"))
        asn-lookup (asn/parse-asns (file dir "GeoASN.csv"))]
    (fn [ip] {:city (city-lookup ip) :asn (asn-lookup ip)})))

(defstate maxmind-lookup :start (create-lookup))

(defn rand-ip
  []
  (join "." (take 4 (repeatedly #(rand-int 255)))))

(defn timeit
  []
  (let [iters 1000000
        lookup maxmind-lookup
        ips (doall (take iters (repeatedly rand-ip)))
        start (. System (nanoTime))
        _ (last (map (comp maxmind-lookup parse-ip) ips))
        seconds (/ (double (- (. System (nanoTime)) start)) 1000000000.0)]
        (prn (str iters " iterations: " seconds " seconds (" (/ (double iters) seconds) " iterations/sec)"))))
