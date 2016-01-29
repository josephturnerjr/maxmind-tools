(ns clojure-iptools.maxmind.asn
  (:require [clojure-iptools.csv :refer [read-csv-file]]
            [clojure-iptools.ip-lookup :refer [create-lookup]]
            [clojure-iptools.ip :refer [create-ip-range]]))

(defn parse-asn-string
  [s]
  (let [[_ asn owner] (re-matches #"(AS[0-9]*) ?(.*)" s)]
    [asn owner]))

(defn parse-asn
  [[start-s end-s asn-owner]]
  (let [ip-range (create-ip-range (Long. start-s) (Long. end-s))
       [asn owner] (parse-asn-string asn-owner)]
    [ip-range {:asn asn :owner owner}]))

(defn parse-asns
  [filename]
  (create-lookup (read-csv-file filename parse-asn)))
