(ns clojure-iptools.maxmind.core
  (:require [clojure-iptools.maxmind.city :as city]
            [clojure-iptools.maxmind.asn :as asn]
            [clojure.java.io :refer [file]]
            [mount.core :refer [defstate]]))

(defn create-lookup
  []
  (let [dir "/Users/jturner/projects/iptools/python/.data"
        city-lookup (city/parse-cities (file dir "GeoCity" "GeoLite2-City-Locations-en.csv") (file dir "GeoCity" "GeoLite2-City-Blocks-IPv4.csv"))
        asn-lookup (asn/parse-asns (file dir "GeoASN.csv"))]
    (fn [ip] {:city (city-lookup ip) :asn (asn-lookup ip)})))

(defstate maxmind-lookup :start (create-lookup))
