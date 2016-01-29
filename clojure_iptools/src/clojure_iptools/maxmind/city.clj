(ns clojure-iptools.maxmind.city
  (:require [clojure-iptools.csv :refer [read-csv-file]]
            [clojure-iptools.ip-lookup :refer [create-lookup]]
            [clojure-iptools.ip :refer [ip-range-from-cidr]]))

(defn parse-location
  [[geoname-id, _, continent-code, continent-name, country-code,
         country-name, r1-code, r1-name,
         r2-code, r2-name, city, _, _]]
  [geoname-id {:continent [continent-code continent-name] :country [country-code country-name] :r1 [r1-code r1-name] :r2 [r2-code r2-name] :city city}])

(defn parse-locations
  [filename]
  (apply hash-map (apply concat (read-csv-file filename parse-location))))

(defn parse-float
  [f]
  (if (empty? f) nil (Float. f)))

(defn parse-range
  [locations csv-data]
  (let [[network, geoname_id, registered_country_geoname_id, _, _, _, _, latitude, longitude] csv-data
        location-id (if (empty? geoname_id) registered_country_geoname_id geoname_id)
        ip-range (ip-range-from-cidr network)]
    [ip-range {
      :location (locations location-id)
      :latitude (parse-float latitude)
      :longitude (parse-float longitude)}]))

(defn parse-ranges
  [locations filename]
  (read-csv-file filename (partial parse-range locations)))

(defn parse-cities
  [location-filename range-filename]
  (let [locations (parse-locations location-filename)]
    (create-lookup (parse-ranges locations range-filename))))
