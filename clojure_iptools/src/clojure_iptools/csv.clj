(ns clojure-iptools.csv
  (:require [clojure-csv.core :refer [parse-csv]]
            [clojure.java.io :as io]))

(defn read-csv-file
  [filename f]
  (with-open [rdr (io/reader filename)]
    (vec (map f (rest (parse-csv (slurp filename)))))))
