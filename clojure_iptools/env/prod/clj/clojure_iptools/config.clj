(ns clojure-iptools.config
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init
   (fn []
     (log/info "\n-=[clojure_iptools started successfully]=-"))
   :middleware identity})
