(ns clojure-iptools.config
  (:require [selmer.parser :as parser]
            [clojure.tools.logging :as log]
            [clojure-iptools.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[clojure_iptools started successfully using the development profile]=-"))
   :middleware wrap-dev})
