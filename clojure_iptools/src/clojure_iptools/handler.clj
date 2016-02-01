(ns clojure-iptools.handler
  (:require [compojure.core :refer [defroutes routes wrap-routes]]
            [clojure-iptools.maxmind.core :refer [timeit]]
            [clojure-iptools.layout :refer [error-page]]
            [clojure-iptools.routes.services :refer [service-routes]]
            [clojure-iptools.middleware :as middleware]
            [clojure.tools.logging :as log]
            [compojure.route :as route]
            [config.core :refer [env]]
            [clojure-iptools.config :refer [defaults]]
            [mount.core :as mount]
            [luminus-log4j.core :as log-adapter]))

(defn init
  "init will be called once when
   app is deployed as a servlet on
   an app server such as Tomcat
   put any initialization code here"
  []
  (log-adapter/init env)
  (doseq [component (:started (mount/start))]
    (log/info component "started"))
  (timeit)
  (timeit)
  ((:init defaults)))

(defn destroy
  "destroy will be called when your application
   shuts down, put any clean up code here"
  []
  (log/info "clojure_iptools is shutting down...")
  (doseq [component (:stopped (mount/stop))]
    (log/info component "stopped"))
  (log/info "shutdown complete!"))

(def app-routes
  (routes
    (var service-routes)
    (route/not-found
      (:body
        (error-page {:status 404
                     :title "page not found"})))))

(def app (middleware/wrap-base #'app-routes))
