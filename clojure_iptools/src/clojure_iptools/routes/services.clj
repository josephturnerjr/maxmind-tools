(ns clojure-iptools.routes.services
  (:require [ring.util.http-response :refer :all]
            [compojure.api.sweet :refer :all]
            [schema.core :as s]))

(defapi service-routes
  (ring.swagger.ui/swagger-ui
   "/swagger-ui")
  ;JSON docs available at the /swagger.json route
  (swagger-docs
    {:info {:title "Sample api"}})
    (GET* "/:x" []
      :return      Long
      :path-params [x :- clojure.lang.BigInt]
      :summary     "x*x with path-parameters"
      (ok (* x x))))
