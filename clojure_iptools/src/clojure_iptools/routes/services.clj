(ns clojure-iptools.routes.services
  (:require [ring.util.http-response :refer :all]
            [compojure.api.sweet :refer :all]
            [clojure-iptools.maxmind.core :refer [maxmind-lookup]]
            [clojure-iptools.ip :refer [parse-ip]]
            [schema.core :as s]))

(defapi service-routes
  (ring.swagger.ui/swagger-ui "/swagger-ui")
  (swagger-docs
    {:info {:title "GeoLite/ASN lookup"}})
    (GET* "/:ip" []
      :return      {:city s/Any :asn s/Any}
      :path-params [ip :- s/Str]
      :summary     "details on an ip"
      (ok (maxmind-lookup (parse-ip ip)))))
