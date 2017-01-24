(ns bikepoints.core
  (:require
   [cheshire.core :as c]
   [geolocation.core :as g]
   [hiccup.core :as h]
   [yada.yada :as y]
   [aleph.netty :as an])
  (:gen-class))

;; Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def leyton [51.5619513 -0.0153277])

(def config
  {:app-id "073acb22"
   :app-key "5164ae2f452aca8433da2bf95b033883"
   :port 3000
   :base "https://api.tfl.gov.uk/BikePoint"})

;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn bounding-box [[lat lon] radius]
  (->> radius
    (g/bounds (g/from-degrees lat lon))
    (mapv g/to-degrees)))

(defn mk-nearby-query
  [{:keys [base app-id app-key]}
   [[sw-lat sw-lon] [ne-lat ne-lon]]]
  (let [q "%s?swLat=%.4f&swLon=%.4f&neLat=%.4f&neLon=%.4f&app_id=%s&app_key=%s"]
    (format q base sw-lat sw-lon ne-lat ne-lon app-id app-key)))

(defn fetch [url]
  (-> url slurp (c/parse-string true)))

;; Queries ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn nbbikes [d]
  (->> d
    (:additionalProperties)
    (some #(when (= (:key %) "NbBikes") %))
    (:value)))

(defn bikepoints-nearby
  "coords - vector of lat, lon
   radius - specify a circle of radius kilometers around coords
   limit - max number of results returned

   Returns a sequence of maps comprised of BikePoint id, commonName and NbBikes"
  [coords radius limit]
  (->> (bounding-box coords radius)
    (mk-nearby-query config)
    (fetch)
    (take limit)
    (map #(-> %
            (select-keys [:id :commonName])
            (assoc :nbBikes (nbbikes %))))))

;; Server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn render-table
  [bikepoints]
  (h/html
    [:html {:style "font-family: sans-serif;"}
     [:body [:h1 "Five Bike Points near to Leyton"]
      [:table
       [:thead [:tr [:th "ID"] [:th "Name"] [:th "Bikes available"]]]
       [:tbody
        (for [{:keys [id commonName nbBikes]} bikepoints]
          [:tr [:td id] [:td commonName] [:td nbBikes]])]]]]))

(defn server [port]
  (let [d {:produces [{:media-type #{"text/html" "application/json;q=0.9"}
                       :charset "UTF-8"}]
           :response (fn [ctx]
                       (let [bikepoints (bikepoints-nearby leyton 2 5)]
                         (case (y/content-type ctx)
                           "text/html" (render-table bikepoints)
                           bikepoints)))}
        add-auth #(assoc % :access-control
                    {:scheme "Basic"
                     :verify (fn [[user password]]
                               (when (= [user password] ["guest" "guest"])
                                 {:user "guest" :roles #{"user"}}))
                     :authorization {:methods {:get "user"}}})]
    (y/listener ["/" [["bikepoints" (y/resource d)]
                      ["bikepoints-auth" (y/resource (add-auth d))]
                      [true (y/as-resource nil)]]]
      {:port port})))

(defn -main
  [& args]
  (println "Hello, JUXT!")
  (server (:port config))
  ;; Bit ugly but :wait-for-close doesn't seem to be available even
  ;; though #115 is closed (in 1.2.1?, but this is not in clojars yet...)
  @(promise))
