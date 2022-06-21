(ns hench.core
  (:require [aleph.http :as http]
            [reitit.ring :as ring]
            [muuntaja.core :as m]
            [reitit.middleware :as middleware]
            [reitit.ring.middleware.muuntaja :as muuntaja]
            [mount.core :as mount]
            [hench.space :refer :all]
            [hench.food :refer :all]
            [hench.path :refer :all]
            [hench.shout :refer :all]
            [hench.samples :refer :all]
            [hench.utils :refer :all]))

(defn get-battlesnake-handler
  "Customization, latency checks and ping"
  [req]
  ;(clojure.pprint/pprint (:body-params req)) ;;for debugging
  {:body {:apiversion "1"
          :author "hbl206"
          :color "#61EB42" #_(rand-nth ["#2600E4" "#61EB42"])
          :head "evil"
          :tail "hook"
          :version "0.2.0"}})

(defn new-game-handler
  [req]
  ;(clojure.pprint/pprint (:body-params req)) ;;for debugging
  (clojure.pprint/pprint "A new game has started!")
  {:status 200 :body {}})

; Created once and for all to avoid calculating during the game (not even once I think)
(def base-graph (create-base-graph am-sample))

(defn move-handler
  [req]
  (let [body-params (:body-params req)
        base-graph base-graph
        other-snakes (other-snakes body-params)
        me (:you body-params)
        my-asp (asp me base-graph body-params)
        my-fasp (fasp me base-graph body-params 11) ;10 is chosen for specific cases that happen. Should work for all that I have seen so far.
        other-asp (mapv #(asp % base-graph body-params) other-snakes)]
    {:body {:move (->> {:up 1 :down 1 :right 1 :left 1}
                       (begin-turn body-params)
                       (avoid-hazards body-params) ; OK - means avoid walls really
                       (avoid-self-direct-hits body-params) ; OK
                       (avoid-other-snakes body-params) ; OK             
                       (strategize body-params my-asp my-fasp other-snakes other-asp)
                       (choose-move body-params))
            :shout (random-shout sfquotes)}}))

(defn end-game-handler
  [req]
  (let [snake (first (-> req :body-params :board :snakes))]
    (clojure.pprint/pprint "A game has stopped!")
    (println (str (:name snake) " WON!"))
    {:status 200 :body {}}))

(def app
  (ring/ring-handler
   (ring/router
    [["/" {:get get-battlesnake-handler
           :middleware [:content]}]
     ["/start" {:post new-game-handler
                :middleware [:content]}]
     ["/move" {:post move-handler
               :middleware [:content]}]
     ["/end" {:post end-game-handler
              :middleware [:content]}]]
      ;; router data affecting all routes
    {::middleware/registry {:content muuntaja/format-middleware}
     :data {:muuntaja  m/instance}})))

(mount/defstate server
  :start (do (http/start-server #'app {:port 8123})
             (println "Server started!"))
  :stop (.close server))

(defn -main [& _]
  (mount/start))


(comment
  (app {:request-method :get :uri "/"})
  (app {:request-method :post :uri "/start"})
  (app {:request-method :post :uri "/move"})
  (app {:request-method :post :uri "/end"}))