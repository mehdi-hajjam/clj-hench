(ns hench.core
  (:require [aleph.http :as http]
            [reitit.ring :as ring]
            [muuntaja.core :as m]
            [reitit.middleware :as middleware]
            [reitit.ring.middleware.muuntaja :as muuntaja]
            [mount.core :as mount]
            [hench.space :refer :all]
            [hench.food :refer :all]))

(defn get-battlesnake-handler
  "Customization, latency checks and ping"
  [req]
  ;(clojure.pprint/pprint (:body-params req)) ;;for debugging
  {:body {:apiversion "1"
          :author "hbl206"
          :color "#61EB42"
          :head "evil"
          :tail "hook"
          :version "0.0.1"}})

(defn new-game-handler
  [req]
  ;(clojure.pprint/pprint (:body-params req)) ;;for debugging
  (clojure.pprint/pprint "A new game has started!")
  {:status 200 :body {}})

(defn move-handler
  [req]
  (let [body-params (:body-params req)]
  ;(clojure.pprint/pprint (-> body-params :you :head :x))
  ;(clojure.pprint/pprint (-> body-params :you :head :y))
    {:body {:move (->> {:up 1 :down 1 :right 1 :left 1}
                       (avoid-walls body-params)
                       (avoid-self-direct-hits body-params)
                       (avoid-other-snakes body-params)
                       (avoid-self-loop body-params)
                       (avoid-loop-with-walls body-params)
                       (avoid-hazards body-params)
                       (avoid-small-surfaces body-params) ;trop lent!! peut améliorer en ne regardant ça que quand une danger case détecte qqchose
                       (eat body-params)
                       (follow-tail body-params)
                       (recenter body-params)
                       (avoid-borders body-params)
                       (optionality body-params)
                       (fear body-params)
                       (choose-move body-params))
            :shout "Omae wa mo shinde iru!"}}))

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
  :start (http/start-server #'app {:port 8123})
  :stop (.close server))

(defn -main [& _]
  (mount/start))


(comment
  (app {:request-method :get :uri "/"})
  (app {:request-method :post :uri "/start"})
  (app {:request-method :post :uri "/move"})
  (app {:request-method :post :uri "/end"}))