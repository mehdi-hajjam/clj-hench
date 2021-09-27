(ns hench.core
  (:require [aleph.http :as http] 
            [reitit.ring :as ring]
            [muuntaja.core :as m]
            [reitit.middleware :as middleware]
            [reitit.ring.middleware.muuntaja :as muuntaja]
            [mount.core :as mount]
  )
)

(defn get-battlesnake-handler
  "Customization, latency checks and ping"
  [req]
  ;(clojure.pprint/pprint (:body-params req)) ;;for debugging
  {:body {:apiversion "1"
          :author "hbl206"
          :color "#61EB42"
          :head "evil"
          :tail "hook",
          :version "0.0.1"}})

(defn random-move
"A random move, shouting s"
  [moves]
  (if (= moves {}) 
        "up"          
        (rand-nth (mapv #(name %) (keys moves)))))

(defn avoid-walls
"Avoids the walls of the board"
  [body-params moves]
(let [xmax (- (-> body-params :board :width) 1)
      ymax (- (-> body-params :board :height) 1)
      xhead (-> body-params :you :head :x)
      yhead (-> body-params :you :head :y)]
(cond-> moves
  (= xmax xhead) (dissoc :right)
  (= ymax yhead) (dissoc :up)
  (= 0 xhead) (dissoc :left)
  (= 0 yhead) (dissoc :down))))

(defn avoid-self-direct-hits
"Avoids colliding with itself on the next move"
 [body-params moves]
(let [body (-> body-params :you :body)
      head (-> body-params :you :head)]
  (cond-> moves
   (some #(= (update head :x inc) %) body) (dissoc :right)
   (some #(= (update head :x dec) %) body) (dissoc :left)
   (some #(= (update head :y inc) %) body) (dissoc :up)
   (some #(= (update head :y dec) %) body) (dissoc :down)
  )))

(defn new-game-handler
  [req]
  ;(clojure.pprint/pprint (:body-params req)) ;;for debugging
  {:status 200 :body {}})

(defn move-handler
  [req]
  (let [body-params (:body-params req)]
  (clojure.pprint/pprint (-> body-params :you :head :x))
  (clojure.pprint/pprint (-> body-params :you :head :y))
  {:body {:move (->> {:up 0 :down 0 :right 0 :left 0}
                      (avoid-walls body-params)
                      (avoid-self-direct-hits body-params)
                      (random-move))
          :shout "Omae wa mo shinde iru!"}}))

(defn end-game-handler
  [req]
  ;(clojure.pprint/pprint (:body-params req)) ;;for debugging
  {:status 200 :body {}})

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

