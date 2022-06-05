(ns hench.path
  (:require [hench.utils :refer :all]
            [hench.samples :refer :all]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]))

;;transforms board into graph

(defn c->n
  "Coordinates to name"
  [{:keys [x y]}]
  (str x " " y))

(defn n->c
  "Name to coordinates"
  [s]
  (let [sp (clojure.string/split s #"\s+")]
    {:x (read-string (first sp)) :y (read-string (second sp))}))

(defn neighbours
  "From coordinates to set of neighbours names"
  [criteria c w h]
  (set (mapv #(c->n %) (filterv criteria [(add c {:x 1 :y 0} w h)
                                          (add c {:x 0 :y 1} w h)
                                          (add c {:x -1 :y 0} w h)
                                          (add c {:x 0 :y -1} w h)]))))

(defn snaky
  "all snakes but snake's head and all tails"
  [snake body-params]
  (let [board (:board body-params)
        width (:width board)
        height (:height board)
        s-length (-> snake :length)
        s-body (-> snake :body)
        s-head (-> snake :head)
        s-body-head (filterv #(not (= % s-head)) (butlast s-body))
        snakes (other-snakes snake body-params)
        projected-heads (into [] (apply concat
                                        (mapv #(if (< (:length %) s-length)
                                                 []
                                                 (project-head % width height)) snakes)))
        all-bodies (into [] (apply concat (mapv #(vec (butlast (:body %))) snakes)))]
    (into [] (concat s-body-head projected-heads all-bodies))))

(defn filter-keys
  "Returns a map with keys whose keys are not forbidden"
  [m forbidden]
  (let [str-forbidden (mapv #(c->n %) forbidden)]
    (select-keys m (filterv #(not (in? % str-forbidden)) (keys m)))))

;;
; Intersections
;; 

;; Should be a function later to adapt to any map
;; An intersection is a point that has strictly more than 2 neighbours that are not hazards
;; but that def doesn't work for groups of free cases (comme en y 8 x 1 par exemple)

(def am-intersections
  [{:x 1 :y 1}
   {:x 8 :y 1}
   {:x 10 :y 1}
   {:x 17 :y 1}
   {:x 2 :y 3}
   {:x 16 :y 3}
   {:x 4 :y 5}
   {:x 6 :y 5}
   {:x 8 :y 5}
   {:x 10 :y 5}
   {:x 12 :y 5}
   {:x 14 :y 5}
   {:x 1 :y 7}
   {:x 4 :y 7}
   {:x 6 :y 7}
   {:x 12 :y 7}
   {:x 14 :y 7}
   {:x 17 :y 7}
   {:x 6 :y 9}
   {:x 8 :y 9}
   {:x 10 :y 9}
   {:x 12 :y 9}
   {:x 4 :y 11}
   {:x 6 :y 11}
   {:x 8 :y 11}
   {:x 10 :y 11}
   {:x 12 :y 11}
   {:x 14 :y 11}
   {:x 8 :y 13}
   {:x 10 :y 13}
   {:x 1 :y 15}
   {:x 4 :y 15}
   {:x 14 :y 15}
   {:x 17 :y 15}
   {:x 1 :y 17}
   {:x 4 :y 17}
   {:x 6 :y 17}
   {:x 8 :y 17}
   {:x 10 :y 17}
   {:x 12 :y 17}
   {:x 14 :y 17}
   {:x 17 :y 17}
   {:x 1 :y 19}
   {:x 4 :y 19}
   {:x 14 :y 19}
   {:x 17 :y 19}])

;;
; Trying ubergraph
;;

(defn massoc 
  "Assoc all elements of n(eighbours) to p(oint) in v(ector)"
  [v p n]
  (loop [n n
         res v]
    (cond
      (= n []) res
      :else (recur (rest n)
                   (into res [[p (first n)]])))))

(defn board->ubergraph
  [snake body-params]
  (let [board (-> body-params :board)
        w (-> board :width)
        h (-> board :height)
        m {}
        hazards (-> board :hazards)
        neck (neck (-> snake :body))
        forbidden (into hazards [neck])
        whole-graph (into {} (for [x (range 0 w)
                                   y (range 0 h)]
                               (let [n (neighbours #(not (in? % forbidden)) {:x x :y y} w h)]
                                 (assoc m (c->n {:x x :y y}) (massoc [] (c->n {:x x :y y}) n)))))
        whole-map (filter-keys whole-graph forbidden)]
    (into [] (apply concat (vals whole-map)))))

(comment
  (def g (apply uber/graph (board->ubergraph (:you am-sample) am-sample)))
  (uber/pprint g)
  ;without end node, gives recipe for whole board
  (def outof21 (alg/shortest-path g {:start-node "2 1"}))
  (alg/nodes-in-path (alg/path-to outof21 "9 11"))
  (time (def outof21 (alg/shortest-path g {:start-node "2 1"}))) ; "Elapsed time: 1.687958 msecs"
  (time (def outof21h (alg/shortest-path g {:start-node "2 1" :heuristic-fn #(d (n->c %) {:x 2 :y 1} 19 21)}))) ; "Elapsed time: 27.21775 msecs"
  ; ==> it seems using a heuristic is detrimental here!!!
  )

(defn asp
  "All shortest paths from p"
  [snake body-params]
  (let [g (apply uber/graph (board->ubergraph snake body-params))
        w (-> body-params :board :width)
        h (-> body-params :board :height)
        head (:head snake)]
    (alg/shortest-path g {:start-node (c->n head)
                          #_#_:heuristic-fn #(d (n->c %) head w h)})))

(comment
  (time (asp (:you am-sample) am-sample))
"Elapsed time: 36.792916 msecs"
  )