(ns hench.path
  (:require [hench.utils :refer :all]))

;; from https://matthewdowney.github.io/astar-in-clojure-find-k-shortest-paths.html
(declare a*-seq, next-a*-path, unseen?, step-factory, rpath, cmp-step)

(defn a*
  "A sequence of paths from `src` to `dest`, shortest first, within the supplied `graph`.
  If the graph is weighted, supply a `distance` function. To make use of A*, supply a 
  heuristic function. Otherwise performs like Dijkstra's algorithm."
  [graph src dest & {:keys [distance heuristic]}]
  (let [init-adjacent (sorted-set-by cmp-step {:node src :cost 0 :entered 0})]
    (a*-seq graph dest init-adjacent
            (or distance (constantly 1))
            (or heuristic (constantly 0)))))

(defn a*-seq
  "Construct a lazy sequence of calls to `next-a*-path`, returning the shortest path first."
  [graph dest adjacent distance heuristic]
  (lazy-seq
   (when-let [[path, adjacent'] (next-a*-path graph dest adjacent distance heuristic)]
     (cons path (a*-seq graph dest adjacent' distance heuristic)))))

(defn next-a*-path [graph dest adjacent f-cost f-heur]
  (when-let [{:keys [node] :as current} (first adjacent)]
    (let [path (rpath current)
          adjacent' (disj adjacent current)] ;; "pop" the current node
      (if (= node dest)
        [(reverse path), adjacent']
        (let [last-idx (or (:entered (last adjacent')) 0)
              factory (step-factory current last-idx f-cost f-heur dest)
              xform (comp (filter (partial unseen? path)) (map-indexed factory))
              adjacent'' (into adjacent' xform (get graph node))]
          (recur graph dest adjacent'' f-cost f-heur))))))

(defn unseen? [path node]
  (not-any? #{node} path))

(defn step-factory [parent last-insertion cost heur dest]
  (fn [insertion-idx node]
    {:parent parent
     :node node
     :entered (+ last-insertion (inc insertion-idx))
     :cost (+ (:cost parent) (cost (:node parent) node) (heur node dest))}))

(defn rpath [{:keys [node parent]}]
  (lazy-seq
   (cons node (when parent (rpath parent)))))

(defn cmp-step [step-a step-b]
  (let [cmp (compare (:cost step-a) (:cost step-b))]
    (if (zero? cmp)
      (compare (:entered step-a) (:entered step-b))
      cmp)))

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
  "all snakes but my head and all tails"
  [body-params]
  (let [board (:board body-params)
        width (:width board)
        height (:height board)
        my-length (-> body-params :you :length)
        my-body (-> body-params :you :body)
        my-head (-> body-params :you :head)
        my-body-head (filterv #(not (= % my-head)) (butlast my-body))
        snakes (other-snakes body-params)
        projected-heads (into [] (apply concat
                                        (mapv #(if (< (:length %) my-length)
                                                 []
                                                 (project-head % width height)) snakes)))
        all-bodies (into [] (apply concat (mapv #(vec (butlast (:body %))) snakes)))]
    (into [] (concat my-body-head projected-heads all-bodies))))

(defn filter-keys
  "Returns a map with keys whose keys are not forbidden"
  [m forbidden]
  (let [str-forbidden (mapv #(c->n %) forbidden)]
    (select-keys m (filterv #(not (in? % str-forbidden)) (keys m)))))

(defn board->graph
  [body-params]
  (let [board (-> body-params :board)
        w (-> board :width)
        h (-> board :height)
        m {}
        snaky (snaky body-params)
        whole-graph (into {} (for [x (range 0 w)
                                   y (range 0 h)]
                               (assoc m (c->n {:x x :y y}) (neighbours #(not (in? % snaky)) {:x x :y y} w h))))]
    (filter-keys whole-graph snaky)))

;;
; Path related calculations
;;

(defn uhcost
  "Returns the unitary health cost of a step in a path"
  [c hazards]
  (cond
    (in? (n->c c) hazards) 15 ;as per https://blog.battlesnake.com/updates-to-royale-mode-and/
    :else 1))

(defn hcost
  "Returns the health cost of a path"
  [path hazards]
  (reduce + (map #(uhcost % hazards) path)))

(defn reachable?
  "Returns the length (nb of steps) of fastest feasible path for snake"
  [snake path hazards]
  (let [h (:health snake)
        c (hcost path hazards)]
    (cond 
      (>= h c) (count path)
      :else false)))

(defn sfp 
  "Shortest feasible path"
  [snake end board hazards w h]
  (let [ns (c->n (:head snake))
        ne (c->n end)
        all-paths (a* board ns ne :heuristic #(d (n->c %1) (n->c %2) w h))
        afp (filter #(reachable? snake % hazards) all-paths)]
    (first afp)))

(defn lsfp
  "Length or duration of sfp"
  [snake end board hazards w h]
  (count (sfp snake end board hazards w h)))
