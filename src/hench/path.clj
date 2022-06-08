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
;; am-intersections has been ordered for central, 4 escape routes intersections first
;; it will be nshuffled for a certain n to avoid obvious looping that'd be detrimental

(def am-intersections
  [;4 escape routes intersection, from closest to center to further away
   {:x 8 :y 13}
   {:x 10 :y 13}
   {:x 4 :y 11}
   {:x 14 :y 11}
   {:x 4 :y 7}
   {:x 14 :y 7}
   {:x 4 :y 17}
   {:x 14 :y 17}
   ;others, mostly 3 escape routes ones, and will be shuffled as well
   {:x 1 :y 1}
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
   {:x 6 :y 7}
   {:x 12 :y 7}
   {:x 17 :y 7}
   {:x 6 :y 9}
   {:x 8 :y 9}
   {:x 10 :y 9}
   {:x 12 :y 9}
   {:x 6 :y 11}
   {:x 8 :y 11}
   {:x 10 :y 11}
   {:x 12 :y 11}
   {:x 1 :y 15}
   {:x 4 :y 15}
   {:x 14 :y 15}
   {:x 17 :y 15}
   {:x 1 :y 17}
   {:x 6 :y 17}
   {:x 8 :y 17}
   {:x 10 :y 17}
   {:x 12 :y 17}
   {:x 17 :y 17}
   {:x 1 :y 19}
   {:x 4 :y 19}
   {:x 14 :y 19}
   {:x 17 :y 19}])

; used to derive which intersection after the food should be free for a path to food to be valid
(def food-intersections
  {"3 11" [{:x 4 :y 11} {:x 14 :y 11}]
   "9 11" [{:x 8 :y 11} {:x 10 :y 11}]
   "15 11" [{:x 4 :y 11} {:x 14 :y 11 }]})

; one of these should be free as well because more escape route
(def center-food-intersections
  {"8 11" [{:x 8 :y 9} {:x 8 :y 13}]
   "10 11" [{:x 10 :y 9} {:x 10 :y 13}]})

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

(defn create-base-graph
  [body-params]
  (board->ubergraph {:body []} body-params))

(defn remove-point-from-graph
  "Removes all vectors containing p either first or second"
  [p vv]
  (filterv #(not (in? (c->n p) %)) vv))

(defn asp
  "All shortest paths from p"
  [snake base-graph body-params]
  (let [g (apply uber/graph (remove-point-from-graph (second (:body snake)) base-graph))
        #_#_w (-> body-params :board :width)
        #_#_h (-> body-params :board :height)
        head (:head snake)]
    (alg/shortest-path g {:start-node (c->n head)
                          #_#_:heuristic-fn #(d (n->c %) head w h)})))

(comment
  (time (asp (:you am-sample) am-sample))
  "Elapsed time: 36.792916 msecs"
  ; of which building the ubergraph is longest!
  (time (apply uber/graph (board->ubergraph (:you am-sample) am-sample)))
  "Elapsed time: 39.049167 msecs"
  (time (asp (:you am-sample) base-graph am-sample))
  "Elapsed time: 6.671958 msecs" ; much better with base graph out
  )

;;
; Path validation
;;

; intersection detection

(defn list-intersections
  "Returns a list of all intersections encountered in path
   TAKES A PATH OF NAMES
   RETURNS A VECTOR OF COORDINATES OF INTERSECTIONS"
  [path]
  (let [named-intersections (filterv #(in? (n->c %) am-intersections) path)]
    (mapv #(n->c %) named-intersections)))

; intersection validation

(defn valid-intersection?
  "Returns true if the intersection is safe for snake"
  [i my-snake my-asp other-snakes other-asp]
  (let [my-length (:length my-snake)
        my-dist (count (alg/nodes-in-path (alg/path-to my-asp (c->n i))))]
    (loop [s other-snakes
           asps other-asp]
      (let [his-dist (count (alg/nodes-in-path (alg/path-to (first asps) (c->n i))))]
        (cond
          (= s []) true ; si y a plus de serpents à valider c'est que je suis bon
          (< my-dist his-dist) (recur (rest s)
                                      (rest asps)) ; si j'arrive avant le serpent je peux recur
          (and (> my-length (:length (first s)))
               (> my-dist (+ his-dist (:length (first s))))) (recur (rest s)
                                                                    (rest asps)) ; si j'arrive après la queue du serpent et il est plus petit, recur (pas besoin de refaire la comparaison des distances) 
          :else false ; sinon j'arrive sur le corps d'un serpent plus petit ou il est plus grand donc pas safe
          )))))


; obstacle validation (snakes encountered in the path)

(defn reverse-index
  "Returns the index of point p in snake s in reverse, e.g. tail is index 0 in the snake"
  [p snake]
  (let [his-body (:body snake)]
    (.indexOf (reverse his-body) p)))

(defn index-in-path
  [p path]
  (.indexOf path p))

(defn encounters?
  "Returns false if no encounter, first point of encounter otherwise.
   Beware, (rest path) only should be considered as my head will be there when I am the other-snake for myself otherwise"
  [path other-snake]
  (let [rpath (rest path)
        common (into [] (clojure.set/intersection (set rpath) (set (:body other-snake))))]
    (cond
      (= [] common) false
      :else (let [indexes (mapv #(.indexOf rpath %) common)
                  minval (apply min indexes)]
              {:e (nth path minval) :snake other-snake}))))

(defn list-encounters
  "Returns the list of encounters, even with my self, hence all-snakes and not other-snakes"
  [path all-snakes]
  (filterv true? (mapv #(encounters? path %) all-snakes)))

(defn non-lethal?
  "Returns true if obstacle (first encounter with a part of a snake) is safe (will have disappeared by then), false otherwise
   TAKES A PATH OF COORDINATES"
  [e path my-snake other-snake]
  (cond
    ; si c'est sa tête et qu'il est plus petit que moi c'est ok
    (and (= (:head other-snake) e)
         (< (:length other-snake) (:length my-snake))) true
    ; si c'est sa tête et qu'il est plus grand ou égal à moi c'est mort (le cas égal est débattable)
    (and (= (:head other-snake) e)
         (>= (:length other-snake) (:length my-snake))) false
    ; si l’indice dans son corps à l’envers de sa case que je rencontre est plus petit ou égal à l’indice de cette rencontre dans mon path c'est bon
    (<= (reverse-index e other-snake) (index-in-path e (rest path))) true
    :else false))

;BE CAREFUL IF I DON'T GET A VECTOR OF 1 OTHER SNAKE I COULD RUN INTO TYPE ISSUES, getting one element instead of a vector of 1 element
(defn list-of-lethal-encounters
  "Returns the list of lethal encounters
   TAKES A PATH OF COORDINATES (for list of encouters to work)"
  [path my-snake other-snakes]
  (let [enc-list (list-encounters path (conj other-snakes my-snake))
        leth-enc-list (mapv #(non-lethal? (:e %) path my-snake (:snake %)) enc-list)]
    (filterv false? leth-enc-list)))

; path validation using all three fns above
; must apply to all intersections in path, and all first obstacles in path
;; remember to always convert path back into coordinates with a mapv #(n->c %) call
;; "x y" should only be an interface format as much as possible

(defn valid?
  "Returns true if a path is valid, false otherwise
   TAKES A PATH OF NAMES BECAUSE OF LIST-INTERSECTIONS (could be changed if needed)"
  [path my-snake my-asp other-snakes other-asp]
  (let [rpath (rest path)
        int-list (list-intersections rpath)]
    (cond
      ; if even one intersection is invalid, return false
      (some false? (mapv #(valid-intersection? % my-snake my-asp other-snakes other-asp) int-list)) false
      ; if some encounters are lethal, return false
      (not= [] (list-of-lethal-encounters path my-snake other-snakes)) false
      :else true)))



;;
; Eatables
;; 

; coder que pour un path to food l'intersection après food doit être valide ET le path to food valide
; pour center food, il faut voir plus loin et l'une ou l'autre des center food intersections doit être libre aussi

(defn fvalid?
  "Returns true if a path to food is valid, false otherwise.
   A valid path to food is firstly a valid path, and then depending on the food there are some additional conditions
   TAKES A PATH OF NAMES BECAUSE OF VALID?"
  [path my-snake my-asp other-snakes other-asp]
  (let [food (last path)]
    (and (valid? path my-snake my-asp other-snakes other-asp)
         (cond
           (= "3 11" food) (every? true? (mapv #(valid-intersection? % my-snake my-asp other-snakes other-asp) (food-intersections food)))
           (= "15 11" food) (every? true? (mapv #(valid-intersection? % my-snake my-asp other-snakes other-asp) (food-intersections food)))
           (= "9 11" food) (let [list (list-intersections path)
                                 next-int (vector-difference (food-intersections food) list) ;it's a vector with one coordinate at this point
                                 ]
                             (and (valid-intersection? next-int my-snake my-asp other-snakes other-asp)
                                  (some true? (mapv #(valid-intersection? % my-snake my-asp other-snakes other-asp) (center-food-intersections food)))))))))



(defn eatables
  "Returns the path to the closest food that can be started during the turn"
  [body-params my-snake other-snakes my-asp other-asp]
  (let [available-food (-> body-params :board :food)]
    (cond
      (= available-food []) []
      :else (let [paths (mapv #(alg/nodes-in-path (alg/path-to my-asp (c->n %))) available-food)
                  valid-paths (filterv #(valid? % my-snake my-asp other-snakes other-asp) paths)]
              (cond
                (= valid-paths []) []
                ; I have paths of names here
                :else (mapv #(n->c %) (shortest valid-paths)))))))

;;
; killables
;;

(defn not-at-xroads
  "Returns the snakes which head is not at a crossroads or 9 11, amongst other-snakes"
  [other-snakes]
  (filterv #(not (in? (:head %) (into am-intersections [{:x 9 :y 11}]))) other-snakes))

(defn next-int
  "Returns the next intersection a snake not at an intersection will go through"
  [snake other-snakes other-asp]
  (let [i (.indexOf other-snakes snake)
        sasp (nth other-asp i)
        cpath (mapv #(n->c %) (alg/nodes-in-path (alg/path-to sasp "9 11"))) ;cpath is path of coordinates
        ints (filterv #(in? % am-intersections) cpath)]
    (cond
      (= ints []) (println "ERROR in next-int - cpath doesn't contain any intersection from am-intersections")
      :else {:intersection (first ints) :turns (.indexOf cpath (first ints)) :slength (:length snake)})))

(defn in-window?
  "Returns true if [path-length;path-length + my-length] contains a value of (:turns intersection)"
  [my-length path intersection]
  (cond
    (> my-length (:slength intersection)) (and (<= (- (count path) 1) (:turns intersection)) ;I am there first or at same time as opponent
                                               (>= (+ (- (count path) 1) my-length) (:turns intersection)) ;my tail is there after or same time as him
                                               )
    :else (and (< (- (count path) 1) (:turns intersection)) ;I am there strictly first
               (>= (+ (- (count path) 1) my-length) (:turns intersection)) ;my tail is there after or same time as him
               )))

(comment
  clj꞉hench.path꞉>  (in-window? 4 [0 1 2 3 4] {:turns 3 :slength 5})
false
clj꞉hench.path꞉> 
(in-window? 4 [0 1 2 3 4] {:turns 4 :slength 5})
false
clj꞉hench.path꞉> 
(in-window? 4 [0 1 2 3 4] {:turns 5 :slength 5})
true
clj꞉hench.path꞉> 
(in-window? 4 [0 1 2 3 4] {:turns 8 :slength 5})
true
clj꞉hench.path꞉> 
(in-window? 4 [0 1 2 3 4] {:turns 9 :slength 5})
false
  )

(defn killables
  "Returns the path to the closest killable snake"
  [my-snake my-asp other-snakes other-asp]
  (let [targets (not-at-xroads other-snakes)]
    (cond
      (= targets []) []
      :else (let [indexes (mapv #(.indexOf other-snakes %) targets)
                  their-asp (mapv #(nth other-asp %) indexes)
                  ints (mapv #(next-int % targets their-asp) targets)
                  my-npaths-to-ints (mapv #(alg/nodes-in-path (alg/path-to my-asp (c->n (:intersection %)))) ints)]
              (shortest (filterv true? (mapv #(in-window? (:length my-snake) %1 %2) my-npaths-to-ints ints)))))))


; Remember the first elem of a path is where my head is I think!!!!!!!
(defn strategize
  [])
