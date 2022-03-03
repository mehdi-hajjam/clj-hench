(ns hench.utils
  (:require [clojure.data :as data]))

(defn food
  [body-params]
  (-> body-params :board :food))

(defn hazard
  [body-params]
  (-> body-params :board :hazards))

(defn hazard?
  [c hazards]
  (some #(= c %) hazards))

(defn add
  "Vectorial addition"
  [c1 c2]
  {:x (+ (:x c1) (:x c2)) :y (+ (:y c1) (:y c2))})

(defn substract
  "Vectorial substraction"
  [c1 c2]
  {:x (- (:x c1) (:x c2)) :y (- (:y c1) (:y c2))})

(defn head
  "Returns the head of the snake"
  [body]
  (first body))

(defn neck
  "Returns the neck of the snake"
  [body]
  (second body))

(defn tail
  "Returns the tail of the snake"
  [body]
  (last body))

(defn other-snakes
  "Returns only the other snakes (I shouldn't project my own head!)"
  [body-params]
  (let [snakes (-> body-params :board :snakes)
        me (-> body-params :you)
        myID (:id me)]
    (filterv #(not= myID (:id %)) snakes)))

(defn multiply-points
  "Creates the points' doppelgangers in all 4 directions"
  [p width height]
  [p
   (add p {:x width :y 0})
   (substract p {:x width :y 0})
   (add p {:x 0 :y height})
   (substract p {:x 0 :y height})])

(defn wrap-multiply
  "Duplicates points in the grid in all 4 directions"
  [v width height]
  (vec (apply concat (mapv #(multiply-points % width height) v))))

(defn update-snake
  "Updates the snakes bodies and heads under :snakes"
  [snake width height]
  (-> snake
      (update-in [:body] #(wrap-multiply % width height))))

(defn update-snakes
  "Updates all snakes"
  [snakes width height]
  (mapv #(update-snake % width height) snakes))

(defn project-head
  "Returns a vector of all the potential head locations of snake s.
   Needs to take into account head doppelgangers as head is not a vector in a snake map"
  [s w h]
  (let [body (:body s)
        head (:head s)
        neck (nth body 1) ;the square before the head
        all-squares [(update head :x inc)
                     (update head :x dec)
                     (update head :y inc)
                     (update head :y dec)] ;all the squares around the head
        ]
    (wrap-multiply (vec (remove #(= % neck) all-squares)) w h)))

(defn obstacles
  "Returns all the obstacles on the board game in one vector : namely all the other snakes's projected bodies (ie no tail but all projected heads"
  [body-params s]
  (let [all-snakes (-> body-params :board :snakes)
        snakes (vec (remove #(= (:id s) (:id %)) all-snakes)) ; compare ids otherwise am now in troube since you and me in snakes are different by :ndbody key at least
        length (-> s :length)
        head (-> s :head)
        board (:board body-params)
        width (:width board)
        height (:height board)
        projected-heads
        (into [] (apply concat
                        (mapv #(if (< (:length %) length)
                                 []
                                 (project-head % width height)) snakes))) ;only project heads if the snake is more healthy or equally healthy
        all-bodies (into [] (apply concat (mapv #(vec (butlast (:body %))) snakes))) ;the tail is ok - corner case to be added if the head is close to food though
        ]
    (into [] (concat all-bodies projected-heads))))

(defn remove-point
  "remove-point removes point p from vector v and returns the new v"
  [p v]
  (let [i (.indexOf v p)] ;;(select-keys p [:name :coor])
    (cond
      (= i -1) v
      (= 1 (count v)) []
      :else (into [] (concat (subvec v 0 i) (subvec v (min (count v) (+ i 1))))))))

(defn vector-difference
  "Returns the difference of two vectors"
  [v1 v2]
  (let [common (into [] (last (data/diff (set v1) (set v2))))]
    ;(println "common: " common)
    (loop [v v1
           c common]
      (cond
        (= c []) v
        :else (recur (remove-point (first c) v)
                     (into [] (rest c)))))))

(defn convex-hull
  "Returns the convex-hull of a snake"
  [s]
  (let [body (:body s)
        xmin (apply min (mapv #(:x %) body))
        ymin (apply min (mapv #(:y %) body))
        xmax (apply max (mapv #(:x %) body))
        ymax (apply max (mapv #(:y %) body))]
    (vec (for [x (vec (range xmin (+ xmax 1)))
               y (vec (range ymin (+ ymax 1)))]
           {:x x :y y}))))

;; should be min of d and distance by warping

(defn d
  "Mathematical distance without obstacles"
  [a b]
  (+ (Math/abs (- (:x a) (:x b)))
     (Math/abs (- (:y a) (:y b)))))

(defn sd
  "Snake distance to a point without obstacles"
  [s c]
  (let [head (-> s :head)
        neck (second (-> s :body))
        d (d head c)]
    (cond
      (and (= (:x c) (:x head) (:x neck))
           (< (Math/abs (- (:x c) (:x neck)))
              (Math/abs (- (:x c) (:x head)))))
      (+ d 2)
      (and (= (:y c) (:y head) (:y neck))
           (< (Math/abs (- (:y c) (:y neck)))
              (Math/abs (- (:y c) (:y head)))))
      (+ d 2)
      :else d)))

(defn min-hazard-in-x
  "Returns the minimum number of cases in the x direction that are hazards"
  [chull hazards]
  (cond
    (= chull []) 0
    :else
    (let [xmin (apply min (mapv #(:x %) chull))
          xmax (apply max (mapv #(:x %) chull))]
      (loop [x xmin
             res (count chull)]
        (let [c (count (filterv #(hazard? % hazards) (filterv (fn [a] (= x (:x a))) chull)))]
          (cond
            (= x (+ xmax 1)) res
            (= c 0) 0
            :else (recur (+ x 1)
                         (min res c))))))))

(defn min-hazard-in-y
  "Returns the minimum number of cases in the y direction that are hazards"
  [chull hazards]
  (cond
    (= chull []) 0
    :else
    (let [ymin (apply min (mapv #(:y %) chull))
          ymax (apply max (mapv #(:y %) chull))]
                ;(println "ymin: " ymin)
                ;(println "ymax: " ymax)
      (loop [y ymin
             res (count chull)]
        (let [c (count (filterv #(hazard? % hazards) (filterv (fn [a] (= y (:y a))) chull)))]
                  ;(println "c: " c)
          (cond
            (= y (+ ymax 1)) res
            (= c 0) 0
            :else (recur (+ y 1)
                         (min res c))))))))

(defn hd
  "Health distance, ie taking hazards into account.
  It is equal to sd*1 + 14*(min hazard in x + min hazard in y - 1) -1*dirac(head is on a hazard case) when there are no obstacles in the convex hull
   Adding own body to hazards, see https://play.battlesnake.com/g/528bfb5d-a390-413d-83d1-a0bd1620484b/ move 178"
  [s c hazards]
  (let [head (-> s :head)
        hazards+body (vec (concat hazards (:body s)))
        chull (convex-hull {:body [head c]})
        in-x (min-hazard-in-x chull hazards+body)
        in-y (min-hazard-in-y chull hazards+body)
        temp-res (+ (sd s c)
                    (* (+ in-x in-y) 14))]
                    ;(println "in-x: " in-x)
                    ;(println "in-y: " in-y)
    (cond-> temp-res
      (and (< 0 in-x) (< 0 in-y)) (- 14)
      (hazard? head hazards) (- 14)
      (hazard? c hazards) (- 14))))

(defn d-to-others
  ""
  [head others]
  (mapv #(d head (:head %)) others))

(defn border?
  "Returns true if c is in the border"
  [body-params c]
  (let [height (-> body-params :board :height)
        width (-> body-params :board :width)
        hazards (-> body-params :board :hazards)
        board (vec (for [x (vec (range 0 width))
                         y (vec (range 0 height))]
                     {:x x :y y}))
        head (-> body-params :you :head)
        free (filterv #(not (hazard? % hazards)) board)
        xmin (apply min (mapv #(:x %) free))
        xmax (apply max (mapv #(:x %) free))
        ymin (apply min (mapv #(:y %) free))
        ymax (apply max (mapv #(:y %) free))
        borders (vec (concat
                      (filterv #(= xmin (:x %)) free)
                      (filterv #(= xmax (:x %)) free)
                      (filterv #(= ymin (:y %)) free)
                      (filterv #(= ymax (:y %)) free)))]
    (cond
      (some #(= c %) borders) true
      :else false)))

(defn free?
  "Returns true if c is free, false otherwise"
  [c obstacles hazards]
  (not (or
        (some #(= c %) hazards)
        (some #(= c %) obstacles))))

(defn free-cases
  "Returns free cases next to c"
  [c all-obstacles hazards]
  (let [all-squares [(update c :x inc)
                     (update c :x dec)
                     (update c :y inc)
                     (update c :y dec)]
        all-free-squares (filterv #(free? % all-obstacles hazards) all-squares)]
    all-free-squares))

(defn probabilise-movements
  [head obstacles f moves]
  (let [new-moves (cond-> moves
                    (some #(= (update head :x inc) %) obstacles) (update :right #(* f %))
                    (some #(= (update head :x dec) %) obstacles) (update :left #(* f %))
                    (some #(= (update head :y inc) %) obstacles) (update :up #(* f %))
                    (some #(= (update head :y dec) %) obstacles) (update :down #(* f %)))]
    (println new-moves)
    new-moves))

(defn make-wall
  "Returns a vector of the cells of the wall"
  [string height width]
  (cond
    (= string "top") (mapv (fn [a] {:x a :y height}) (range -1 (+ width 1)))
    (= string "down") (mapv (fn [a] {:x a :y -1}) (range -1 (+ width 1)))
    (= string "left") (mapv (fn [a] {:x -1 :y a}) (range -1 (+ height 1)))
    (= string "right") (mapv (fn [a] {:x width :y a}) (range -1 (+ height 1)))))

(defn all-walls
  "Returns all the walls of a game"
  [height width]
  (into [] (apply concat (mapv #(make-wall % height width) ["top" "down" "left" "right"]))))

(defn all-obstacles
  "Returns all the obstacles including the walls"
  [body-params s]
  (into [] (concat (into [] (butlast (:body s)))
                   (obstacles body-params s)
                   (all-walls (-> body-params :board :height)
                              (-> body-params :board :width)))))

(defn concatv
  [v1 v2]
  (into [] (concat v1 v2)))

(defn touched-walls
  "Returns the walls (vector of cells) touched by snake s.
 The coordinates might be negative."
  [subbody board]
  (let [h (:height board)
        w (:width board)
        b subbody]
    (cond->> []
      (some #(= 0 (:x %)) b) (concatv (make-wall "left" h w))
      (some #(= (- w 1) (:x %)) b) (concatv (make-wall "right" h w))
      (some #(= 0 (:y %)) b) (concatv (make-wall "down" h w))
      (some #(= (- h 1) (:y %)) b) (concatv (make-wall "top" h w)))))