(ns hench.food
  (:require [hench.utils :refer :all]))

(defn visible?
  "Returns true if food is visible from head"
  [head obstacles food]
  (let [chull (convex-hull {:body [head food]})
        intersection (into [] (clojure.set/intersection
                               (set chull)
                               (set obstacles)))]
    (cond
      (= intersection []) true
      :else false)))

(defn accessible?
  "Returns true if we have the health to reach food"
  [s hazards food w h]
  (let [health (-> s :health)
        hdist (hd s food hazards w h)]
    (cond
      (<= hdist health) true
      :else false)))

(defn am-i-closest?
  "Returns true if I am closest than all other snakes"
  [body-params s other-snakes hazards food]
  (let [w (-> body-params :board :width)
        h (-> body-params :board :height)
        can-see (filterv #(visible? (:head %) (obstacles body-params %) food) other-snakes)
        can-access (filterv #(accessible? % hazards food w h) can-see)
        distances (mapv #(sd % food w h) can-access)
        my-distance (sd s food w h)]
    (cond
    ;corner case to treat : equal distance but I am longer is ok
      (= distances []) true
      (< my-distance (apply min distances)) true
      :else false)))

(defn food-to-eat
  "Returns a sorted vector of the food that I can eat, from closest to farthest"
  [body-params]
  (let [foods (food body-params)
        hazards (hazard body-params)
        s (-> body-params :you)
        w (-> body-params :board :width)
        h (-> body-params :board :height)
        head (-> s :head)
        obstacles (obstacles body-params s)
        other-snakes (other-snakes body-params)]
    ;(println "obstacles: " obstacles)
    (vec (->> foods
              (filterv #(visible? head obstacles %))
              (filterv #(accessible? s hazards % w h))
              (filterv #(am-i-closest? body-params s other-snakes hazards %))
              (sort-by #(sd s % w h))))))

(defn eat
  "Drives the snake to eat"
  [body-params moves]
  (println "EAT")
  (let [foods (food-to-eat body-params)
        snakes (other-snakes body-params)
        snakes-length (mapv #(:length %) snakes)
        me (-> body-params :you)
        my-health (-> me :health)
        my-length (-> me :length)]
    (println "foods: " foods)
    (cond
      (= foods []) moves
      #_(or (> 70 my-health)
            (<= my-length (apply max snakes-length)))
      (or (> 63 my-health)
          (<= my-length (+ 1 (apply max snakes-length)))
          ;(<= my-length (- (apply max snakes-length) 2))
          ) ;last clause not to be distanced too much by larger snake
      (let [food (first foods)
            head (-> body-params :you :head)
            chull (convex-hull {:body [head food]})]
        (probabilise-movements head chull 10 moves))
      :else moves)))

(defn barycentre
  "Returns the barycentre of a vector of points"
  [v]
  (let [c (count v)
        avx (quot (reduce + (mapv #(:x %) v)) c)
        avy (quot (reduce + (mapv #(:y %) v)) c)]
    {:x avx :y avy}))

(defn food-barycentre
  "Returns the barycentre of no hazard food"
  [body-params]
  (let [foods (-> body-params :board :food)
        hazards (-> body-params :board :hazards)]
    (barycentre (filterv #(not (hazard? % hazards)) foods))))

(defn free-space-barycentre
  "Returns the barycentre of the free space on the board"
  [body-params]
  (let [height (-> body-params :board :height)
        width (-> body-params :board :width)
        hazards (-> body-params :board :hazards)
        board (vec (for [x (vec (range 0 width))
                         y (vec (range 0 height))]
                     {:x x :y y}))
        free (filterv #(not (hazard? % hazards)) board)]
    (barycentre free)))

(defn recenter
  "Pushes the snake back to the barycentre of no hazard food or free space on the board if there's none of these. Does that when I am hungry or head in hazard. 
   3 so that recenter * hazard * best degree less than eat on food"
  [body-params moves]
  (println "RECENTER")
  (let [hazards (hazard body-params)
        foods (food body-params)
        snakes (other-snakes body-params)
        snakes-length (mapv #(:length %) snakes)
        free-food (filterv #(not (hazard? % hazards)) foods)
        target (if (= [] free-food)
                 (free-space-barycentre body-params)
                 (food-barycentre body-params))
        me (-> body-params :you)
        head (:head me)
        my-health (-> me :health)
        my-length (-> me :length)]
    (cond
      (or (> 63 my-health) ;I am hungry
          (<= my-length (+ 1 (apply max snakes-length))) ;hungry coz not large enough
          (hazard? head hazards)) ;My head is on hazards
      (let [chull (convex-hull {:body [head target]})]
        (probabilise-movements head chull 3 moves))
      :else moves)))
