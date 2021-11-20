(ns hench.food
  (:require [hench.space :as space]
            [clojure.data :as data]))

(defn food
  [body-params]
  (-> body-params :board :food))

(defn hazard
  [body-params]
  (-> body-params :board :hazards))

(defn hazard?
  [c hazards]
  (some #(= c %) hazards))

(defn remove-point [p v]
  "remove-point removes point p from vector v and returns the new v"

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

(defn d
  "Mathematical distance withiut obstacles"
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
        chull (space/convex-hull {:body [head c]})
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

(defn visible?
  "Returns true if food is visible from head"
  [head obstacles food]
  (let [chull (space/convex-hull {:body [head food]})
        intersection (into [] (clojure.set/intersection
                               (set chull)
                               (set obstacles)))]
    (cond
      (= intersection []) true
      :else false)))

(defn accessible?
  "Returns true if we have the health to reach food"
  [s hazards food]
  (let [health (-> s :health)
        hdist (hd s food hazards)]
    (cond
      (<= hdist health) true
      :else false)))

(defn am-i-closest?
  "Returns true if I am closest than all other snakes"
  [body-params s other-snakes hazards food]
  (let [can-see (filterv #(visible? (:head %) (space/obstacles body-params %) food) other-snakes)
        can-access (filterv #(accessible? % hazards food) can-see)
        distances (mapv #(sd % food) can-access)
        my-distance (sd s food)]
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
        head (-> s :head)
        obstacles (space/obstacles body-params s)
        other-snakes (space/other-snakes body-params)]
    (vec (->> foods
              (filterv #(visible? head obstacles %))
              (filterv #(accessible? s hazards %))
              (filterv #(am-i-closest? body-params s other-snakes hazards %))
              (sort-by #(sd s %))))))

(defn eat
  "Drives the snake to eat"
  [body-params moves]
  (println "EAT")
  (let [foods (food-to-eat body-params)
        snakes (space/other-snakes body-params)
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
            chull (space/convex-hull {:body [head food]})]
        (space/probabilise-movements head chull 10 moves))
      :else moves)))

(defn safe-snake?
  "Check that no part of the snake are on a hazard case."
  [body-params]
  (let [s (-> body-params :you)
        hazards (-> body-params :board :hazards)]
    (not (some #(hazard? % hazards) (:body s)))))

(defn free-space
  "returns the proportion of free space on the board"
  [body-params]
  (let [snakes  (into [] (apply concat (mapv #(:body %) (-> body-params :board :snakes))))
        hazards (-> body-params :board :hazards)
        width (-> body-params :board :width)
        height (-> body-params :board :height)]
    (- 1 (/ (+ (count snakes) (count hazards))
            (* width height)))))

(defn follow-tail
  "Promote x1.4 the possible head projections that decrease the most the distance to the tail. 1-0.7=0.3 < 0.33 = (/40 121) ie 4 lines of hazards. 1.4*0.7 < 1 so that rotation on borders are prohibited if avoidable"
  [body-params moves]
;; (println "safe-snake?: " (safe-snake? (-> body-params :you)))
  (cond
    (< (/ 88 121) (free-space body-params)) moves
    (safe-snake? body-params)
    (let [s (-> body-params :you)
          tail (last (-> s :body))
          head (-> s :head)
          heads (space/project-head s)
          scores (mapv #(d % tail) heads)
          mins (apply min scores)
          bests (filterv #(= mins (d % tail)) heads)]
      (println "FOLLOW-TAIL")
      (space/probabilise-movements head bests 1.4 moves))
    :else moves))

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
  "Pushes the snake back to the barycentre of no hazard food or free space on the board if there's none of these. Does that when I am hungry or head in hazard. 3 so that recenter * hazard * best degree less than eat on food"
  [body-params moves]
  (println "RECENTER")
  (let [hazards (hazard body-params)
        foods (food body-params)
        free-food (filterv #(not (hazard? % hazards)) foods)
        target (if (= [] free-food)
                 (free-space-barycentre body-params)
                 (food-barycentre body-params))
        me (-> body-params :you)
        head (:head me)
        my-health (-> me :health)
        my-length (-> me :length)]
    (cond
      (or (> 45 my-health) ;I am hungry
          (hazard? head hazards)) ;My head is on hazards
      (let [chull (space/convex-hull {:body [head target]})]
        (space/probabilise-movements head chull 3 moves))
      :else moves)))

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


(defn avoid-borders
  "Avoids borders to not get cornered by the please snake. The coefficient used (0.68) has to be better than 0.66, the one for the hazard, otherwise the snake doesn't go the health-optimal way. It has to be smaller than 5*1.4/10 as well."
  [body-params moves]
  (println "AVOID BORDERS")
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
      (hazard? head hazards) moves
      :else (space/probabilise-movements head borders 0.68 moves))))

(defn grade-case
  "Returns a value of degree that a case gives another case. The order is important if a case is many things!!"
  [c obstacles hazards]
  (cond
    (some #(= c %) obstacles) 0
    (hazard? c hazards) 0.4 ;worse than 0.5 because 2 hazards shouldn't be better than just one free case (true only coupled with fear)
    :else 1))

(defn degree
  "Returns the degree of freedom of a case"
  [body-params c]
  (let [me (-> body-params :you)
        hazards (hazard body-params)
        all-squares [(update c :x inc)
                     (update c :x dec)
                     (update c :y inc)
                     (update c :y dec)]
        all-obst (space/all-obstacles body-params me)
        grades (mapv #(grade-case % all-obst hazards) all-squares)]
    (reduce + grades)))

(defn min-d-to-others
  "returns the min distance to heads of others"
  [my-head others-heads]
  (apply min (mapv #(d my-head %) others-heads)))

(defn optionality
  "Favours largest degree of freedom of next case when distance to a larger snake is less or equal to 4 (seems the right distance). 
   Coeff chosen to be peculiar and beat follow tail even on hazard case (3.14*0.5 > 1.4)
   To be applied when there is no way to get further away from the opponent"
  [body-params moves]
  (println "OPTIONALITY")
  (let [me (-> body-params :you)
        head (:head me)
        length (:length me)
        others (filterv #(> (:length %) (+ 1 length)) (space/other-snakes body-params))
        heads (mapv #(:head %) others)
        distances (mapv #(sd me %) heads)]
    (cond
      (some #(>= 4 %) distances)
      (let [pheads (space/project-head me)
            degrees (mapv #(degree body-params %) pheads)
            maxd (apply max degrees)
            bests (filterv #(= maxd (degree body-params %)) pheads)
            maxmind (apply max (mapv #(min-d-to-others % heads) pheads))]
        (cond
              ;4 because I'm not using their projected heads
          (>= 4 maxmind) (do (println "TOO CLOSE TO LARGER SNAKES")
                             (space/probabilise-movements head bests 3.14 moves))
          :else moves))
      :else moves)))

(defn forbidden-part
  "Returns the forbidden part of a chull for FEAR"
  [chull]
  (let [nbx (count (distinct (mapv #(:x %) chull)))
        nby (count (distinct (mapv #(:y %) chull)))]
    (cond
      (or (= nbx 1)
          (= nby 1)) [] ; it's not a problem if it's a line
      (= nbx 4) (let [maxx (apply max (mapv #(:x %) chull))
                      minx (apply min (mapv #(:x %) chull))]
                  (filterv #(> maxx (:x %) minx) chull)) ;this is the 2 by 4 case
      (= nby 4) (let [maxy (apply max (mapv #(:y %) chull))
                      miny (apply min (mapv #(:y %) chull))]
                  (filterv #(> maxy (:y %) miny) chull))
      :else chull ;this is the 3x3 case
      )
    )
  )

(defn fear
  "Discourages getting into the chull made with larger opponents deemed too close (4).
   0.22 carefully crafted to fit other parameters."
  [body-params moves]
  (println "FEAR")
  (let [me (-> body-params :you)
        head (:head me)
        length (:length me)
        others (filterv #(> (:length %) length) (space/other-snakes body-params))
        heads (mapv #(:head %) others)
        distances (mapv #(sd me %) heads)]
    (cond
      (and (some #(>= 4 %) distances)
           (not (or (border? body-params head)
                    (hazard? head (hazard body-params)))))
      (let [dangers (filterv #(>= 4 (sd me %)) heads)
                                       chulls (mapv #(forbidden-part (space/convex-hull {:body [head %]})) dangers)
                                       total (vec (apply concat chulls))]
                                   (space/probabilise-movements head total 0.22 moves))
      :else moves)))

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

(defn find-closest-free-case
  "Favours the chull of head closest-free-case"
  [body-params moves]
  (println "FIND-CLOSEST-FREE-CASE")
  (let [me (-> body-params :you)
        body (-> me :body)
        head (-> me :head)
        hazards (hazard body-params)
        all-obst (space/all-obstacles body-params me)
        ]
    (loop [body (vec (rest body))]
      (cond
        (not (hazard? head hazards)) moves
        (= [] body) moves
        (= [] (free-cases (first body) all-obst hazards)) (recur (vec (rest body)))
        :else (let [f (first (free-cases (first body) all-obst hazards))
                    chull (space/convex-hull {:body [head f]})]
                (space/probabilise-movements head chull 2.55 moves)
                )
        ))))