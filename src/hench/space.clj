(ns hench.space
  (:require clojure.set
            [hench.utils :refer :all]))

(def sample {:game {:id "game-00fe20da-94ad-11ea-bb37"
                    :ruleset {:name "standard"
                              :version "v.1.2.3"}
                    :timeout 500}
             :turn 14
             :board {:height 11
                     :width 11
                     :food [{:x 0, :y 2}
                            {:x 2, :y 0}
                            {:x 5, :y 5}
                            {:x 9, :y 0}
                            {:x 2, :y 6}]
                     :hazards [{:x 0, :y 3}
                               {:x 0, :y 1}
                               {:x 3, :y 2}]
                     :snakes [{:id "snake-508e96ac-94ad-11ea-bb37"
                               :name "My Snake"
                               :health 54
                               :body [{:x 0, :y 1}
                                      {:x 1, :y 1}
                                      {:x 2, :y 1}
                                      {:x 2, :y 0}
                                      {:x 3, :y 0}]
                               :latency "111"
                               :head {:x 0, :y 1}
                               :length 5
                               :shout "why are we shouting??"
                               :squad ""}
                              {:id "snake-b67f4906-94ae-11ea-bb37"
                               :name "Another Snake"
                               :health 56
                               :body [{:x 5, :y 4}
                                      {:x 5, :y 3}
                                      {:x 6, :y 3}
                                      {:x 6, :y 2}]
                               :latency "222"
                               :head {:x 5, :y 4}
                               :length 4
                               :shout "I'm not really sure..."
                               :squad ""}]}
             :you {:id "snake-508e96ac-94ad-11ea-bb37"
                   :name "My Snake"
                   :health 54
                   :body [{:x 0, :y 1}
                          {:x 1, :y 1}
                          {:x 2, :y 1}
                          {:x 2, :y 0}
                          {:x 3, :y 0}]
                   :latency "111"
                   :head {:x 0, :y 1}
                   :length 5
                   :shout "why are we shouting??"
                   :squad ""}})

(defn random-move
  "A random move"
  [moves]
  (if (= moves {})
    "up"
    (rand-nth (mapv #(name %) (keys moves)))))

(defn filter-map
  "Returns a map with only the [k a] where a = v"
  [m v]
  (loop [ks (vec (keys m))
         res {}]
    (cond
      (= ks []) res
      (= ((first ks) m) v) (recur (vec (rest ks))
                                  (assoc res (first ks) v))
      :else (recur (vec (rest ks))
                   res))))

(defn choose-move
  "The best scoring move - if many, a random pick"
  [body-params moves]
  (println "CHOOSE MOVE")
  (clojure.pprint/pprint (str "turn: " (:turn body-params) " " moves))
  (clojure.pprint/pprint (str "health: " (-> body-params :you :health)))
  (if (= moves {})
    "up"
    (let [max (apply max (vals moves))
          fmoves (filter-map moves max)]
      (random-move fmoves))))

(defn avoid-walls
  "Avoids direct hits to the walls of the board"
  [body-params moves]
  (println "AVOID WALLS")
  (let [xmax (- (-> body-params :board :width) 1)
        ymax (- (-> body-params :board :height) 1)
        xhead (-> body-params :you :head :x)
        yhead (-> body-params :you :head :y)]
    (cond-> moves
      (= xmax xhead) (update :right #(* % 0))
      (= ymax yhead) (update :up #(* % 0))
      (= 0 xhead) (update :left #(* % 0))
      (= 0 yhead) (update :down #(* % 0)))))

(defn avoid-self-direct-hits
  "Avoids colliding with itself on the next move"
  [body-params moves]
  (println "AVOID SELF DIRECT HITS")
  (let [body (vec (butlast (-> body-params :you :ndbody))) ;the tail is ok, hence butlast
        head (-> body-params :you :head)
        board (:board body-params)
        width (:width board)
        height (:height board)]
    (probabilise-movements head (wrap-multiply body width height) 0 moves)))

(defn avoid-other-snakes
  "Avoids direct hits with other snakes"
  [body-params moves]
  (println "AVOID OTHER SNAKES")
  (let [snakes (other-snakes body-params)
        my-length (-> body-params :you :length)
        head (-> body-params :you :head)
        board (:board body-params)
        width (:width board)
        height (:height board)
        projected-heads
        (into [] (apply concat
                        (mapv #(if (< (:length %) my-length)
                                 []
                                 (project-head % width height)) snakes))) ;only project heads if the snake is more healthy or equally healthy
        all-bodies (into [] (apply concat (mapv #(vec (butlast (:body %))) snakes))) ;the tail is ok - corner case to be added if the head is close to food though
        ]
      (println "projected-heads: " projected-heads)
    (->> moves
         (probabilise-movements head all-bodies 0)
         (probabilise-movements head projected-heads 0.1))))

(defn elbow?
  "Returns true if n is the index in body of an elbow, false otherwise"
  [n body]
  (let [c1 (nth body (- n 1))
        c2 (nth body n)
        c3 (nth body (+ n 1))]
    (and
     (not= (:x c1) (:x c2) (:x c3))
     (not= (:y c1) (:y c2) (:y c3)))))

(defn elbows
  "Returns a vector of all the elbows in snake s"
  [s]
  (let [trunk (vec (rest (butlast (:body s)))); withoutheadortail
        body (:body s)]
    (filterv #(elbow? % body) (range 1 (+ (count trunk) 1)))))

(defn direction
  "Returns the direction of the elbow, run through from head to tail"
  [elbow body]
  (substract (nth body (+ elbow 1)) (nth body elbow)))

(defn sbody
  "Returns the sbody of a snake"
  [s]
  (let [elbows (elbows s)]
    (cond
      (= [] elbows) []
      :else (subvec s (first elbows)))))

(defn close?
  "Returns true if c1 touches c2"
  [c1 c2]
  (= 1 (+
        (Math/abs (- (:x c1) (:x c2)))
        (Math/abs (- (:y c1) (:y c2))))))

(defn adjacent?
  "Returns true if we could be in an imminent self crash at case c"
  [head neck c]
  (and (not= c neck)
       (close? head c)))

(defn inverse
  "Returns the inverse of a vector, or a point"
  [{:keys [x y]}]
  {:x y :y x})

(defn danger-cases
  "Returns a vector of the three danger cases"
  [head neck]
  (let [diff (substract head neck)
        ortho (inverse diff)
        front (add head diff)
        c1 (add front ortho)
        c2 (substract front ortho)]
    (vec (distinct [c1 front c2]))))

(defn self-danger?
  "Returns true if one of the danger-cases is you"
  [body-params]
  (let [body (-> body-params :you :body)
        ndbody (-> body-params :you :ndbody)
        head (-> body-params :you :head)
        neck (neck ndbody)
        danger-cases (danger-cases head neck)]
    (and (> (count danger-cases) 1)
         (not= #{} (clojure.set/intersection (set danger-cases)
                                             (set body))))))

(defn snake?
  "Returns true if the case is part of you"
  [c s]
  (let [body (:body s)]
    (some #(= c %) body)))

(defn same-x
  "Returns the points in obstacles with the same x as c"
  [c obstacles]
  (filterv #(= (:x c) (:x %)) obstacles))

(defn same-y
  "Returns the points in obstacles with the same y as c"
  [c obstacles]
  (filterv #(= (:y c) (:y %)) obstacles))

(defn bounded-in-x?
  "Returns true if c is bounded in x by cases in obstacles"
  [c obstacles]
  (let [obst (same-y c obstacles)]
    (and (some #(< (:x c) (:x %)) obst)
         (some #(> (:x c) (:x %)) obst))))

(defn bounded-in-y?
  "Returns true if c is bounded in x by cases in obstacles"
  [c obstacles]
  (let [obst (same-x c obstacles)]
    (and (some #(< (:y c) (:y %)) obst)
         (some #(> (:y c) (:y %)) obst))))

(defn bounded?
  "Returns true if c is bounded in both x and y by obstacles"
  [c obstacles]
  (and (bounded-in-x? c obstacles)
       (bounded-in-y? c obstacles)))

(defn avoid-self-loop
  "Anticipates self-loops and helps avoid them"
  [body-params moves]
  (println "AVOID-SELF-LOOP")
  (let [s (-> body-params :you)
        body (-> s :body)
        ndbody (-> s :ndbody) ;non duplicated body
        head (head ndbody)
        neck (neck ndbody)]
  ;(println "self-danger: " (self-danger? s))
  ;(println "danger-cases (selfloop): " (danger-cases head neck))
    (if (self-danger? body-params)
      (let [chull (convex-hull s)
            not-snake (filterv #(not (snake? % s)) chull)
            danger-cases (danger-cases head neck)
            vi (mapv #(.indexOf body %) danger-cases)
            i (apply max (filterv pos? vi)) ;closest to tail, using pos? is fine as head (0) is never a danger case
            subbody (subvec body 0 (+ i 1))
            bounded (filterv #(bounded? % subbody) not-snake)]
          ;(println "subbody (selfloop): " subbody)
          ;(println "not-snake (selfloop): " not-snake)
          ;(println "bounded (selfloop): " bounded)
        (probabilise-movements head bounded 0.009 moves))
      moves)))

(defn wall-danger?
  "Returns true if the face is facing a wall"
  [s board]
  (let [head (head (-> s :body))
        neck (neck (-> s :body))
        diff (substract head neck)
        front (add head diff)
        xmax (-> board :width)
        ymax (-> board :height)]
    (or (= (:x front) xmax)
        (= (:y front) ymax)
        (= (:x head) 0)
        (= (:y head) 0))))

(defn next-to-wall
  "Returns true if c is next to a wall"
  [c board]
  (let [xmax (-> board :width)
        ymax (-> board :height)]
    (or (= (:x c) (- xmax 1))
        (= (:x c) 0)
        (= (:y c) (- ymax 1))
        (= (:y c) 0))))

(defn xray
  "Filters the cells of body and keeps them if they are close to a wall"
  [s board]
  (let [body (-> s :body)]
    (filterv #(next-to-wall % board) body)))

(defn avoid-loop-with-walls
  "Anticipates loops with walls"
  [body-params moves]
  (println "AVOID LOOP WITH WALLS")
  (let [s (-> body-params :you)
        body (-> s :body)
        board (-> body-params :board)]
  ;(println "wall-danger: " (wall-danger? s board))
    (if (not (wall-danger? s board))
      moves
      (let [xray (xray s board)]
      ;(println "xray: " xray)
        (if (= [] xray)
          moves
          (let [l (last xray) ;the one closest to the tail
                i (.indexOf body l)
                subbody (subvec body 0 (+ i 1))
                chull (convex-hull s)
                not-snake (filterv #(not (snake? % s)) chull)
                walls (touched-walls subbody board)
                bounded (filterv #(bounded? % (vec (concat subbody walls))) not-snake)]
          ;(println "subbody: " subbody)
          ;(println "Wall loop danger true!")
          ;(println "bounded (wallloop): " bounded)
            (probabilise-movements (head body) bounded 0.8 moves)))))))

(defn avoid-hazards
  "Avoids hazards as much as it makes sense"
  [body-params moves]
  (println "AVOID HAZARDS")
  (let [hazards (-> body-params :board :hazards)
        health (-> body-params :you :health)
        head (-> body-params :you :head)]
    (cond
      (> 75.0 health) (probabilise-movements head hazards 0.66 moves)
      :else (probabilise-movements head hazards 0.5 moves))))

(defn diagonal?
  "Returns true if c1 and c2 are touching diagonally"
  [c1 c2]
  (and (= 1 (Math/abs (- (:x c1) (:x c2)))
          (= 1 (Math/abs (- (:y c1) (:y c2)))))))

(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn snaky?
  "Returns the snake c belongs to, or false if not a snake"
  [c snakes]
  (let [bodies (mapv #(:body %) snakes)
        reveal (mapv #(some (fn [x] (= c x)) %) bodies)
        i (first (indices true? reveal))]
    (cond
      (= i nil) false
      :else (nth snakes i))))

(defn get-segment
  "Returns the vector of free cases from c in the direction of vector (et son sens) until a forbidden case is reached"
  [c direction forbidden]
  (loop [c (add c direction)
         res []]
    (cond
      (some #(= c %) forbidden) res
      :else (recur (add c direction)
                   (conj res c)))))

(defn opposite
  "Returnst the opposite of a unit direction vector"
  [direction]
  (-> direction
      (update-in [:x] #(* -1 %))
      (update-in [:y] #(* -1 %))))

(defn get-line
  "Applies get-segment but in both directions"
  [c direction forbidden]
  (into [] (concat (get-segment c direction forbidden)
                   (get-segment c (opposite direction) forbidden))))

(defn perpendicular
  "Returns two unit vectors, both perpendiculars to direction"
  [direction]
  (cond
    (= 0 (:x direction)) [{:x 1 :y 0} {:x -1 :y 0}]
    (= 0 (:y direction)) [{:x 0 :y 1} {:x 0 :y -1}]
    :else "Please enter a reduced direction (one with a coordinate equal to 0"))

(defn expand-case
  "Returns the cases next to c that are not obstacles"
  [c obs w h]
  (let [res [(update c :x #(mod (inc %) w))
             (update c :x #(mod (dec %) w))
             (update c :y #(mod (inc %) h))
             (update c :y #(mod (dec %) h))]]
    (filterv #(not-obstacle? % obs) res)))

(defn expand-cases
  "Returns the unique cases next to all cs of v that are not obstacles"
  [v obs w h]
  (vec (distinct (apply concat (mapv #(expand-case % obs w h) v)))))

(defn surface
  "Returns the cases that are reachable from c, and only length s + 1 if there are more."
  [c s obstacles w h]
  (let [l (:length s)]
    (if (some #(= c %) obstacles)
      []
      (loop [v [c]
             forbidden (conj obstacles c)
             res [c]]
        (let [expc (expand-cases v forbidden w h)]
          (cond
             ; if I have enough, return the ones already found
            (> (count res) l) res
             ; if I can't find new ones, return the ones already found
            (= expc []) res
          ; otherwise, iterate with a new v and forbidden augmented of new v
            :else (recur expc
                         (vec (concat forbidden expc))
                         (vec (concat res expc)))))))))

#_(defn surface
  "Returns the vector of cases reachable from c"
  [c s obstacles]
  (if (some #(= c %) obstacles)
    []
    (loop [v [c]
           direction (substract c (:head s))
           forbidden obstacles]
       ;(println "direction: " direction)
      (let [res (apply concat (mapv #(get-line % direction forbidden) v))]
        (cond
          (> (count (into [] (distinct v))) (:length s)) (into [] (distinct v))
          (and (= res [])
               (= [c] v)) (recur [c c]
                                 (first (perpendicular direction))
                                 forbidden)
          (= res []) (into [] (distinct v))
          :else (recur (into [] (concat v res))
                       (first (perpendicular direction))
                       (into [] (concat forbidden res))))))))

(defn contains-tail
  "Returns true if surface contains snake's tail"
  [body-params surface]
  (let [me (-> body-params :you)
        tail (last (:body me))]
    (some #(close? % tail) surface)))

(defn avoid-small-surfaces
  "Avoids surfaces smaller than snake (don't put 0 as it still is better than a wall!)"
  [body-params moves]
  (println "AVOID-SMALL-SURFACES")
  (let [me (-> body-params :you)
        w (-> body-params :board :width)
        h (-> body-params :board :height)
        head (:head me)
        length (:length me)
        all-obs (all-obstacles body-params me)]
    (cond-> moves
      (and (> length (count (surface (update head :x #(mod (inc %) w)) me all-obs w h)))
           (not (contains-tail body-params (surface (update head :x #(mod (inc %) w)) me all-obs w h)))) (update :right #(* 0.009 %))
      (and (> length (count (surface (update head :x #(mod (dec %) w)) me all-obs w h)))
           (not (contains-tail body-params (surface (update head :x #(mod (dec %) w)) me all-obs w h)))) (update :left #(* 0.009 %))
      (and (> length (count (surface (update head :y #(mod (inc %) h)) me all-obs w h)))
           (not (contains-tail body-params (surface (update head :y #(mod (inc %) h)) me all-obs w h)))) (update :up #(* 0.009 %))
      (and (> length (count (surface (update head :y #(mod (dec %) h)) me all-obs w h)))
           (not (contains-tail body-params (surface (update head :y #(mod (dec %) h)) me all-obs w h)))) (update :down #(* 0.009 %)))))

(defn favour-straight-line
  "Favours going on a straight line over turning when two moves have same probability.
   It helps to have a way back for my snake"
  [body-params moves]
  (let [values (vec (vals moves))
        maxv (apply max values)]
    (cond
      (= 1 (count (filterv #(= maxv %) values))) moves
      :else (let [me (-> body-params :you)
                  head (-> me :head)
                  neck (neck (-> me :body))
                  diff (substract head neck)
                  straight (add head diff)]
              (probabilise-movements head [straight] 1.1 moves)))))

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
          board (:board body-params)
          width (:width board)
          height (:height board)
          heads (project-head s width height)
          scores (mapv #(d % tail width height) heads)
          mins (apply min scores)
          bests (filterv #(= mins (d % tail width height)) heads)]
      (println "FOLLOW-TAIL")
      (probabilise-movements head bests 1.4 moves))
    :else moves))

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
      :else (probabilise-movements head borders 0.68 moves))))

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
        all-obst (all-obstacles body-params me)
        grades (mapv #(grade-case % all-obst hazards) all-squares)]
    (reduce + grades)))

(defn min-d-to-others
  "returns the min distance to heads of others"
  [my-head others-heads w h]
  (apply min (mapv #(d my-head % w h) others-heads)))

(defn optionality
  "Favours largest degree of freedom of next case when distance to a larger snake is less or equal to 4 (seems the right distance). 
   Coeff chosen to be peculiar and beat follow tail even on hazard case (3.14*0.5 > 1.4)
   To be applied when there is no way to get further away from the opponent"
  [body-params moves]
  (println "OPTIONALITY")
  (let [board (:board body-params)
        width (:width board)
        height (:height board)
        me (-> body-params :you)
        head (:head me)
        length (:length me)
        others (filterv #(> (:length %) (+ 1 length)) (other-snakes body-params))
        heads (mapv #(:head %) others)
        distances (mapv #(sd me % width height) heads)]
    (cond
      (some #(>= 4 %) distances)
      (let [pheads (project-head me width height)
            degrees (mapv #(degree body-params %) pheads)
            maxd (apply max degrees)
            bests (filterv #(= maxd (degree body-params %)) pheads)
            maxmind (apply max (mapv #(min-d-to-others % heads width height) pheads))]
        (cond
              ;4 because I'm not using their projected heads
          (>= 4 maxmind) (do (println "TOO CLOSE TO LARGER SNAKES")
                             (probabilise-movements head bests 3.14 moves))
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
      )))

(defn fear
  "Discourages getting into the chull made with larger opponents deemed too close (4).
   0.22 carefully crafted to fit other parameters."
  [body-params moves]
  (println "FEAR")
  (let [me (-> body-params :you)
        w (-> body-params :board :width)
        h (-> body-params :board :height)
        head (:head me)
        length (:length me)
        others (filterv #(> (:length %) length) (other-snakes body-params))
        heads (mapv #(:head %) others)
        distances (mapv #(sd me % w h) heads)]
    (cond
      (and (some #(>= 4 %) distances)
           (not (or false #_(border? body-params head)
                    (hazard? head (hazard body-params)))))
      (let [dangers (filterv #(>= 4 (sd me % w h)) heads)
            chulls (mapv #(forbidden-part (convex-hull {:body [head %]})) dangers)
            total (vec (apply concat chulls))]
        (probabilise-movements head total 0.22 moves))
      :else moves)))

(defn find-closest-free-case
  "Favours the chull of head closest-free-case"
  [body-params moves]
  (println "FIND-CLOSEST-FREE-CASE")
  (let [me (-> body-params :you)
        body (-> me :body)
        head (-> me :head)
        hazards (hazard body-params)
        all-obst (all-obstacles body-params me)]
    (loop [body (vec (rest body))]
      (cond
        (not (hazard? head hazards)) moves
        (= [] body) moves
        (= [] (free-cases (first body) all-obst hazards)) (recur (vec (rest body)))
        :else (let [f (first (free-cases (first body) all-obst hazards))
                    chull (convex-hull {:body [head f]})]
                (probabilise-movements head chull 1.55 moves))))))

(defn wrapped-mode
  "Duplicates the board on each side of the initial board, but not any head. 
   They are duplicated directly in the projected-head fn"
  [body-params]
  (let [board (:board body-params)
        width (:width board)
        height (:height board)]
    (-> body-params
        (assoc-in [:you :ndbody] (-> body-params :you :body))
        (update-in [:board :food] #(wrap-multiply % width height))
        (update-in [:board :hazards] #(wrap-multiply % width height))
        (update-in [:you :body] #(wrap-multiply % width height))
        (update-in [:board :snakes] #(update-snakes % width height)))))
