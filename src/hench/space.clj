(ns hench.space
  (:require clojure.set))

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



(defn restrict-movements
  [head obstacles moves]
  (cond-> moves
    (some #(= (update head :x inc) %) obstacles) (dissoc :right)
    (some #(= (update head :x dec) %) obstacles) (dissoc :left)
    (some #(= (update head :y inc) %) obstacles) (dissoc :up)
    (some #(= (update head :y dec) %) obstacles) (dissoc :down)))

(defn probabilise-movements
  [head obstacles f moves]
  (let [new-moves (cond-> moves
                    (some #(= (update head :x inc) %) obstacles) (update :right #(* f %))
                    (some #(= (update head :x dec) %) obstacles) (update :left #(* f %))
                    (some #(= (update head :y inc) %) obstacles) (update :up #(* f %))
                    (some #(= (update head :y dec) %) obstacles) (update :down #(* f %)))]
    (println new-moves)
    new-moves))


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
  (let [body (vec (butlast (-> body-params :you :body))) ;the tail is ok, hence butlast
        head (-> body-params :you :head)]
    (probabilise-movements head body 0 moves)))

(defn project-head
  "Returns a vector of all the potential head locations of snake s"
  [s]
  (let [body (:body s)
        head (:head s)
        neck (nth body 1) ;the square before the head
        all-squares [(update head :x inc)
                     (update head :x dec)
                     (update head :y inc)
                     (update head :y dec)] ;all the squares around the head
        ]
    (vec (remove #(= % neck) all-squares))))

(defn other-snakes
  "Returns only the other snakes (I shouldn't project my own head!)"
  [body-params]
  (let [snakes (-> body-params :board :snakes)
        me (-> body-params :you)]
    (filterv #(not= me %) snakes)))

(defn avoid-other-snakes
  "Avoids direct hits with other snakes"
  [body-params moves]
  (println "AVOID OTHER SNAKES")
  (let [snakes (other-snakes body-params)
        my-length (-> body-params :you :length)
        head (-> body-params :you :head)
        projected-heads
        (into [] (apply concat
                        (mapv #(if (< (:length %) my-length)
                                 []
                                 (project-head %)) snakes))) ;only project heads if the snake is more healthy or equally healthy
        all-bodies (into [] (apply concat (mapv #(vec (butlast (:body %))) snakes))) ;the tail is ok - corner case to be added if the head is close to food though
        ]
      ;(println "projected-heads: " projected-heads)
    (->> moves
         (probabilise-movements head all-bodies 0)
         (probabilise-movements head projected-heads 0.1))))

(defn add
  "Vectorial addition"
  [c1 c2]
  {:x (+ (:x c1) (:x c2)) :y (+ (:y c1) (:y c2))})

(defn substract
  "Vectorial substraction"
  [c1 c2]
  {:x (- (:x c1) (:x c2)) :y (- (:y c1) (:y c2))})

(defn obstacles
  "Returns all the obstacles on the board game in one vector : namely all the other snakes's projected bodies (ie no tail but all projected heads"
  [body-params s]
  (let [all-snakes (-> body-params :board :snakes)
        snakes (vec (remove #(= s %) all-snakes))
        length (-> s :length)
        head (-> s :head)
        projected-heads
        (into [] (apply concat
                        (mapv #(if (< (:length %) length)
                                 []
                                 (project-head %)) snakes))) ;only project heads if the snake is more healthy or equally healthy
        all-bodies (into [] (apply concat (mapv #(vec (butlast (:body %))) snakes))) ;the tail is ok - corner case to be added if the head is close to food though
        ]
    (into [] (concat all-bodies projected-heads))))

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

(defn head<->body?
  "Returns true if head is adjacent to body (accounting for the neck obv)"
  [body]
  (some #(adjacent? (head body) (neck body) %) (vec (rest (rest body)))))

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
  [s]
  (let [body (:body s)
        head (head body)
        neck (neck body)
        danger-cases (danger-cases head neck)]
    (and (> (count danger-cases) 1)
         (not= #{} (clojure.set/intersection (set danger-cases)
                                             (set body))))))

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

(defn avoid-self-loop
  "Anticipates self-loops and helps avoid them"
  [body-params moves]
  (println "AVOID-SELF-LOOP")
  (let [s (-> body-params :you)
        body (-> s :body)
        head (head body)
        neck (neck body)]
  ;(println "self-danger: " (self-danger? s))
  ;(println "danger-cases (selfloop): " (danger-cases head neck))
    (if (self-danger? s)
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

(defn surface
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
        head (:head me)
        length (:length me)
        all-obs (all-obstacles body-params me)]
    (cond-> moves
      (and (> length (count (surface (update head :x inc) me all-obs)))
           (not (contains-tail body-params (surface (update head :x inc) me all-obs)))) (update :right #(* 0.009 %))
      (and (> length (count (surface (update head :x dec) me all-obs)))
           (not (contains-tail body-params (surface (update head :x dec) me all-obs)))) (update :left #(* 0.009 %))
      (and (> length (count (surface (update head :y inc) me all-obs)))
           (not (contains-tail body-params (surface (update head :y inc) me all-obs)))) (update :up #(* 0.009 %))
      (and (> length (count (surface (update head :y dec) me all-obs)))
           (not (contains-tail body-params (surface (update head :y dec) me all-obs)))) (update :down #(* 0.009 %)))))







