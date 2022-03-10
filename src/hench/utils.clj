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
  [c1 c2 w h]
  {:x (mod (+ (:x c1) (:x c2)) w) :y (mod (+ (:y c1) (:y c2)) h)})

(defn substract
  "Vectorial substraction"
  [c1 c2 w h]
  {:x (mod (- (:x c1) (:x c2)) w) :y (mod (- (:y c1) (:y c2)) h)})

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
  [; initial board
   p
   ; right
   (add p {:x width :y 0} width height)
   ; left
   (add p {:x (- width) :y 0} width height)
   ; top
   (add p {:x 0 :y height} width height)
   ; bottom
   (add p {:x 0 :y (- height)} width height)
   ; top right
   (add p {:x width :y height} width height)
   ; top left
   (add p {:x (- width) :y height} width height)
   ; bottom right
   (add p {:x width :y (- height)} width height)
   ; bottom left
   (add p {:x (- width) :y (- height)} width height)])

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
        all-squares [(update head :x #(mod (inc %) w))
                     (update head :x #(mod (dec %) w))
                     (update head :y #(mod (inc %) h))
                     (update head :y #(mod (dec %) h))] ;all the squares around the head
        ]
    (vec (remove #(= % neck) all-squares))))

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

(defn not-obstacle?
  [c obs]
  (not (some #(= c %) obs)))

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

(defn included?
  "Returns true if v is included in s, false otherwise"
  [v s]
  (= (count v)
     (count (into [] (clojure.set/intersection
                      (set v)
                      (set s))))))

(defn convex-hull
  "Returns the convex hull of a vector of cells, ie the smallest of all
   4 possible surfaces containing all points of v.
   This one sort of works, but fails to capture the smallest possible surface containing all points
   because I only consider min and max, whereas if I add a diagonal point, it should sometimes act as the max...
   see (convex-hull [{:x 0 :y 0} {:x 2 :y 2} {:x 10 :y 10}]) for instance..."
  [v]
  (let [xmin (apply min (mapv #(:x %) v))
        ymin (apply min (mapv #(:y %) v))
        xmax (apply max (mapv #(:x %) v))
        ymax (apply max (mapv #(:y %) v))
        ;direct x and direct y
        s1 (vec (for [x (vec (range xmin (+ xmax 1)))
                      y (vec (range ymin (+ ymax 1)))]
                  {:x (mod x 11) :y (mod y 11)}))
        ;direct x and indirect y
        s2 (vec (for [x (vec (range xmin (+ xmax 1)))
                      y (vec (range ymax (+ ymin 11 1)))]
                  {:x (mod x 11) :y (mod y 11)}))
        ;indirect x and direct y
        s3 (vec (for [x (vec (range xmax (+ xmin 11 1)))
                      y (vec (range ymin (+ ymax 1)))]
                  {:x (mod x 11) :y (mod y 11)}))
        ;indirect x and indirect y
        s4 (vec (for [x (vec (range xmax (+ xmin 11 1)))
                      y (vec (range ymax (+ ymin 11 1)))]
                  {:x (mod x 11) :y (mod y 11)}))]
    (first (sort-by count (filterv #(included? v %) [s1 s2 s3 s4])))))

(defn d
  "Mathematical distance without obstacles.
   Not symmetric. a is snake, b is food or target, w is board's width, h is board's height."
  [a b w h]
  #_(+ (Math/abs (- (:x a) (:x b)))
             (Math/abs (- (:y a) (:y b))))
  (apply min
         [; initial board
          (+ (Math/abs (- (:x a) (:x b)))
             (Math/abs (- (:y a) (:y b)))) 
          ; to the right
          (+ (Math/abs (- (:x a) (+ (:x b) w)))
             (Math/abs (- (:y a) (:y b)))) 
          ; to the left
          (+ (Math/abs (- (:x a) (- (:x b) w)))
             (Math/abs (- (:y a) (:y b)))) 
          ; to the top
          (+ (Math/abs (- (:x a) (:x b)))
             (Math/abs (- (:y a) (+ (:y b) h))))
         ; to the bottom 
          (+ (Math/abs (- (:x a) (:x b)))
             (Math/abs (- (:y a) (- (:y b) h))))
         ; to the top right 
          (+ (Math/abs (- (:x a) (+ (:x b) w)))
             (Math/abs (- (:y a) (+ (:y b) h))))
         ; to the top left 
          (+ (Math/abs (- (:x a) (- (:x b) w)))
             (Math/abs (- (:y a) (+ (:y b) h)))) 
          ; to the bottom right
          (+ (Math/abs (- (:x a) (+ (:x b) w)))
             (Math/abs (- (:y a) (- (:y b) h)))) 
          ; to the bottom left
          (+ (Math/abs (- (:x a) (- (:x b) w)))
             (Math/abs (- (:y a) (- (:y b) h)))) 
          ]
         ))

(defn sd
  "Snake distance to a point without obstacles"
  [s c w h]
  (let [head (-> s :head)
        neck (second (-> s :body))
        d (d head c w h)]
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

(defn max-hazard-in-x
  "Returns the maximum number of cases in the x direction that are hazards"
  [chull hazards]
  (cond
    (= chull []) 0
    :else
    (let [xmin (apply min (mapv #(:x %) chull))
          xmax (apply max (mapv #(:x %) chull))]
      (loop [x xmin
             res 0]
        (let [c (count (filterv #(hazard? % hazards) (filterv (fn [a] (= x (:x a))) chull)))]
          (cond
            (= x (+ xmax 1)) res
            :else (recur (+ x 1)
                         (max res c))))))))

(defn min-hazard-in-y
  "Returns the minimum number of cases in the y direction that are hazards"
  [chull hazards]
  (cond
    (= chull []) 0
    :else
    (let [ymin (apply min (mapv #(:y %) chull))
          ymax (apply max (mapv #(:y %) chull))]
      (loop [y ymin
             res (count chull) ;this initial value doesn't matter as long as it's higher than c anyway
             ]
        (let [c (count (filterv #(hazard? % hazards) (filterv (fn [a] (= y (:y a))) chull)))]
          (cond
            (= y (+ ymax 1)) res
            (= c 0) 0
            :else (recur (+ y 1)
                         (min res c))))))))

(defn max-hazard-in-y
  "Returns the maximum number of cases in the y direction that are hazards"
  [chull hazards]
  (cond
    (= chull []) 0
    :else
    (let [ymin (apply min (mapv #(:y %) chull))
          ymax (apply max (mapv #(:y %) chull))]
      (loop [y ymin
             res 0]
        (let [c (count (filterv #(hazard? % hazards) (filterv (fn [a] (= y (:y a))) chull)))]
          (cond
            (= y (+ ymax 1)) res
            :else (recur (+ y 1)
                         (max res c))))))))

(defn hd
  "Health distance, ie taking hazards into account.
  Not sure I still need to add my own body to the hazards, but works for now.
   Because of the shape of the sauce, the amount of cells in the sauce we travel
   is (min (minx + maxy) (miny+maxx)) and then minus the usual 14pts in different cases"
  [s c hazards w h]
  (let [head (-> s :head)
        hazards+body (vec (concat hazards (vec (rest (:body s)))))
        chull (convex-hull [head c])
        min-in-x (min-hazard-in-x chull hazards+body)
        min-in-y (min-hazard-in-y chull hazards+body)
        max-in-x (max-hazard-in-x chull hazards+body)
        max-in-y (max-hazard-in-y chull hazards+body)
        ddist (sd s c w h)
        minsum (min (+ min-in-x max-in-y)
                    (+ min-in-y max-in-x))
        temp-res (+ ddist
                    (* minsum 14))]
    (cond-> temp-res
      (and (< 0 min-in-x) (< 0 min-in-y)) (- 14)
      (hazard? head hazards) (- 14)
      (hazard? c hazards) (- 14))))

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
  [head obstacles f moves w h]
  (let [new-moves (cond-> moves
                    (some #(= (update head :x (fn [a] (mod (inc a) w))) %) obstacles) (update :right #(* f %))
                    (some #(= (update head :x (fn [a] (mod (dec a) w))) %) obstacles) (update :left #(* f %))
                    (some #(= (update head :y (fn [a] (mod (inc a) h))) %) obstacles) (update :up #(* f %))
                    (some #(= (update head :y (fn [a] (mod (dec a) h))) %) obstacles) (update :down #(* f %)))]
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
                   (obstacles body-params s) ;all the other snake's projected bodies
                   #_(all-walls (-> body-params :board :height)
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