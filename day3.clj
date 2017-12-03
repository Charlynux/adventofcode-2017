(def moves [:right :up :left :down])

(def moves-fn 
    {:right #(vector %1 (inc %2))
    :up #(vector (inc %1) %2)
    :left #(vector %1 (dec %2))
    :down #(vector (dec %1) %2)})

(assert (= (apply (:right moves-fn) [0 0]) [0 1]))
(assert (= (apply (:up moves-fn) [0 1]) [1 1]))
(assert (= (apply (:left moves-fn) [1 1]) [1 0]))
(assert (= (apply (:down moves-fn) [1 -1]) [0 -1]))

(defn move-after [move]
    (get moves 
        (mod 
            (inc (.indexOf moves move)) 
            (count moves))))

(defn next-move [[x y] prev-move]
        (cond
            (and (not= :right prev-move) (= (Math/abs x) (Math/abs y))) (move-after prev-move)
            (and (= :right prev-move) (= (+ y x) 1)) (move-after prev-move)
                :else prev-move))

(assert (= (next-move [0 0] :right) :right))
(assert (= (next-move [0 1] :right) :up))
(assert (= (next-move [1 1] :up) :left))

(def init-state {:position [0 0] :prev-move :right :value 1})

(defn reducer [prev]
    (let [move (next-move (:position prev) (:prev-move prev))]
        {
            :position (apply (move moves-fn) (:position prev))
            :prev-move move
            :value (inc (:value prev))
        }
    ))

(defn find-position [number]
    (->>
        (iterate reducer init-state)
        (take-while #(<= (:value %) number))
        last
        :position
    ))
(find-position 13)

; (assert (= (find-position 1) [0 0]))

; d = |a - x| + |b - y|
(defn manhattan-distance [[a b] [x y]]
    (let [abs-sub #(Math/abs (- %1 %2))]
        (+ (abs-sub a x) (abs-sub b y))))

(defn distance-to-start [position]
        (manhattan-distance [0 0] position))

(defn calculate-distance [number]
    (distance-to-start (find-position number)))

(assert (= (calculate-distance 1) 0))
(assert (= (calculate-distance 12) 3))
(assert (= (calculate-distance 23) 2))
(assert (= (calculate-distance 1024) 31))

(calculate-distance 368078)


(defn is-neighbor [[x y]]
    (let [diff-is-one #(= (Math/abs (- %1 %2))  1)
            diff-is-zero #(= (Math/abs (- %1 %2))  0)]
        (fn [{ [a b] :position }] 
            (or 
                (and (diff-is-one a x) (diff-is-one b y))
                (and (diff-is-zero a x) (diff-is-one b y))
                (and (diff-is-one a x) (diff-is-zero b y))
            ))))

(defn find-neighbors [position coll]
    (filter (is-neighbor position) coll))

(defn neighbor-reducer [acc value]
    (let [  prev (last acc)
            move (next-move (:position prev) (:prev-move prev))
            next-position (apply (move moves-fn) (:position prev))
            neighbors (find-neighbors next-position acc)]
        (conj acc {
            :position next-position
            :prev-move move
            :value (apply + (map :value neighbors))
        })
    ))

(defn find-next-value-neighbors [number]
    (let [reducer-until 
            #(if (> (:value (last %1)) number) 
                (reduced %1)
                (neighbor-reducer %1 %2))]
        (->>
            (range)
            (reduce reducer-until [init-state])
            last
            :value
        )))

(find-next-value-neighbors 3)
(find-next-value-neighbors 368078)
