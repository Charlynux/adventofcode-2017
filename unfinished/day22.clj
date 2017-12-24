(require '[clojure.string :as str])

(def directions [[-1 0] [0 -1] [1 0]  [0 1]])

(defn turn-right [current]
    (let [index (inc (.indexOf directions current))]
        (nth (cycle directions) index)))

(defn turn-left [current]
    (let [index (+ 3 (.indexOf directions current))]
        (nth (cycle directions) index)))

(defn toggle [value]
    (if (= value '.)
        'X
        '.))

(defn move [movement]
    (partial mapv + movement))

(defn burst [{position :position movement :movement :as acc}]
    (let [status (get-in acc [:nodes position])
            change-movement (if (= status '.) turn-left turn-right)
            new-status (toggle status)
            new-movement (change-movement movement)]
        (-> 
            (assoc-in acc [:nodes position] new-status)
            (assoc :movement new-movement)
            (update :position (move new-movement))
            (update :counter (if (= new-status 'X) inc identity))
            )))

; (burst { :nodes { [2 2] \.  } :position [2 2] :movement [0 1] :counter 0})

(defn read-line [line]
    (->> 
        (str/replace line #"#" "X")
        (#(str/replace % #"" " "))
        (#(str "[" % "]"))
        (read-string)))

(defn parse [input]
    (->> 
        (str/split input #"\n")
        (map read-line)
        (map-indexed (fn [y line] (map-indexed (fn [x value] {[x y] value}) line)))
        (flatten)
        (into {})))

(def data (parse (slurp "day22.data")))

(defn init [data]
    { :nodes data :position [4 4] :movement [0 -1] :counter 0 })

(defn solution [data]
    (->
        (iterate burst (init data))
        (nth 1000)))

(solution (parse ". . . . . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . # . . .
. . . # . . . . .
. . . . . . . . .
. . . . . . . . .
. . . . . . . . ."))

(defn render-board [{board :nodes position :position }]
  (let [min-coord (apply min (flatten (cons position (keys board))))
        max-coord (apply max (flatten (cons position (keys board))))]
    (->> (for [y (range min-coord (inc max-coord))
               x (range min-coord (inc max-coord))]
           [y x])
         (map #(if (= % position)
                 (str "[" (board % ".") "]")
                 (str " " (board % ".") " ")))
         (partition (- (inc max-coord) min-coord))
         (map #(apply str %))
         (mapv println))
    nil))


(render-board (burst (init (parse ". . . . . . . . .
 . . . . . . . . .
 . . . . . . . . .
 . . . . . # . . .
 . . . # . . . . .
 . . . . . . . . .
 . . . . . . . . .
 . . . . . . . . ."))))