(defn line-checksum [coll] 
    (- (apply max coll) (apply min coll)))

(defn checksum [lines]
    (->> 
        (map line-checksum lines)
        (reduce +)))


(assert (= (checksum [[5 1 9 5] [7 5 3] [2 4 6 8]]) 18))

(defn parse [text] 
    (->>
        (clojure.string/split text #"\n")
        (map #(clojure.string/split % #"\t"))
        (mapv (partial mapv read-string))
    ))

(def data (parse (slurp "day2.data")))

(checksum data)

(defn all-pairs [coll]
    (for [x coll
          y coll
          :when (not= x y)]
        [x y]))

(defn evenly-divible [x y]
    (= 0 (mod x y)))

(defn line-checksum2 [coll] 
    (->>
        (all-pairs coll)
        (filter #(apply evenly-divible %))
        first
        (apply /)))

(defn checksum2 [lines]
    (->> 
        (map line-checksum2 lines)
        (reduce +)))

(assert (= (checksum2 [[5 9 2 8] [9 4 7 3] [3 8 6 5]]) 9))

(checksum2 data)