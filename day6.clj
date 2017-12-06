(defn distribution-offsets [coll index value]
    (->>
        (range (+ index 1) (+ index value 1))
        (map #(mod % (count coll)))))

(defn update-index [coll index]
        (update coll index inc))

(defn distribute [coll index]
    (let [value (get coll index)
          offsets (distribution-offsets coll index value)
          reset-coll (assoc coll index 0)]
        (reduce update-index reset-coll offsets)))

(defn get-max-index [coll]
    (ffirst (filter #(= (apply max coll) (second %)) (map-indexed vector coll))))

(defn iterater [coll]
    (let [index (get-max-index coll)]
        (distribute coll index)))

(defn distribute-all [coll]
    (->>
        (iterate iterater coll)
        (reductions conj [])
        (take-while #(or (empty? %) (apply distinct? %)))))

(defn solution [coll]
    (count (last (distribute-all coll))))


(defn parse [text] 
    (->>
        (clojure.string/split text #"\t")
        (mapv read-string)))

(def data (parse (slurp "day6.data")))

(defn count-distance [coll value]
    (->>
        (map-indexed vector coll)
        (filter #(= value (second %)))
        (map first)
        (apply -)
        (Math/abs)))

(defn distribute-all-2 [coll]
    (->>
        (iterate iterater coll)
        (reductions conj [])
        (drop-while #(or (empty? %) (apply distinct? %)))
        (take 1)))

(defn solution-2 [coll]
    (let [result (last (distribute-all-2 coll))]
        (count-distance result (last result))))

(solution-2 test-data)