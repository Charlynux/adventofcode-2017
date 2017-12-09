(defn get-childrens [programs-map]
    (apply concat (map :children (vals programs-map))))

(defn solution [programs-map]
    (let [
            parents (keys programs-map)
            childrens (get-childrens programs-map)]
        (first (clojure.set/difference (set parents) (set childrens)))))

(defn read-program-line [text]
    (let [matcher (re-matcher #"(\w+) \((\d+)\)(?: -> )?(.+)?" text)
            find (re-find matcher)]
    (rest (re-groups matcher))))

(defn create
    [key weight children]
        { key 
            { 
                :children (if (nil? children) [] (clojure.string/split children #", "))
                :weight (read-string weight)
            } })

(def create-from-line (comp #(apply create %) read-program-line))

(create-from-line "abcibu (84) -> aobio, oioih")

(create-from-line "abcibu (84)")

(defn parse [text] 
    (->>
        (clojure.string/split text #"\n")
        (mapv create-from-line)
        (apply merge)
    ))

(def data (parse (slurp "day7.data")))

;;(solution data)


;;; Stole some data structure ideas to Bruce Haumann
(defn index-data [accum [name weight children]]
    (let [children (if (nil? children) [] (clojure.string/split children #", "))]
        (->
            (reduce (fn [acc child] (assoc-in acc [:parent child] name)) accum children)
            (assoc-in [:weight name] (read-string weight))
            (assoc-in [:children name] (set children))
        )))

(defn parse-index [text]
    (->>
        (clojure.string/split text #"\n")
        (map read-program-line)
        (reduce index-data {})
    ))

(def data (parse-index (slurp "day7.data")))
(def example-data (parse-index (slurp "day7-example.data")))

(defn solution-1-with-index [programs-map]
    (first 
        (clojure.set/difference
            (set (vals (:parent programs-map)))
            (set (keys (:parent programs-map))))))

;;; Be careful !
;;; This code is very dirty.
;;; This puzzle given a very hard day. I hope my graph teacher never see this code.

(defn find-leafs [programs-map]
    (set
        (map first 
            (filter #(empty? (second %)) 
                    (:children programs-map)))))

(defn reduce-leaf [programs-map leaf]
    (let [parent (get-in programs-map [:parent leaf])
            leaf-weight (get-in programs-map [:weight leaf])]
        (if (nil? parent)
            programs-map
            (->
                (update-in programs-map [:weight parent] #(+ % leaf-weight))
                (update :next-step #(conj % parent)))
    )))

(defn step [programs-map]
    (let [leafs (:next-step programs-map)]
   (reduce reduce-leaf (assoc programs-map :next-step #{}) leafs)))

(defn apply-weight [programs-map]
    (->>
        (iterate step (assoc programs-map :next-step (find-leafs programs-map)))
        (drop-while #(> (count (:next-step %)) 1))
        (take 1)
        first
    ))

(defn find-different-children [programs-map]
    (fn [[root root-delta]]
        (let [children (get-in programs-map [:children root])
            differents (for [
                x children
                y children
                :let [
                    x-weight (get-in programs-map [:weight x])
                    y-weight (get-in programs-map [:weight y])
                    delta (- x-weight y-weight)
                ]
                :when (and (not= x y) (not= delta 0))]
                [x delta]
            )]
            (->> 
                (frequencies differents)
                (sort-by val > )
                ffirst)
        )))

(defn solution-2 [programs-map]
    (let [programs-map-weight (apply-weight programs-map)
            root (first (:next-step programs-map-weight))
            find-next-root (find-different-children programs-map-weight)]
        (->>
            (iterate find-next-root [root 0])
            (take-while (complement nil?))
            (last)
            ((fn [[name delta]] (- (get-in programs-map [:weight name]) delta))))))