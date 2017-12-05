;;; (def m1 (atom [0 1 2 3]))
;;; (swap! m1 update-in [0] inc)

(defn create-reducer [coll]
    (let [values (atom coll)]
        (fn [index] 
            (let 
                [offset (nth @values index)]
                (swap! values update-in [index] inc)
                (+ offset index)))))

;;;(def get-next-value (create-reducer [0 1 2 3]))
;;;(get-next-value 2)
;;;(get-next-value 2)

(defn count-steps [coll]
        (->> 
            (iterate (create-reducer coll) 0)
            (take-while #(and (>= % 0) (< % (count coll))))
            (count)
        ))

(assert (= (count-steps [0 3  0  1 -3]) 5))

(defn parse [text] 
    (->>
        (clojure.string/split text #"\n")
        (mapv read-string)
    ))

(def data (parse (slurp "day5.data")))

(count-steps data)

(defn update-offset [offset]
    (if (>= offset 3)
        (dec offset)
        (inc offset)))

(defn create-reducer-2 [coll]
    (let [values (atom coll)]
        (fn [index] 
            (let 
                [offset (nth @values index)]
                (swap! values update-in [index] update-offset)
                (+ offset index)))))

(defn count-steps-2 [coll]
    (->> 
        (iterate (create-reducer-2 coll) 0)
        (take-while #(and (>= % 0) (< % (count coll))))
        (count)
    ))

(assert (= (count-steps-2 [0 3  0  1 -3]) 10))

(count-steps-2 data)