(defn parse [input]
    (read-string (str "[" input "]")))

(assert (= (parse "3, 4, 1, 5") [3 4 1 5]))

(defn sublist [list start length]
    (->> (cycle list)
        (drop start)
        (take length)
        (reverse)
        (vec)))

(assert (= (sublist [0 1 2 3 4] 0 3) [2 1 0]))
(assert (= (sublist [2 1 0 3 4] 3 4) [1 2 4 3]))

(defn insert [list start changes]
    (reduce-kv
            (fn [acc offset value]
                (assoc acc (mod (+ start offset) (count list)) value))
            list
            changes))

(defn solution-step [{list :list index :index :as acc} skip length]
    (let [changes (sublist list index length)
          new-list (insert list index changes)
          new-index (mod (+ index length skip) (count list))]
        {
            :list new-list
            :index new-index
        }))

(defn solution [input]
    (->> (reduce-kv 
            solution-step
            { :list (apply vector (range 256)) :index 0 }
            input)
         (:list)
         (take 2)
         (apply *)))

(def data (parse "199,0,255,136,174,254,227,16,51,85,1,2,22,17,7,192"))

;(solution data)
