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

(def input-string "199,0,255,136,174,254,227,16,51,85,1,2,22,17,7,192")
(def data (parse input-string))

;(solution data)

;;;; PART 2

(def ascii-codes (partial map int))
(defn add-suffix [list] (concat list [17 31 73 47 23]))
(def multiply-input (comp flatten (partial repeat 64)))
(def prepare-data (comp vec multiply-input add-suffix ascii-codes))
  
;;; (prepare-data "1,2,3")

;; 65 ^ 27 ^ 9 ^ 1 ^ 4 ^ 3 ^ 40 ^ 50 ^ 91 ^ 7 ^ 6 ^ 0 ^ 2 ^ 5 ^ 68 ^ 22 = 64
;;;(bit-xor 65 27 9 1 4 3 40 50 91 7 6 0 2 5 68 22)

(defn calculate-result [numbers]
    (->> 
        (partition 16 numbers)
        (map (partial apply bit-xor))
        (map (partial format "%02x"))
        (apply str)))

(defn solution-2 [input]
    (->> (prepare-data input)
         (reduce-kv 
            solution-step
            { :list (apply vector (range 256)) :index 0 })
         (:list)
         (calculate-result)))

(solution-2 input-string)
