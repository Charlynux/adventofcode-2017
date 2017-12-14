
(defn sublist [list start length]
    (->> (cycle list)
        (drop start)
        (take length)
        (reverse)
        (vec)))

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


(def ascii-codes (partial map int))
(defn add-suffix [list] (concat list [17 31 73 47 23]))
(def multiply-input (comp flatten (partial repeat 64)))
(def prepare-data (comp vec multiply-input add-suffix ascii-codes))
  
(defn calculate-result [numbers]
    (->> 
        (partition 16 numbers)
        (map (partial apply bit-xor))
        (map (partial format "%02x"))
        (apply str)))

(defn knot-hash [input]
    (->> (prepare-data input)
         (reduce-kv 
            solution-step
            { :list (apply vector (range 256)) :index 0 })
         (:list)
         (calculate-result)))

(defn binary-string [hex-value]
    (->> (read-string (str "0x" hex-value))
        (Integer/toBinaryString)
        (read-string)
        (format "%04d")))

(defn hash-to-binary [hash]
    (->> (map binary-string hash)
        (apply str)))

(def generate-line (comp hash-to-binary knot-hash))

(defn solution [input]
    (->> (range 128)
         (map #(str input "-" %))
         (map generate-line)
         (apply str)
         (#(clojure.string/replace % #"0" ""))
         (count)))
;;(solution "nbysizxe")