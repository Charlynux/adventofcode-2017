
(def data (clojure.string/split (slurp "day4.data") #"\n"))

(defn is-valid [text]
    (->>
       (clojure.string/split text #" ")
       ((juxt identity set))
       (map count)
       (apply =)
    )
)

(defn count-valid [lines]
    (->> 
        (filter is-valid lines)
        count))

(count-valid data)