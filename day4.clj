
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

(defn string-to-frequencies [text]
        (frequencies (clojure.string/split text #"")))

(defn is-valid-2 [text]
    (->>
       (clojure.string/split text #" ")
       (map string-to-frequencies)
       ((juxt identity set))
       (map count)
       (apply =)
    )
)

(defn count-valid-2 [lines]
    (->> 
        (filter is-valid-2 lines)
        count))

(count-valid-2 data)