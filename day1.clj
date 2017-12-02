(require '[clojure.string :as str])

(defn add-first-to-end [col]
    (conj col (first col)))

(defn are-equals [[x y]] (= x y))

(defn text-as-vector [text]
    (mapv read-string
        (str/split text #"")))

(def keep-if-equals-to-next
    (comp
        (filter are-equals)
        (map first)))

(defn group-with-next [coll]
        (partition 2 1 coll))

(def catpcha-reader
    (comp
        (partial transduce keep-if-equals-to-next +)
        group-with-next
        add-first-to-end
        text-as-vector))

(assert (= (catpcha-reader "1122") 3))
(assert (= (catpcha-reader "1111") 4))
(assert (= (catpcha-reader "1234") 0))
(assert (= (catpcha-reader "91212129") 9))