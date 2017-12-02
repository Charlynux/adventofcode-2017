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

(defn move-from-half [coll]
    (let [index (/ (count coll) 2)]
        (concat
            (subvec coll index)
            (subvec coll 0 index))))

(defn group [coll]
        (map vector 
            coll
            (move-from-half coll)))

(def catpcha-reader
    (comp
        (partial transduce keep-if-equals-to-next +)
        group
        text-as-vector))

(assert (= (catpcha-reader "1212") 6))
(assert (= (catpcha-reader "1221") 0))
(assert (= (catpcha-reader "123425") 4))
(assert (= (catpcha-reader "123123") 12))
(assert (= (catpcha-reader "12131415") 4))
