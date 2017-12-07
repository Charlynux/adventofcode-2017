(defn get-childrens [programs-map]
    (reduce-kv
        #(concat %1 (:children %3))
        []
        programs-map))

(defn solution [programs-map]
    (let [childrens (get-childrens programs-map)]
        (filter
            (complement #(some #{(first %)} childrens))
            programs-map))
        )

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
