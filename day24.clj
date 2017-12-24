
(def example-components '((0 2) (2 2) (2 3) (3 4) (3 5) (0 1) (10 1) (9 10)))

(defn find-connected [list port]
    (filter
        #(some (partial = port) %)
        list))

; (find-connected example-components 0)

(defn find-unused-port [component port]
    (if (apply = component)
        port
        (first (filter (partial not= port) component))))

(defn next-node [node component]
    (-> node
        (update :port (partial find-unused-port component))
        (update :used (partial cons component))))

(defn intersect [components used]
    (filter (complement (set used)) components))

(defn find-children [components node]
    (map
        (partial next-node node)
        (find-connected (intersect components (:used node)) (:port node))))

(defn components-tree [components]
    (tree-seq
        (constantly true)
        (partial find-children components)
        { :used '() :port 0 }))

(defn bridge-strength [bridge]
    (apply + 
        (flatten bridge)))

(defn solution [components]
    (apply max
        (map #(bridge-strength (:used %))
            (components-tree components))))

(defn read-line [line]
    (read-string (str "(" line ")")))

(defn parse [input]
    (-> input
        (clojure.string/replace #"/" ",")
        (clojure.string/split #"\n")
        (#(map read-line %))
        ))

(def data (parse (slurp "day24.data")))

(defn solution-2 [components]
    (let [bridges (map :used (components-tree components))
            longest (apply max (map count bridges))]
        (->> 
            (filter #(= (count %) longest) bridges)
            (map bridge-strength)
            (apply max))
    ))
