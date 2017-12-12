(use 'clojure.data)

(defn transform-line [line]
    (->> 
        (str "[" line "]")
        (read-string)
        ((fn [[program _ & rest]] {program rest}))))

(def data (->> 
                (slurp "day12.data")
                (#(clojure.string/split % #"\n"))
                (map transform-line)
                (apply merge)))

;;; to_visit []
;;; visited #{}

(defn traveller [registry]
    (fn [{ to_visit :to_visit visited :visited :as acc}]
        (let [new_to_visit (->> (map #(get registry %) to_visit)
                                (flatten)
                                (filter #((complement contains?) visited %)))]
        (-> 
            (assoc acc :to_visit new_to_visit)
            (update :visited #(apply conj % to_visit)))
    )))

;;; ((traveller data) { :to_visit [0] :visited #{} })
;;; PART 1
(->> 
    (iterate (traveller data) { :to_visit [0] :visited #{} })
    (drop-while #((complement empty?) (:to_visit %)))
    ((comp count :visited first)))

;;; PART 2

(defn find-group-for [program]
    (->> 
        (iterate (traveller data) { :to_visit [program] :visited #{} })
        (drop-while #((complement empty?) (:to_visit %)))
        ((comp :visited first))))

(defn find-groups [{ availables :availables :as acc }]
      (let [key (first availables)
            programs (find-group-for key)] 
        (-> 
            (update acc :availables #(first (diff % (set programs))))
            (update :groups #(merge % { key programs })))))

(->> 
    (iterate find-groups { :availables (set (keys data)) :groups #{} })
    (drop-while #((complement empty?) (:availables %)))
    ((comp count :groups first)))