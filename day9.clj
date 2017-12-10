(defn next-action-collect [character]
    (case character
        "<" :collect-garbage
        :collect))

(defn next-action-collect-garbage [character]
    (case character
        ">" :collect
        "!" :ignore-next
        :collect-garbage))

(defn next-action [action character]
    (case action
        :collect (next-action-collect character)
        :collect-garbage (next-action-collect-garbage character)
        :ignore-next :collect-garbage
        :collect))

(defn step [accum character]
    (let [
        action (:action accum)
        next-action (next-action action character)
        collector (cond 
                            (and (= action :collect) (or (= "{" character) (= "}" character))) :values
                            (= action next-action :collect-garbage) :garbage
                            :else nil)
        update-collector (if (nil? collector)
                            identity
                            (fn [acc] (update acc collector #(conj % character))))]
    (-> (assoc accum :action next-action)
        (update-collector))))

(defn process-data [characters]
    (reduce step { :action :collect :values [] :garbage [] } characters))

(def remove-garbage (comp :values process-data))

(defn group-step [accum character]
    (if (= character "}")
        (-> (update accum :values #(conj % (:open-bracket accum)))
            (update :open-bracket dec))
        (update accum :open-bracket inc)))

(defn solution [characters]
    (->> (remove-garbage characters)
        (reduce group-step { :open-bracket 0 :values [] })
        ((comp (partial reduce + 0) :values))))

(def data (clojure.string/split (slurp "day9.data") #""))

(solution data)

(def get-garbage (comp :garbage process-data))

(defn solution-2 [characters]
    (count (get-garbage characters)))

(solution-2 data)