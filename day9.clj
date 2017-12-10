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
    (let [update-values (if (and 
                                (= (:action accum) :collect)
                                (or (= "{" character) (= "}" character)))
                            (fn [values] (conj values character))
                            identity)
            update-garbage (if (and 
                                ;; Should be prev-action = ":collect-garbage" and next-action = ":collect-garbage"
                                (= (:action accum) :collect-garbage)
                                (and (not= "!" character) (not= ">" character)))
                            (fn [values] (conj values character))
                            identity)]
    (-> (update accum :action #(next-action % character))
        (update :garbage update-garbage)
        (update :values update-values))))

(defn process-data [characters]
    (reduce step { :action :collect :values [] :garbage [] } characters))

(def remove-garbage (comp :values process-data))

(defn group-step [accum character]
    (if (and (= character "}") (>= (:open-bracket accum) 1))
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