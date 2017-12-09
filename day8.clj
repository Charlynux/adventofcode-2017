(require '[clojure.spec.alpha :as s])

(s/def ::comparator?
    (s/alt 
        :gt #{'>}
        :gte #{'>=}
        :lt #{'<}
        :lte #{'<=}
        :eq #{'==}
        :neq #{'!=}))

(s/def ::operator?
    (s/alt 
        :inc #{'inc}
        :dec #{'dec}))

(s/def ::instruction
    (s/cat 
        :op-register symbol?
        :operator ::operator?
        :op-value number?
        ::if #{'if}
        :com-register symbol?
        :comparator ::comparator?
        :com-value number?))

(defn line-to-seq [line] (read-string (str "[" line "]")))

(defn parse-instruction [line] (s/conform ::instruction (line-to-seq line)))

(parse-instruction "b inc 5 if a > 1")
;; {:op-register 'b, :operator [:inc inc], :op-value 5, :user/if if, :com-register 'a, :comparator [:gt >], :com-value 1}
(parse-instruction "a inc 1 if b < 5")
;; {:op-register a, :operator [:inc inc], :op-value 1, :user/if if, :com-register b, :comparator [:lt <], :com-value 5}
(parse-instruction "c dec -10 if a >= 1")
;; {:op-register c, :operator [:dec dec], :op-value -10, :user/if if, :com-register a, :comparator [:gte >=], :com-value 1}
(parse-instruction "c inc -20 if c == 10")
;; {:op-register c, :operator [:inc inc], :op-value -20, :user/if if, :com-register c, :comparator [:eq ==], :com-value 10}

;;;;(update {} :b (fnil #(+ 5 %) 0))

(def comparators {
    :gt >
    :gte >=
    :lt <
    :lte <=
    :eq =
    :neq not=
})

(def operators {
    :inc +
    :dec -
})

(defn apply-instruction [registry instruction]
    (let [comparator (comparators (first (:comparator instruction)))
            operator (operators (first (:operator instruction)))]
        (if 
            (comparator (get registry (:com-register instruction) 0) (:com-value instruction))
            (update registry (:op-register instruction) (fnil #(operator % (:op-value instruction)) 0))
            registry
        )))

(defn solution [instructions]
    (->>
        (reduce apply-instruction {} instructions)
        (vals)
        (apply max)
))

;;;
;;; I should really learn to read Lazily
(defn parse [text] 
    (->>
        (clojure.string/split text #"\n")
        (mapv parse-instruction)
    ))

(def data (parse (slurp "day8.data")))

(solution data)