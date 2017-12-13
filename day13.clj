; Complete calculation for scanner position 
; (defn nth-movement [index length]
;     (let [
;         nb-movements (- (* 2 length) 2)
;         modulo (mod index nb-movements)]
;     (if (< modulo length)
;         modulo
;         (inc (- modulo length)))))

(defn is-zero [index length]
    (let [
        nb-movements (- (* 2 length) 2)
        modulo (mod index nb-movements)]
    (= modulo 0)))

(defn convert-line [[index length]]
    (if (is-zero index length)
        (* index length)
        0))

(defn solution [lines]
   (apply + (map convert-line lines)))

(solution [[0 3] [1 2] [4 4] [6 4]])

(defn parse [text] 
        (->>
            (clojure.string/split text #"\n")
            (map #(clojure.string/split % #":"))
            (mapv (partial mapv read-string))
        ))

(def data (parse (slurp "day13.data")))

(defn is-ko [lines]
    (some #(is-zero (first %) (second %)) lines))

(defn delay-lines [lines time]
  (mapv (fn [[index length]] (vector (+ index time) length)) lines))

(defn solution-2 [lines]
    (->> (range)
         (map #(delay-lines lines %))
         (drop-while is-ko)
         (take 1)))

; (solution-2 [[0 3] [1 2] [4 4] [6 4]])

(solution-2 data)
