;; Convert instruction from text to clojure function
(defn read-instruction [instruction]
    (-> instruction
        (clojure.string/replace #"^[a-z]" #(str % " "))
        (clojure.string/replace #"[a-z]/" #(str "'" % "'"))
        (clojure.string/replace #"/" ",")
        (#(str "(" % ")"))
        (read-string)
        (eval)))

(defn spin [offset list]
    (let [length (count list)
            tail (- length offset)]
        (into
            (subvec list tail)
            (subvec list 0 tail))))

(defn exchange [indexA indexB list]
    (let [valueA (get list indexA)
            valueB (get list indexB)]
        (assoc list indexA valueB indexB valueA)))

(defn partner [valueA valueB list]
    (let [indexA (.indexOf list valueA)
            indexB (.indexOf list valueB)]
        (assoc list indexA valueB indexB valueA)))

(defn s [offset]
    (partial spin offset))

(defn x [indexA indexB]
    (partial exchange indexA indexB))

(defn p [valueA valueB]
    (partial partner valueA valueB))

(defn solution [programs instructions]
    (reduce 
        (fn ([list instruction] (instruction list)))
        programs
        instructions))

(def data ['a 'b 'c 'd 'e 'f 'g 'h 'i 'j 'k 'l 'm 'n 'o 'p])

(def instructions
    (-> (slurp "day16.data")
        (clojure.string/split #",")
        (#(map read-instruction %))))

(defn find-repetition [programs instructions]
    (let [step solution]
        (count
            (take-while #(not= programs %)
                (rest one-round)))))

(def one-round (iterate (fn [previous] (solution previous instructions)) data))

(nth one-round (mod 1000000000 36))