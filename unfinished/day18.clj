(require '[clojure.java.io :as io])

(defn read-line [line]
    (read-string (str "(" line ")")))

(def data 
    (->> (io/resource "day18.data")
            io/reader
            line-seq
            (mapv read-line)))

(defmulti music-player (fn [_ [cmd]] cmd))

(defmethod music-player snd [state [_ register]]
    (assoc state :snd (register state)))

(defmethod music-player rcv [state [_ register]]
    (if (not= (register state) 0)
        (assoc state register (:snd state))
        state))

(defmethod music-player set [state [_ register value]]
    (assoc state register value))

(defmethod music-player add [state [_ registerX valueY]]
    (update state registerX (partial + valueY)))

(defmethod music-player mul [state [_ registerX valueY]]
    (update state registerX (partial * valueY))

(defmethod music-player mod [state [_ registerX registerY]]
    (update state registerX (partial % (registerY state))))
    
(reduce music-player #{} [[set /x 1]])