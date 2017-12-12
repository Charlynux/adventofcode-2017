(def data (->> (slurp "day11.data")
                (#(str "[" % "]"))
                (read-string)))

(frequencies data)
;; {ne 1295, se 1701, n 1024, nw 1336, sw 1331, s 1536}
;;

;;; Done it by hand just to understand, solve the problem by accident. ;)
;;; ne + sw => 0 { ne 0 sw 36 }
;;; nw + se => 0 { nw 0 se 365 }
;;; n + s => 0 { n 0 s 512 }
;;; { sw 36 se 365 s 512 }
;;; sw + se => s
;;; { sw 0 se 329 s 476 }
;;; 877

(def antipods {
    'n 's
    's 'n
    'ne 'sw
    'sw 'ne
    'nw 'se
    'se 'nw
})

;; ne + nw => s
;; ne + s => se
;; TODO

(def completions {
    'ne 'nw 
    'nw 'ne
    'sw 'se
    'se 'sw
})

(def completers {
    'ne 'n
    'nw 'n
    'sw 's
    'se 's
})

;;; {ne 1295, se 1701, n 1024, nw 1336, sw 1331, s 1536}

(defn update-key [acc key]
    (let [antipod (key antipods)
            completion (key completions)
            completer (key completers)]
        (cond
            (>= (get acc antipod 0) 1)
                (update acc antipod (fnil dec 0))
            (>= (get acc completion 0) 1)
                (-> (update acc completion (fnil dec 0))
                    (update completer (fnil inc 0)))
            :else (update acc key (fnil inc 0)))
        ))

(reduce 
    update-key
    {} data)