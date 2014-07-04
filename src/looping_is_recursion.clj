(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* acc base ) (dec n))))]
    (helper 1 exp)))



 (defn last-element [a-seq]
  (let [helper (fn [acc b-seq]
                 (if (empty? b-seq) acc
                   (recur (first b-seq) (rest b-seq))))]
    (helper nil a-seq))
   )


(defn seq= [seq1 seq2]
  (let [helper (fn [state s1 s2]
                 (cond (or (empty? s1) (empty? s2)) state
                   (not= (first s1) (first s2)) false
                       :else
                       (recur true (rest s1) (rest s2))))]
    (if (not= (count seq1) (count seq2)) false
      (helper true seq1 seq2))))


(defn find-first-index [pred a-seq]
  (loop [idx 0]
    (cond (>= idx (count a-seq)) nil
          (pred (get a-seq idx)) idx
          :else
            (recur (inc idx)))))


(defn avg [a-seq]
  (if (empty? a-seq ) 0
  (loop [run-total 0 idx 0]
    (cond (>= idx (count a-seq)) (/ run-total idx)
          :else (recur (+ run-total (get a-seq idx)) (inc idx))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn parity [a-seq]
  (loop [parity-set #{} b-seq a-seq]
    (cond (empty? b-seq) parity-set
          :else (recur (toggle parity-set (first b-seq)) (rest b-seq)))))

(defn fast-fibo [n]
  (if (zero? n) 0
    (loop [f-n 1 f-n1 0 indx n]
      (cond (> indx 1 )
          (recur  (+ f-n f-n1) f-n (dec indx))
           :else f-n))))


(defn cut-at-repetition [a-seq]
  (loop [out-vec [] track-set #{} b-seq a-seq ]
    (cond (empty? b-seq) out-vec
    :else
      (recur (if (contains? track-set (first b-seq)) out-vec (conj out-vec (first b-seq )))
             (conj track-set (first b-seq))
             (rest b-seq)))))


