(ns looping-is-recursion)

(defn power [base exp]
    (loop [acc   1
           base  base
           exp   exp]
    (if (zero? exp) acc
        (recur (* acc base) base (dec exp)))))


(defn last-element [a-seq]
  (if (<= 1 (count a-seq))
      (first a-seq)
      (recur (rest a-seq))))


(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (not (= (first seq1) (first seq2)))  false
    :else (recur (rest seq1) (rest seq2)))
)

(defn find-first-index [pred a-seq]
  (loop [ idx    0
          pred?  pred
          a-seq  a-seq]
    (cond
      (empty? a-seq) nil
      (pred? (first a-seq)) idx
      :else (recur (inc idx) pred? (rest a-seq))))
)


(defn avg [a-seq]
    (loop [ total   0
            n-elem  0
            a-seq   a-seq]
    (if (empty? a-seq)
        (/ total n-elem)
        (recur (+ total (first a-seq)) (inc n-elem) (rest a-seq)))))

(defn parity [a-seq]
  (loop [ counts {}
          a-seq  a-seq ]
    (if (empty? a-seq)
        (set (map key (filter #(odd? (val %)) counts)))
        (let [cur-count (get counts (first a-seq))
              next-counts (assoc counts (first a-seq) (inc (if (= nil cur-count) 0 cur-count)))
              ]
        (recur next-counts (rest a-seq))))
))

(defn fast-fibo [n]
  (loop [f-0 0
         f-1 1
         f   1]
    (cond
     (<= n 1) n
     ( = f n) f-1
     :else (recur f-1 (+ f-0 f-1) (inc f) ))))


(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         a-seq a-seq]
  (if (or (empty? a-seq)
          (contains? seen (first a-seq)))
              (vec seen)
              (recur (conj seen (first a-seq)) (rest a-seq)))))
