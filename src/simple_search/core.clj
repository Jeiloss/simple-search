(ns simple-search.core
  (:use simple-search.knapsack-examples.knapPI_11_20_1000
        simple-search.knapsack-examples.knapPI_13_20_1000
        simple-search.knapsack-examples.knapPI_16_20_1000
        simple-search.knapsack-examples.knapPI_16_200_1000))

;;; An answer will be a map with (at least) four entries:
;;;   * :instance
;;;   * :choices - a vector of 0's and 1's indicating whether
;;;        the corresponding item should be included
;;;   * :total-weight - the weight of the chosen items
;;;   * :total-value - the value of the chosen items

(defrecord Answer [instance choices total-weight total-value])

(defn included-items
  "Takes a sequences of items and a sequence of choices and
  returns the subsequence of items corresponding to the 1's
  in the choices sequence."
  [items choices]
  (map first
       (filter #(= 1 (second %))
               (map vector items choices))))

(defn make-answer
  [instance choices]
  (let [included (included-items (:items instance) choices)]
    (->Answer instance choices
              (reduce + (map :weight included))
              (reduce + (map :value included)))))

(defn random-answer
  "Construct a random answer value for the given instance of the
  knapsack problem."
  [instance]
  (let [choices (repeatedly (count (:items instance))
                            #(rand-int 2))]
    (make-answer instance choices)))

;; (random-answer knapPI_13_20_1000_7)

;;; It might be cool to write a function that
;;; generates weighted proportions of 0's and 1's.

(defn score
  "Takes the :total-weight of the given answer unless it's over capacity,
   in which case we return 0."
  [answer]
  (if (> (:total-weight answer)
         (:capacity (:instance answer)))
    0
    (:total-value answer)))

(defn penalized-score
  "Takes the total-weight of the given answer unless it's over capacity,
   in which case we return the negative of the total weight."
  [answer]
  (if (> (:total-weight answer)
         (:capacity (:instance answer)))
    (- (:total-weight answer))
    (:total-value answer)))

(defn lexi-score
  [answer]
  (let [shuffled-items (shuffle (included-items (:items (:instance answer))
                                                (:choices answer)))
        capacity (:capacity (:instance answer))]
    (loop [value 0
           weight 0
           items shuffled-items]
      (if (empty? items)
        value
        (let [item (first items)
              w (:weight item)
              v (:value item)]
          (if (> (+ weight w) capacity)
            (recur value weight (rest items))
            (recur (+ value v)nswer
                   (+ weight w)
                   (rest items))))))))

;(lexi-score (random-answer knapPI_16_200_1000_1))

(defn add-score
  "Computes the score of an answer and inserts a new :score field
   to the given answer, returning the augmented answer."
  [scorer answer]
  (assoc answer :score (scorer answer)))

(defn random-search
  [scorer instance max-tries]
  (apply max-key :score
         (map (partial add-score scorer)
               (repeatedly max-tries #(random-answer instance)))))

; (random-search penalized-score knapPI_16_200_1000_1 10000)

(defn mutate-choices
  [choices]
  (let [mutation-rate (/ 1 (count choices))]
    (map #(if (< (rand) mutation-rate) (- 1 %) %) choices)))

(defn mutate-answer
  [answer]
  (make-answer (:instance answer)
               (mutate-choices (:choices answer))))

; (def ra (random-answer knapPI_11_20_1000_1))
; (mutate-answer ra)

(defn hill-climber
  [mutator scorer instance max-tries]
  (loop [current-best (add-score scorer (random-answer instance))
         num-tries 1]
    (let [new-answer (add-score scorer (mutator current-best))]
      (if (>= num-tries max-tries)
        current-best
        (if (> (:score new-answer)
               (:score current-best))
          (recur new-answer (inc num-tries))
          (recur current-best (inc num-tries)))))))

; (time (random-search score knapPI_16_200_1000_1 100000
; ))

; (time (hill-climber mutate-answer score knapPI_16_200_1000_1 100000
; ))

; (time (hill-climber mutate-answer penalized-score knapPI_16_200_1000_1 100000
; ))


;;;;;;MATT START
;;   [answer]
;;     (let [item (first :items)
;;           w (:weight item)
;;           v (:value item)]
;;       (/ v w)))


(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

;; (mean [1 4 3])

(defn standard-deviation [coll]
  (let [avg (mean coll)
        squares (for [x coll]
                  (let [x-avg (- x avg)]
                    (* x-avg x-avg)))
        total (count coll)]
    (-> (/ (apply + squares)
           (- total 1))
        (Math/sqrt))))

;; (standard-deviation [4 5 2 9 5 7 4 5 4])

;;;;;;END MATT

;;;;;;TRAVIS START


(:value (first (:items knapPI_16_200_1000_1)))
(first knapPI_16_200_1000_1)

;; (def enlist `(3))
;; (cons 5 enlist)

(def make-list (fn [instance]
                 (loop [remaining (count (:items instance))
                        lists (:items instance)
                        enlist nil]
                   (if (= remaining 0)
                     enlist
                     (recur (dec remaining)
                            (rest lists)
                            (cons (float (/ (:value (first lists)) (:weight (first lists)))) enlist))))))

(mean (make-list knapPI_16_200_1000_1))
(standard-deviation (make-list knapPI_16_200_1000_1))

;; (def find-value (fn [thing]
;;                   (/ (:value thing) (:weight thing))))

(def avg-price (fn [instance]
                 (loop [total 0
                        remaining (count (:items instance))
                        lists (:items instance)]
                   (if (= remaining 0)
                     (/ total (count (:items instance)))
                     (recur (+ total (:value (first lists)))
                            (dec remaining)
                            (rest lists))))))

(avg-price knapPI_16_200_1000_1)
;;;;;;END TRAVIS
