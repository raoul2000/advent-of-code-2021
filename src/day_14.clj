(ns day-14
  (:require [clojure.string :as str]
            [clojure.core.reducers :as r]))

;; part 1 ============================

(def test-data "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(defn update-insertion-rules-map [m s]
  (->> (re-matches #"(..) -> (.)" s)
       rest
       (apply (partial assoc m))))

(defn parse-data [s]
  (let [[polymer-template _ insertion-rules] (->> (str/split-lines s)
                                                  (partition-by #{""}))]
    [(first polymer-template)
     (reduce update-insertion-rules-map {} insertion-rules)]))

(defn create-pairs [s]
  (map #(apply str %) (partition 2 1 s)))

(defn do-step-fn [insertion-rules]
  (fn [polymer]
    (let [result (->> (create-pairs  polymer)
                      (reduce #(conj %1 (str (first %2) (get insertion-rules %2))) [])
                      (apply str))]
      (str result (last  polymer)))))

(defn calculate-score [s]
  (->> (frequencies s)
       vals
       (apply (juxt max min))
       (apply -)))

(defn solve-part-1 [s step-count]
  (let [[polymer insertion-rules] (parse-data s)
        step-fn                   (do-step-fn insertion-rules)]
    (->> (iterate step-fn polymer)
         (take (inc step-count))
         last
         calculate-score)))

(comment
  (solve-part-1 test-data 10)
  ;; => 1588
  (solve-part-1 (slurp "./resources/puzzle_14.txt") 10))
  ;; => 3118


;; part 2 =============================================
;; Just like day 6, we are facing an exponential computation time increase
;; With 20 steps, solution to part-1 take 3.4s. For 1 more step (21) we reach 6.6s
;; some metrics on test-data :
;; 10 : "Elapsed time: 15.0679 msecs"
;; 1588
;; 15 : "Elapsed time: 119.018 msecs"
;; 56892
;; 20 : "Elapsed time: 3466.4279 msecs"
;; 1961318
;; 21 : "Elapsed time: 6609.8251 msecs"
;; 3942739
;;
;; We must find a way to parallelize computation see if we can get better results
;; in terms of computation time.

(defn reducef [insertion-rules]
  (fn
    ([] [])
    ([result item]
     (conj result (str (first item) (get insertion-rules item))))))

(defn combinef
  ([] [])
  ([a b]
   (into a b)))

(defn better-do-step-fn [insertion-rules]
  (fn [polymer]
    (let [result (->> (create-pairs  polymer)
                      (into [])
                      (r/fold combinef (reducef insertion-rules))
                      (apply str))]
      (str result (last  polymer)))))


(defn solve-part-2 [s step-count]
  (let [[polymer insertion-rules] (parse-data s)
        step-fn                   (better-do-step-fn insertion-rules)]
    (->> (iterate step-fn polymer)
         (take (inc step-count))
         last
         calculate-score)))

(comment

  (time (solve-part-2 test-data 10))
  ;; 10 : "Elapsed time: 4.6831 msecs / 15.0679 msecs"
  ;; 1588
  ;; 15 : "Elapsed time:75.5656 msecs / 119.018 msecs"
  ;; 56892
  ;; 20 : "Elapsed time: 2371.2025 msecs / 3466.4279 msecs"
  ;; 1961318
  ;; 21 : "Elapsed time:  4665.855501 msecs / 6609.8251 msecs"
  ;; 3942739
  ;; 25 : "Elapsed time:  174303.700901 msecs / no metrics"
  ;; 64726890

  ;; using parallel reduce (with r/fold) improve computation time but 
  ;; not enough to be acceptable
  ;; ... just like for day 6 we must find another solution
  )


;; another option is to consider the polymer as a seq of characters
;; and avoid string/seq conversion

(defn rule-matcher [insertion-rules]
  (memoize ;; save some time with memoize ?
   (fn [a b]
     (if (nil? a)
       [b]
       (if-let [ins (get insertion-rules (str a b))]
         [a ins b]
         [a b])))))

(defn do-step-fn-a [assemble]
  (fn [polymer]
    (->> polymer
         (reduce (fn [result c]
                   (apply (partial conj result) (assemble (last result) c))) []))))

(defn solve-part-2a [s step-count]
  (let [[polymer insertion-rules] (parse-data s)
        step-fn                   (do-step-fn-a (rule-matcher insertion-rules))]
    (->> (iterate step-fn polymer)
         (take (inc step-count))
         last
         calculate-score)))

(comment
  (time (solve-part-2a test-data 7))
  ;; "Elapsed time: 189.6861 msecs"
  ;; ok stop now : worst than before
  )

(defn solve-part-2b [s step-count]
  (let [[polymer insertion-rules] (parse-data s)
        step-fn                   (do-step-fn insertion-rules)]
    (->> (iterate step-fn polymer)
         (take (inc step-count))
         last
         calculate-score)))

(comment
  (time (solve-part-2b test-data 10))
  ;; "Elapsed time: 6.2774 msecs"
  ;; 1588

  (time (solve-part-2b test-data 15))
  ;; "Elapsed time: 103.4746 msecs"
  ;; 56892

  (time (solve-part-2b test-data 20))
  ;; "Elapsed time: 2914.4404 msecs"
  ;; 1961318

  (time (solve-part-2b test-data 21))
  ;; "Elapsed time: 5532.4629 msecs"
  ;; 3942739

  ;; nope :(
  ;; we are still spending too much time !
  )

;; ok this one is tricky. Let's find a completely different way
;; to solve this problem.

;; initial polymer
(def initial-polymer "NNCB")

;; create initial pair list
(def initial-pair-coll (map (partial into []) (partition 2 1 initial-polymer)))

;; store pairs in a map where k is the pair and v the occurence
;; example :
;; { "NN" 1
;;   "NC" 2
;;   etc...
;; }
;; let's see that on an example: statr with NNCB which produce the follosing map:
;;
;; { "NN" 1
;;   "NC" 1
;;   "CB" 1}
;; The first pair "NN" is replaced with 2 new pairs, based on the insertion rules :
;; "NC" and "CN".Result is :
;;
;; { "NC" 1
;;   "CN" 1
;; }
;; Now let's work on "NC" which splits into "NB" and "BC". 
;; { "NC" 1
;;   "CN" 1
;;   "NB" 1
;;   "BC" 1
;; }
;; and to end thios step, let's work on "CB" which splits into "CH" and "HB"
;; { "NC" 1
;;   "CN" 1
;;   "NB" 1
;;   "BC" 1
;;   "CH" 1
;;   "HB" 1
;; }
;; ====> This is our map after step 1. Let's go step 2
;;
;; "NC" -> "NB" "BC"
;; { "NB" 1
;;   "BC" 1
;; }
;; "CN" -> "CC" "CN"
;; { "NB" 1
;;   "BC" 1
;;   "CC" 1
;;   "CN" 1
;; }
;; "NB" => "NB" "BB"
;; { "NB" 2
;;   "BC" 1
;;   "CC" 1
;;   "CN" 1
;;   "BB" 1
;; }
;; "BC" -> "BB" "BC"
;; { "NB" 2
;;   "BC" 2
;;   "CC" 1
;;   "CN" 1
;;   "BB" 2
;; }
;; "CH" -> "CB" "HB"
;; { "NB" 2
;;   "BC" 2
;;   "CC" 1
;;   "CN" 1
;;   "BB" 2
;;   "CB" 1
;;   "HB" 2
;; }
;; "HB" -> "HC" "CB"
;; { "NB" 2
;;   "BC" 2
;;   "CC" 1
;;   "CN" 1
;;   "BB" 2
;;   "CB" 2
;;   "HB" 2
;;   "HC" 1
;; }
;; ====> This is our map after step 1. Let's go step 3

;; NNCB
(def pair-1 {[\N \N] 1
             [\N \C] 1
             [\C \B] 1})

;; NCNBCHB
(def pair-2 {[\N \C] 1
             [\C \N] 1

             [\N \B] 1
             [\B \C] 1

             [\C \H] 1
             [\H \B] 1})

(def pair-3 {[\N \C] 1
             [\C \N] 1

             [\N \B] 1
             [\B \C] 1

             [\C \H] 1
             [\H \B] 1})



