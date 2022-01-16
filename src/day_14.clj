(ns day-14
  (:require [clojure.string :as str]))

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

(comment
  (parse-data test-data)
  (let [[polymer insertion-rules] (parse-data test-data)
        step-result               (->> (create-pairs (first polymer))
                                       (reduce #(conj %1 (str (first %2) (get insertion-rules %2))) [])
                                       (apply str))]
    (str step-result (last (first polymer))))

  (let [[polymer insertion-rules] (parse-data test-data)
        step-fn (do-step-fn insertion-rules)]
    ;;(step-fn polymer)
    (take 3 (iterate step-fn polymer)))
  ;;
  )





