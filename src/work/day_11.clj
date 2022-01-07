(ns work.day-11
  (:require [clojure.string :as str]))

(def test-data "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

;;      0 1 2 3 4 5 6 7 8 9
;; -------------------------
;; 0 | (5 4 8 3 1 4 3 2 2 3)
;; 1 | (2 7 4 5 8 5 4 7 1 1)
;; 2 | (5 2 6 4 5 5 6 1 7 3)
;; 3 | (6 1 4 1 3 3 6 1 4 6)
;; 4 | (6 3 5 7 3 8 5 4 7 8)
;; 5 | (4 1 6 7 5 2 4 6 4 5)
;; 6 | (2 1 7 6 8 4 1 7 2 1)
;; 7 | (6 8 8 2 8 8 1 1 3 4)
;; 8 | (4 8 4 6 8 4 8 5 5 4)
;; 9 | (5 2 8 3 7 5 1 5 2 6)

(defn parse-data [s]
  (->> (remove #{\newline} s)
       (map #(Character/digit % 10))
       (into [])))

(def levels (parse-data test-data))

(defn same-line [p1 p2]
  (= (quot p1 10)
     (quot p2 10)))

(defn up-pos [pos]
  (let [up (- pos 10)]
    (when (> up -1) up)))

(comment
  (nil? (up-pos 1))
  (nil? (up-pos 9))
  (= 0 (up-pos 10))
  (= 2 (up-pos 12))
  ;;
  )

(defn down-pos [pos]
  (let [down  (+ pos 10)]
    (when (< down 100) down)))

(comment
  (= 10 (down-pos 0))
  (= 15 (down-pos 5))
  (= 37 (down-pos 27))
  (= 99 (down-pos 89))
  (nil? (down-pos 90)))

(defn right-pos [pos]
  (let [right (inc pos)]
    (when (same-line pos right) right)))

(comment
  (= 1 (right-pos 0))
  (= 9 (right-pos 8))
  (nil? (right-pos 9))
  (= 13 (right-pos 12))
  (nil? (right-pos 29))
  (nil? (right-pos 99)))

(defn left-pos [pos]
  (let [left (dec pos)]
    (when (and (> left -1)
               (same-line pos left)) left)))

(comment
  (nil? (left-pos 0))
  (zero? (left-pos 1))
  (nil? (left-pos 10))
  (= 10 (left-pos 11)))


(defn up-right-pos [pos]
  (when-let [up (up-pos pos)]
    (let [up-right (inc up)]
      (when (same-line up up-right) up-right))))

(comment
  (nil? (up-right-pos 0))
  (= 1 (up-right-pos 10))
  (= 9 (up-right-pos 18))
  (nil? (up-right-pos 19)))

(defn up-left-pos [pos]
  (when-let [up (up-pos pos)]
    (let [up-left (dec up)]
      (when (and (same-line up up-left)
                 (> up-left -1))
        up-left))))

(comment
  (nil? (up-left-pos 0))
  (nil? (up-left-pos 9))
  (nil? (up-left-pos 10))
  (zero? (up-left-pos 11))
  (= 8 (up-left-pos 19))
  ;;
  )

(defn down-right-pos [pos]
  (when-let [down (down-pos pos)]
    (let [down-right (inc down)]
      (when (same-line down down-right) down-right))))

(comment
  (= 19 (down-right-pos 8))
  (nil? (down-right-pos 9))
  (= 21 (down-right-pos 10))
  (nil? (down-right-pos 90)))

(defn down-left-pos [pos]
  (when-let [down (down-pos pos)]
    (let [down-left (dec down)]
      (when (and (same-line down down-left)
                 (> down-left -1))
        down-left))))

(comment
  (nil? (down-left-pos 0))
  (= 10 (down-left-pos 1))
  (= 18 (down-left-pos 9))
  (nil? (down-left-pos 90)))

(defn adjacent-pos [pos]
  (remove nil? [(up-pos pos)
                (up-right-pos pos)
                (right-pos pos)
                (down-right-pos pos)
                (down-pos pos)
                (down-left-pos pos)
                (left-pos pos)
                (up-left-pos pos)]))

(comment
  (adjacent-pos 1)
  (adjacent-pos 10)
  (adjacent-pos 11)
  
  )