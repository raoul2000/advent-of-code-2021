(ns day-4
  (:require [clojure.string :as str]))


(def test-data "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

;; part 1 ========

(def data (->> test-data;;(slurp "./resources/puzzle_4.txt")
               (str/split-lines)))

(def  draw-num (->> (str/split (first data) #",")
                    (map #(Integer/parseInt % 10))))

(defn numbers->int-coll [s]
  (->> (str/split s #" ")
       (remove str/blank?)
       (map #(Integer/parseInt % 10))))

(numbers->int-coll " 12 65 22 11 26 55 98 78")

(def boards (->> (rest data)
                 (reduce (fn [result line]
                           (if (str/blank? line)
                             (str result "\n")
                             (str result " " line))) "")
                 (str/split-lines)
                 (remove str/blank?)
                 (map numbers->int-coll)
                 (map #(partition 5 %))))
boards

(defn mark-num [boards num] boards)

(defn find-winner-board [boards] nil)
(defn compute-score [boards draw] 1)

(defn play-bingo [boards draw-nums]
  (let [this-draw     (first draw-nums)
        boards-played (mark-num boards this-draw)]
    (if-let [win-board (find-winner-board boards)]
      (compute-score win-board this-draw)
      (if (= 1 (count draw-nums))
        0 ;; no winner - no more num to draw
        (recur boards-played (rest draw-nums))))))
