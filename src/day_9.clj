(ns day-9
  (:require [clojure.string :as str]))

;; part 1 ==========================
;; we choose to keep a seq structure to represent the height map (and not try
;; to store it in a matrix)

(def test-data
  "2199943210
3987894921
9856789892
8767896789
9899965678")

(def test-height-map (apply vector "21999432103987894921985678989287678967899899965678"))

(def row-num quot)
(defn get-adjacent
  "Returns a seq of values adjacent to the value at zero based  position *pos* in 
   the seq *xs* considered as a matrix with *col-count* columns. 
   
   Example:
   ```
   (get-adjacent 0 2 [:a :b :c :d])
   => (:b :d)
   (get-adjacent 4 3 [1 2 3 
                      4 5 6 
                      7 8 9])
   => (2 8 6 4)
   ```
   "
  [pos col-count xs]
  (let [pos-row-num      (row-num pos col-count)
        pos-right        (inc pos)
        right-same-line? (= pos-row-num
                            (row-num pos-right col-count))
        pos-left         (dec pos)
        left-same-line?  (= pos-row-num
                            (row-num pos-left col-count))]
    (remove nil? [(get xs (- pos col-count))  ;; up
                  (get xs (+ pos col-count))  ;; bottom
                  (when right-same-line?      ;; right
                    (get xs pos-right))
                  (when left-same-line?       ;; left
                    (get xs pos-left))])))

(comment
  (get-adjacent 0 10 test-height-map)
  (get-adjacent 1 10 test-height-map)
  (get-adjacent 1 2 "0011"))

(defn get-low-points [max-pos col-count height-vector]
  (reduce (fn [result pos]
            (let [heat     (get height-vector pos)
                  adjacent (get-adjacent pos col-count height-vector)]
              (if (< heat (apply min adjacent))
                (conj result heat)
                result)))
          []
          (range 0 max-pos)))

(defn solve-part-1 [s]
  (let [lines        (str/split-lines s)
        col-count    (count (first lines))
        row-count    (count lines)
        height-vector  (->> (remove #{\newline} s)
                          (map #(Character/digit % 10))
                          (apply vector))]
    (->> (get-low-points (* col-count row-count) col-count height-vector)
         (map inc)
         (apply +))))

(comment
  (solve-part-1 test-data)
  ;; => 15
  (solve-part-1 (slurp "./resources/puzzle_9.txt"))
  ;; => 508
  )

