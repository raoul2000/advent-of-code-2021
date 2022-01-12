(ns day-13
  (:require [clojure.string :as str]))

;; part 1 ==========================================

(def test-data "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
")

(defn parse-coords [xs]
  (->> (map #(str/split % #",") xs)
       (map (fn [[x y]]
              [(Integer/parseInt x)
               (Integer/parseInt y)]))))

(defn parse-instructions [xs]
  (map (fn [s]
         (when-let [[_ axis s] (re-matches #"fold along ([xy])=([0-9]+)" s)]
           [axis (Integer/parseInt s)])) xs))

(comment
  (parse-instructions ["fold along y=7"])
  ;;
  )

(defn parse-data [s]
  (let [[coords _ fold-instr] (->> (str/split-lines s)
                                   (partition-by #{""}))]
    [(parse-coords coords)
     (parse-instructions fold-instr)]))

(comment
  (parse-data test-data)
  (parse-data (slurp "./resources/puzzle_13.txt"))
  ;;
  )

(defn paper-size [coords]
  [(inc (apply max (map first coords)))
   (inc (apply max (map last coords)))])

(comment
  (let [[coords _] (parse-data (slurp "./resources/puzzle_13.txt"))]
    (paper-size coords)))

;; boolean marker --------
;;(def empty-point false)
;;(def marked-point true)
;;(defn merge-dots [d1 d2]
;;  (or d1 d2))

(def empty-point \_)
(def marked-point \X)
(defn merge-dots [d1 d2]
  (if (= empty-point d1 d2)
    empty-point
    marked-point))

(defn create-transparent-paper [col-count line-count]
  (into [] (repeatedly line-count #(into [] (repeat col-count empty-point)))))

(comment
  (create-transparent-paper 5 5)
  (create-transparent-paper 5 8))

(defn mark-points [points paper]
  (reduce (fn [r [x y]]
            (update r y #(update % x (constantly marked-point))))
          paper
          points))

(comment
  (let [[coords _]             (parse-data test-data)
        [col-count line-count] (paper-size coords)]
    (->> (create-transparent-paper col-count line-count)
         (mark-points coords)
         ))

  (mark-points
   (create-transparent-paper 5 5)
   [[0 0] [4 4] [1 2]]))


