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

(defn merge-dot-xs [v]
  (vec (map merge-dots (first v) (second v))))

(comment
  (merge-dot-xs [[empty-point marked-point empty-point marked-point]
                 [empty-point empty-point  marked-point marked-point]
                 ])
  ;;
  )


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

(defn cols->lines
  "Returns a matrix (seq of seq) where the cols in the given matrix
   become lines in the result matrix.
   ```
   (cols->lines [[:a :b]
                 [ 1  2]])
   => ([:a 1] [:b 2])
   ```"
  [xs]
  (apply map vector xs))

(comment
  (cols->lines [[:a :b]
                [1 2]])
  ;;
  )
(defn split-on-fold
  "Returns a seq containing 2 items, result of spliting *xs* in 2 seq 
   before and after item at position fold. Item at position fold ignored.

   ```
   (split-on-fold 2 [1 2 3 4 5])
   => [(1 2) (4 5)]
   ```"
  [fold xs]
  (->> ((juxt take  #(drop (inc %1) %2)) fold xs)
       (map vec)))

(comment
  (split-on-fold 2 [1 2 3 4 5])
  ;;
  )

(defn rpad-xs
  "Append *nil* to xs so to reach a size of *len* and returns the result
   ```
   (rpad-xs [1 2 3] 4)
   => [1 2 3 nil]
   ```
   If len is lower than xs size, returns xs with no change
   "
  [xs len]
  (let [xs-len (count xs)]
    (if (< len xs-len)
      xs
      (into xs (repeat (- len xs-len) nil)))))


(defn merge-simple-val [v]
  (if (some nil? v)
    (some identity v)
    v))

(defn fold-xs [fold-index merge-fn xs]
  (let [[l1 l2] (split-on-fold fold-index xs)
        rl1     (vec (reverse l1))
        xs-len  (count xs)]
    (->> (map vector (rpad-xs rl1 xs-len) (rpad-xs l2 xs-len))
         (take-while #(not= [nil nil] %))
         (map merge-fn)
         reverse
         vec)))

(comment
  (let [[coords _]             (parse-data test-data)
        [col-count line-count] (paper-size coords)]
    (->> (create-transparent-paper col-count line-count)
         (mark-points coords)
         ;;cols->lines
         (fold-xs  2 merge-dot-xs )
         ;;
         ))

  (fold-xs  2 merge-simple-val [:a :b :c :d :e :f])
  (fold-xs  4 merge-simple-val [:a :b :c :d :e :f])

  ;; fold
  (let [l1 [:a :b :c]
        l2 [:e :f :g :h]
        max-len (+ (count l1) (count l2))]
    (->> (map vector (into l1 (repeat max-len  nil)) (into l2 (repeat  max-len  nil)))
         (take-while #(not= [nil nil] %))
         (map (fn [v]
                (if (some nil? v)
                  (some identity v)
                  v)))))

  ;;
  )


