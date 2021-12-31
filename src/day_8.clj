(ns day-8
  (:require [clojure.string :as str]))


;; part 1 ==========================

(def test-data "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")


(defn read-data [s]
  (->> (str/split-lines s)
       (map (fn [line] (let [[sig-patterns output-digits] (str/split line #"\|")]
                         [(str/split (str/trim sig-patterns)  #" ")
                          (str/split (str/trim output-digits) #" ")])))))

(defn non-ambigous-digits? [s]
  (contains? #{2 3 4 7} (count s)))

(defn solve-part-1 [data]
  (->> data
       (map second)
       (apply concat)
       (filter non-ambigous-digits?)
       count))

(comment
  (solve-part-1 (read-data test-data))
  ;; 26
  (solve-part-1 (read-data (slurp "./resources/puzzle_8.txt")))
  ;; 264 
  )

;; part 2 ==========================

;; line 1 from test data
;; sig-patterns : "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb"
;; non ambigous :
;; cfbegad = 8 (len 7)
;; be      = 1 (len 2)
;; edb     = 7 (len 3)
;; cgeb    = 4 (len 4)


;; assign an identifier to each segment
;;  1111  
;; 2    3
;; 2    3
;;  4444
;; 5    6
;; 5    6
;;  7777

;; define 
(def display
  "mapping between ordered segment list and the digit displayed
   by the seven-segment display" {[1 2 3 5 6 7]   0
                                  [3 6]           1
                                  [1 3 4 5 7]     2
                                  [1 3 4 6 7]     3
                                  [2 3 4 6]       4
                                  [1 2 4 6 7]     5
                                  [1 2 4 5 6 7]   6
                                  [1 3 6]         7
                                  [1 2 3 4 5 6 7] 8})

(defn render-display
  "given un ordered list of segment id, returns the displayed digit"
  [xs]
  (get display xs))

(comment
  (render-display [1 2 4 6 7]))

(defn segment-6 [s m]
  (assoc m 6 (->> (seq s)
                  (remove #{\space})
                  frequencies
                  (apply max-key val)
                  key)))
(def s1 "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab")

(comment
  (segment-6 s1 {}))

(defn length= [n s]
  (filter #(= n (count %)) (str/split s #" ")))

(comment
  (first (length= 2 s1)))

(defn segment-3 [s m]
  (assoc m 3 (let [sig-1 (first (length= 2 s))]
               (if (= (first sig-1) (get m 6))
                 (second sig-1)
                 (first  sig-1)))))

(comment
  (segment-3 s1 (segment-6 s1 {})))

(defn segment-1 [s m]
  (assoc m 1 (let [sig-7     (first (length= 3 s))
                   assigned? (into #{} (vals m))]
               (first (remove assigned? sig-7)))))

(comment
  (segment-1 s1 {6 \b, 3 \a}))

;;  1111  
;; 2    3
;; 2    3
;;  4444
;; 5    6
;; 5    6
;;  7777

(defn segment-7 [s m]
  (assoc m 7 (let [sig-1     (first (length= 2 s))
                   sig-4     (first (length= 4 s))
                   sig-7     (first (length= 3 s))
                   sig-1-4-7 (into #{} (str sig-1 sig-4 sig-7))
                   sig-len-6 (length= 6 s)]
               (->> (map (fn [s] (remove sig-1-4-7 s)) sig-len-6)
                    (filter #(= 1 (count %)))
                    ffirst))))

(defn segment-4 [s m]
  (assoc m 4 (let [sig-len-5    (length= 5 s)
                   segm-1-3-6-7 (into #{} (vals m))]
               (->> (map #(remove segm-1-3-6-7 %) sig-len-5)
                    (filter #(= 1 (count %)))
                    ffirst))))

(defn segment-2 [s m]
  (assoc m 2 (let [sig-len-5    (length= 5 s)
                   segm-1-4-6-7 (->> m
                                     (filter (fn [[seg _]] (#{1 4 6 7} seg)))
                                     (map second)
                                     (into #{}))]
               (->> (map #(remove segm-1-4-6-7 %) sig-len-5)
                    (filter #(= 1 (count %)))
                    flatten
                    (remove (into #{} (vals m)))
                    first))))

(defn segment-5 [_ m]
  (assoc m 5 (let [sigs (into #{} (vals m))]
               (->> (remove sigs "abcdefg")
                    first))))

(def segments (->> {}
                   (segment-6 s1)
                   (segment-3 s1)
                   (segment-1 s1)
                   (segment-7 s1)
                   (segment-4 s1)
                   (segment-2 s1)
                   (segment-5 s1)))

(defn sig->segm [m c]
  (first (keep #(when (= (val %) c)
                  (key %)) m)))
(comment
  (sig->segm segments \d)
  (sig->segm segments \a)
  ;;
  )

(defn display-sig [segments sigs]
  (->> (map #(sig->segm segments %) sigs)
       sort
       render-display))

(comment
  (display-sig segments "cdfeb")
  (display-sig segments "fcadb")
  (display-sig segments "cdbaf"))



