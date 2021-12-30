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


