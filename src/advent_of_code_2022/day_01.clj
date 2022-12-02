(ns advent-of-code-2022.day-01
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2022/inputs/day01.txt"))
(def test-input "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000")

(defn get-sum-of-top-n
  [input n]
  (->> (clojure.string/split input #"\n\n")
       (map (fn [elf]
              (->> (clojure.string/split-lines elf)
                   (map read-string)
                   (reduce +))))
       (sort)
       (reverse)
       (take n)
       (reduce +)))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 24000))}
  [input]
  (get-sum-of-top-n input 1))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 45000))}
  [input]
  (get-sum-of-top-n input 3))

(comment
  (solve-a input)
  ; 71780

  (solve-b input)
  ; 212489
  )
