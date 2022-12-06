(ns advent-of-code-2022.day-06
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def input (slurp "src/advent_of_code_2022/inputs/day06.txt"))

(defn detect-start-of
  [input length]
  (reduce-kv (fn [buffer i letter]
               (if (= length (count (into #{} buffer)))
                 (reduced i)
                 (take length (conj buffer letter))))
             (list)
             (into [] (clojure.string/split input #""))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 7))}
  [input]
  (detect-start-of input 4))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 19))}
  [input]
  (detect-start-of input 14))

(comment
  (solve-a input)
  ; 1361

  (solve-b input)
  ; 3263
  )
