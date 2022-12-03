(ns advent-of-code-2022.day-03
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw")
(def input (slurp "src/advent_of_code_2022/inputs/day03.txt"))

(def priorities (reduce-kv (fn [a i v]
                             (assoc a v i))
                           {}
                           (into [] (flatten (partition 1 "0abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 157))}
  [input]
  (reduce (fn [a line]
            (let [[first-half second-half] (map set (split-at (/ (count line) 2) line))]
              (reduce + a (map priorities
                               (clojure.set/intersection first-half second-half)))))
          0
          (clojure.string/split-lines input)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 70))}
  [input]
  (reduce (fn [a group]
            (+ a (-> (apply clojure.set/intersection (map (fn [line]
                                                            (set (flatten (partition 1 line))))
                                                          group))
                     (first)
                     (priorities))))
          0
          (partition 3 (clojure.string/split-lines input))))

(comment
  (solve-a input)
  ; 7811

  (solve-b input)
  ; 2639
  )
