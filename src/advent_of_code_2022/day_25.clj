(ns advent-of-code-2022.day-25
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "1=-0-2\n12111\n2=0=\n21\n2=01\n111\n20012\n112\n1=-1=\n1-12\n12\n1=\n122")
(def input (slurp "src/advent_of_code_2022/inputs/day25.txt"))

(defn line->decimal
  [line]
  (let [letters (->> line
                     (re-seq #"0|1|2|-|=")
                     (reverse)
                     (into []))]
    (reduce-kv (fn [a i l]
                 (let [n (condp = l
                           "0" 0
                           "1" 1
                           "2" 2
                           "-" -1
                           "=" -2)]
                   (+ a (* (long (Math/pow 5 i)) n))))
               0
               letters)))

(defn decimal->snafu-character
  [decimal-character]
  (get {-2 "="
        -1 "-"
        0  "0"
        1  "1"
        2  "2"} decimal-character))

(defn decimal->snafu
  [decimal]
  (let [[position-values position-max-total-values] (reduce (fn [[position-values position-max-total-value] i]
                                                              (let [position-value (long (Math/pow 5 i))]
                                                                [(conj position-values position-value)
                                                                 (conj position-max-total-value (+ (or (first position-max-total-value) 0) (* 2 position-value)))]))
                                                            [(list) (list)]
                                                            (range 25))
        snafu-number-length (reduce (fn [_ i]
                                      (when (> decimal (nth position-max-total-values (inc i)))
                                        (reduced (- 25 i))))
                                    nil
                                    (range 25))
        position-values (drop (- 25 snafu-number-length) position-values)
        position-max-total-values (drop (- 25 snafu-number-length) position-max-total-values)
        snafu-list (reduce (fn [[snafu-list remaining-decimal] i]
                             (if (= i (dec snafu-number-length))
                               (conj snafu-list (decimal->snafu-character remaining-decimal))
                               (let [position-value (nth position-values i)
                                     next-position-max-value (nth position-max-total-values (inc i))
                                     number (reduce (fn [_ n]
                                                      (let [value (* position-value n)]
                                                        (when (<= (- next-position-max-value) (- remaining-decimal value) next-position-max-value)
                                                          (reduced n))))
                                                    nil
                                                    [-2 -1 0 1 2])]
                                 [(conj snafu-list (decimal->snafu-character number)) (- remaining-decimal (* position-value number))])))
                           [[] decimal]
                           (range snafu-number-length))]
    (apply str snafu-list)))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) "2=-1=0"))}
  [input]
  (let [lines (clojure.string/split-lines input)]
    (decimal->snafu (reduce (fn [a line]
                              (+ a (line->decimal line)))
                            0
                            lines))))

(comment
  (solve-a input)
  ; 2-02===-21---2002==0
  )
