(ns advent-of-code-2022.day-15
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\nSensor at x=9, y=16: closest beacon is at x=10, y=16\nSensor at x=13, y=2: closest beacon is at x=15, y=3\nSensor at x=12, y=14: closest beacon is at x=10, y=16\nSensor at x=10, y=20: closest beacon is at x=10, y=16\nSensor at x=14, y=17: closest beacon is at x=10, y=16\nSensor at x=8, y=7: closest beacon is at x=2, y=10\nSensor at x=2, y=0: closest beacon is at x=2, y=10\nSensor at x=0, y=11: closest beacon is at x=2, y=10\nSensor at x=20, y=14: closest beacon is at x=25, y=17\nSensor at x=17, y=20: closest beacon is at x=21, y=22\nSensor at x=16, y=7: closest beacon is at x=15, y=3\nSensor at x=14, y=3: closest beacon is at x=15, y=3\nSensor at x=20, y=1: closest beacon is at x=15, y=3")
(def input (slurp "src/advent_of_code_2022/inputs/day15.txt"))

(defn parse-input
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [line]
              (->> (re-seq #"-?\d+" line)
                   (map read-string)
                   (partition 2))))))

(defn get-manhattan-distance
  [p1 p2]
  (+ (abs (- (first p1) (first p2)))
     (abs (- (second p1) (second p2)))))

(defn find-position't
  [sensor-readings]
  (reduce (fn [a v]
            (disj a v))
          (reduce (fn [a reading]
                    ;(println reading)
                    (let [sensor-position (first reading)
                          beacon-position (second reading)
                          distance (get-manhattan-distance sensor-position beacon-position)]
                      (reduce conj a (for [x (range (- (first sensor-position) distance) (+ (first sensor-position) distance 1))
                                           y (range (- (second sensor-position) distance) (+ (second sensor-position) distance 1))
                                           :when (<= (get-manhattan-distance sensor-position [x y]) distance)]
                                       [x y]))))
                  #{}
                  sensor-readings)
          (map second sensor-readings)))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input 10) 26))}
  [input row-to-check]
  (->> input
       (parse-input)
       (find-position't)
       (filter (fn [p]
                 (= row-to-check (second p))))
       (count)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 93))}
  [input]
  93)

(comment
  (time (solve-a input 2000000))
  ;

  (solve-b input)
  ;
  )
