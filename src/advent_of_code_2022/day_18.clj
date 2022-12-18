(ns advent-of-code-2022.day-18
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "2,2,2\n1,2,2\n3,2,2\n2,1,2\n2,3,2\n2,2,1\n2,2,3\n2,2,4\n2,2,6\n1,2,5\n3,2,5\n2,1,5\n2,3,5")
(def input (slurp "src/advent_of_code_2022/inputs/day18.txt"))

(defn parse-input
  [input]
  (->> input
       (clojure.string/split-lines)
       (map (fn [line]
              (->> line
                   (re-seq #"\d+")
                   (map read-string)
                   (into []))))
       (into #{})))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 64))}
  [input]
  (let [points (parse-input input)]
    (reduce (fn [a point]
              (reduce (fn [a d]
                        (if (contains? points (mapv + point d))
                          a
                          (inc a)))
                      a
                      [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))
            0
            points)))

(defn get-outside-points
  [droplet-points]
  (let [[min-x max-x min-y max-y min-z max-z] (reduce (fn [[min-x max-x min-y max-y min-z max-z] [x y z]]
                                                        [(min min-x x) (max max-x x) (min min-y y) (max max-y y) (min min-z z) (max max-z z)])
                                                      [##Inf ##-Inf ##Inf ##-Inf ##Inf ##-Inf]
                                                      droplet-points)
        perimeter-points (into #{} (for [x (range (dec min-x) (+ max-x 2))
                                         y (range (dec min-y) (+ max-y 2))
                                         z (range (dec min-z) (+ max-z 2))
                                         :when (or (= x (dec min-x)) (= x (inc max-x))
                                                   (= y (dec min-y)) (= y (inc max-y))
                                                   (= z (dec min-z)) (= z (inc max-z)))]
                                     [x y z]))]
    (loop [outside-points perimeter-points
           points-to-check perimeter-points]
      (if (empty? points-to-check)
        outside-points
        (let [next-points-to-check (reduce (fn [a p]
                                             (reduce (fn [a d]
                                                       (let [new-p (mapv + p d)
                                                             [x y z] new-p]
                                                         (if (and (not (contains? droplet-points new-p))
                                                                  (not (contains? outside-points new-p))
                                                                  (>= x min-x)
                                                                  (<= x max-x)
                                                                  (>= y min-y)
                                                                  (<= y max-y)
                                                                  (>= z min-z)
                                                                  (<= z max-z))
                                                           (conj a new-p)
                                                           a)))
                                                     a
                                                     [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))
                                           #{}
                                           points-to-check)]
          (recur (clojure.set/union outside-points next-points-to-check) next-points-to-check))))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 58))}
  [input]
  (let [droplet-points (parse-input input)
        outside-points (get-outside-points droplet-points)]
    (reduce (fn [a droplet-point]
              (reduce (fn [a d]
                        (if (contains? outside-points (mapv + droplet-point d))
                          (inc a)
                          a))
                      a
                      [[1 0 0] [-1 0 0] [0 1 0] [0 -1 0] [0 0 1] [0 0 -1]]))
            0
            droplet-points)))

(comment
  (solve-a input)
  ; 4474

  (solve-b input)
  ; 2518
  )
