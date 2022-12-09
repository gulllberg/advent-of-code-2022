(ns advent-of-code-2022.day-08
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "30373\n25512\n65332\n33549\n35390")
(def input (slurp "src/advent_of_code_2022/inputs/day08.txt"))

(defn parse-input
  [input]
  (let [lines (clojure.string/split-lines input)
        num-rows (count lines)
        num-columns (count (first lines))]
    [(reduce (fn [a i]
               (let [line (clojure.string/split (nth lines i) #"")]
                 (reduce (fn [a j]
                           (let [height (read-string (nth line j))]
                             (assoc a [i j] height)))
                         a
                         (range num-columns))))
             {}
             (range num-rows))
     num-rows
     num-columns]))

(defn check-horizontal
  [heights i range-to-use]
  (first (reduce (fn [[visible largest-height] j]
                   (let [height (get heights [i j])]
                     (if (> height largest-height)
                       [(conj visible [i j]) height]
                       [visible largest-height])))
                 [#{} -1]
                 range-to-use)))

(defn check-vertical
  [heights j range-to-use]
  (first (reduce (fn [[visible largest-height] i]
                   (let [height (get heights [i j])]
                     (if (> height largest-height)
                       [(conj visible [i j]) height]
                       [visible largest-height])))
                 [#{} -1]
                 range-to-use)))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 21))}
  [input]
  (let [[heights num-rows num-columns] (parse-input input)]
    (count (reduce clojure.set/union
                   (concat (map (fn [i]
                                  (clojure.set/union (check-horizontal heights i (range num-columns))
                                                     (check-horizontal heights i (reverse (range num-columns)))))
                                (range num-rows))
                           (map (fn [j]
                                  (clojure.set/union (check-vertical heights j (range num-rows))
                                                     (check-vertical heights j (reverse (range num-rows)))))
                                (range num-columns)))))))

(defn get-viewing-distance
  [heights i j coords-to-check]
  (let [my-height (get heights [i j])]
    (reduce (fn [a [i j]]
              (let [distance (inc a)
                    height (get heights [i j])]
                (if (<= my-height height)
                  (reduced distance)
                  distance)))
            0
            coords-to-check)))

(defn get-scenic-score
  [heights num-rows num-columns i j]
  (let [up (get-viewing-distance heights i j (mapv vector (reverse (range i)) (repeat j)))
        down (get-viewing-distance heights i j (mapv vector (range (inc i) num-rows) (repeat j)))
        left (get-viewing-distance heights i j (mapv vector (repeat i) (reverse (range j))))
        right (get-viewing-distance heights i j (mapv vector (repeat i) (range (inc j) num-columns)))]
    (* up down left right)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 8))}
  [input]
  (let [[heights num-rows num-columns] (parse-input input)]
    (reduce (fn [a i]
              (reduce (fn [a j]
                        (max a (get-scenic-score heights num-rows num-columns i j)))
                      a
                      (range num-columns)))
            0
            (range num-rows))))

(comment
  (solve-a input)
  ; 1840

  (solve-b input)
  ; 405769
  )

