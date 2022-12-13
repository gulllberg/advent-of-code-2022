(ns advent-of-code-2022.day-13
  (:require [ysera.test :refer [is= is is-not]]
            [ysera.collections :refer [index-of]]))

(def test-input "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]")
(def input (slurp "src/advent_of_code_2022/inputs/day13.txt"))

;; return negative if correct (left smaller), 0 if undetermined, positive if wrong (right smaller)
(defn compare-left-right
  [left right]
  (let [left-is-list (vector? left)
        right-is-list (vector? right)]
    (cond
      ;; two numbers
      (and (not left-is-list)
           (not right-is-list))
      (- left right)

      ;; left is number
      (not left-is-list)
      (compare-left-right [left] right)

      ;; right is number
      (not right-is-list)
      (compare-left-right left [right])

      ;; two lists
      :else
      (loop [i 0]
        (let [left-item (nth left i nil)
              right-item (nth right i nil)]
          (cond
            ;; both ran out of items - undetermined
            (and (nil? left-item)
                 (nil? right-item))
            0

            ;; left ran out of items - correct
            (nil? left-item)
            -1

            ;; right ran out of items - wrong
            (nil? right-item)
            1

            :else
            (let [result (compare-left-right left-item right-item)]
              (if (zero? result)
                (recur (inc i))
                result))))))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 13))}
  [input]
  (let [pairs (->> (clojure.string/split input #"\n\n")
                   (map (fn [two-rows]
                          (->> (clojure.string/split-lines two-rows)
                               (map read-string)))))]
    (reduce (fn [a i]
              (let [[left right] (nth pairs i)]
                (if (neg? (compare-left-right left right))
                  (+ a (inc i))
                  a)))
            0
            (range (count pairs)))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 140))}
  [input]
  (let [packets (as-> input $
                      (clojure.string/replace $ "\n\n" "\n")
                      (clojure.string/split-lines $)
                      (map read-string $)
                      (concat $ [[[2]] [[6]]]))
        sorted-packets (sort compare-left-right packets)]
    (* (inc (index-of sorted-packets [[2]]))
       (inc (index-of sorted-packets [[6]])))))

(comment
  (solve-a input)
  ; 6656

  (solve-b input)
  ; 19716
  )
