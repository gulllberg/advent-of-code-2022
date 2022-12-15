(ns advent-of-code-2022.day-14
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9")
(def input (slurp "src/advent_of_code_2022/inputs/day14.txt"))

(defn parse-input
  [input]
  (let [points (->> (clojure.string/split-lines input)
                    (map (fn [line]
                           (->> line
                                (re-seq #"\d+")
                                (map read-string)
                                (partition 2)))))
        min-c (apply min 500 (flatten (map (partial map first) points)))
        max-c (apply max 500 (flatten (map (partial map first) points)))
        min-r (apply min 0 (flatten (map (partial map second) points)))
        max-r (apply max 0 (flatten (map (partial map second) points)))
        empty-state (reduce (fn [a r]
                              (reduce (fn [a c]
                                        (assoc a [c r] :air))
                                      a
                                      (range min-c (inc max-c))))
                            {}
                            (range min-r (inc max-r)))
        state (reduce (fn [state line-points]
                        (loop [i 0
                               old-col nil
                               old-row nil
                               state state]
                          (let [point (nth line-points i nil)]
                            (cond
                              (nil? point)
                              state

                              (nil? old-row)
                              (recur (inc i) (first point) (second point) state)

                              :else
                              (recur (inc i) (first point) (second point)
                                     (reduce (fn [s r]
                                               (reduce (fn [s c]
                                                         (assoc s [c r] :rock))
                                                       s
                                                       (range (min old-col (first point)) (inc (max old-col (first point))))))
                                             state
                                             (range (min old-row (second point)) (inc (max old-row (second point))))))
                              ))))
                      empty-state
                      points)]
    [state (+ 2 max-r)]))

(defn one-sand-grain
  [state starting-point floor-level]
  (loop [sand-position starting-point]
    (let [missing-value (cond
                          (nil? floor-level) nil
                          (= floor-level (inc (second sand-position))) :floor
                          :else :air)]
      (cond
        ; check down
        (= :air (get state (mapv + sand-position [0 1]) missing-value))
        (recur (mapv + sand-position [0 1]))

        ; check down-left
        (= :air (get state (mapv + sand-position [-1 1]) missing-value))
        (recur (mapv + sand-position [-1 1]))

        ; check down-right
        (= :air (get state (mapv + sand-position [1 1]) missing-value))
        (recur (mapv + sand-position [1 1]))

        ; sand will fall out
        (or (nil? (get state (mapv + sand-position [0 1]) missing-value))
            (nil? (get state (mapv + sand-position [-1 1]) missing-value))
            (nil? (get state (mapv + sand-position [1 1]) missing-value)))
        state

        ; sand came to stop - all three down positions are rock or sand or floor
        :else
        (assoc state sand-position :sand)))))

(defn simulate-sand
  [state starting-point floor-level]
  (loop [state state]
    (let [new-state (one-sand-grain state starting-point floor-level)]
      (if (= new-state state)
        state
        (recur new-state)))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 24))}
  [input]
  (let [[state _] (parse-input input)
        state (simulate-sand state [500 0] nil)]
    (->> state
         (vals)
         (filter #{:sand})
         (count))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 93))}
  [input]
  (let [[state floor-level] (parse-input input)
        state (simulate-sand state [500 0] floor-level)]
    (->> state
         (vals)
         (filter #{:sand})
         (count))))

(comment
  (time (solve-a input))
  ; 805
  ; "Elapsed time: 572.253508 msecs"

  (time (solve-b input))
  ; 25161
  ; "Elapsed time: 13276.637445 msecs"

  ; Would be much faster to release all sand at once (one each tick) instead of waiting for each grain to stop before releasing the next
  )
