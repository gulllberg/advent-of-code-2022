(ns advent-of-code-2022.day-24
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "#.######\n#>>.<^<#\n#.<..<<#\n#>v.><>#\n#<^v^^>#\n######.#")
(def input (slurp "src/advent_of_code_2022/inputs/day24.txt"))

(defn parse-input
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [a i]
              (let [line (clojure.string/split (nth lines i) #"")]
                (reduce (fn [[points blizzards] j]
                          (let [letter (nth line j)]
                            (condp = letter
                              "#" [points blizzards]
                              "." [(conj points [i j]) blizzards]
                              "^" [(conj points [i j]) (conj blizzards [[i j] [-1 0]])]
                              ">" [(conj points [i j]) (conj blizzards [[i j] [0 1]])]
                              "v" [(conj points [i j]) (conj blizzards [[i j] [1 0]])]
                              "<" [(conj points [i j]) (conj blizzards [[i j] [0 -1]])])))
                        a
                        (range (count line)))))
            [#{} #{}]
            (range (count lines)))))

(defn get-starting-point
  [points]
  (reduce (fn [_ [i j]]
            (when (= i 0)
              (reduced [i j])))
          nil
          points))

(defn get-goal-point
  [points]
  (reduce (fn [[g-i g-j] [i j]]
            (if (> i g-i)
              [i j]
              [g-i g-j]))
          [0 0]
          points))

(defn get-wrap-around-position
  [points position direction]
  (let [[static-position-picking-fn
         dynamic-position-picking-fn
         min-max-fn
         pos-creating-fn]
        (condp = direction
          [-1 0] [second first max (fn [d s] [d s])]
          [0 -1] [first second max (fn [d s] [s d])]
          [1 0] [second first min (fn [d s] [d s])]
          [0 1] [first second min (fn [d s] [s d])])]
     (reduce (fn [a p]
                 (if (not= (static-position-picking-fn a) (static-position-picking-fn p))
                   a
                   ; Same row/column
                   (pos-creating-fn (min-max-fn (dynamic-position-picking-fn a) (dynamic-position-picking-fn p)) (static-position-picking-fn a))))
               position
             points)))

(defn get-to-goal
  [points blizzards starting-point goal-points]
  (loop [iter-n 0
         positions #{starting-point}
         blizzards blizzards
         goal-points goal-points]
    (let [goal-point (first goal-points)]
      (cond
        (nil? goal-point)
        iter-n

        (contains? positions goal-point)
        (recur iter-n #{goal-point} blizzards (drop 1 goal-points))

        :else
        (let [new-blizzards (reduce (fn [new-blizzards [position direction]]
                                      (let [new-position (mapv + position direction)]
                                        (if (contains? points new-position)
                                          (conj new-blizzards [new-position direction])
                                          (conj new-blizzards [(get-wrap-around-position points position direction) direction]))))
                                    #{}
                                    blizzards)
              blizzard-positions (into #{} (map first new-blizzards))
              new-positions (reduce (fn [new-positions position]
                                      (reduce (fn [new-positions direction]
                                                (let [new-position (mapv + position direction)]
                                                  (if (and (contains? points new-position)
                                                           (not (contains? blizzard-positions new-position)))
                                                    (conj new-positions new-position)
                                                    new-positions)))
                                              new-positions
                                              [[1 0] [0 1] [-1 0] [0 -1] [0 0]]))
                                    #{}
                                    positions)]
          (recur (inc iter-n) new-positions new-blizzards goal-points))))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 18))}
  [input]
  (let [[points blizzards] (parse-input input)
        starting-point (get-starting-point points)
        goal-point (get-goal-point points)]
    (get-to-goal points blizzards starting-point [goal-point])))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 54))}
  [input]
  (let [[points blizzards] (parse-input input)
        starting-point (get-starting-point points)
        goal-point (get-goal-point points)]
    (get-to-goal points blizzards starting-point [goal-point starting-point goal-point])))

(comment
  (time (solve-a input))
  ; 221
  ; "Elapsed time: 5460.504375 msecs"

  (time (solve-b input))
  ; 739
  ; "Elapsed time: 18222.491708 msecs"
  )
