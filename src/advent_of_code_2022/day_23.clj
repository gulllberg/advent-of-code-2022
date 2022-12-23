(ns advent-of-code-2022.day-23
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "....#..\n..###.#\n#...#.#\n.#...##\n#.###..\n##.#.##\n.#..#..")
(def input (slurp "src/advent_of_code_2022/inputs/day23.txt"))

(defn parse-input
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [a row]
              (let [line (clojure.string/split (nth lines row) #"")]
                (reduce (fn [a column]
                          (if (= (nth line column) "#")
                            (conj a [row column])
                            a))
                        a
                        (range (count line)))))
            #{}
            (range (count lines)))))

(defn valid-move?
  [elves position move]
  (reduce (fn [_ d]
            (if (contains? elves (mapv + position d))
              (reduced false)
              true))
          true
          move))

(defn no-neighbours?
  [elves position]
  (valid-move? elves position [[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]))

(defn get-first-valid-move
  [elves position move-order i]
  (reduce (fn [_ j]
            (let [move (nth move-order (mod (+ i j) 4))]
              (if (valid-move? elves position move)
                (reduced move)
                nil)))
          nil
          (range 4)))

(defn move-elves
  [elves max-rounds]
  (let [move-order [; north
                    [[-1 -1] [-1 0] [-1 1]]
                    ;south
                    [[1 -1] [1 0] [1 1]]
                    ; west
                    [[1 -1] [0 -1] [-1 -1]]
                    ; east
                    [[1 1] [0 1] [-1 1]]]]
    (loop [elves elves
           i 0]
      (if (and max-rounds (= i max-rounds))
        elves
        (let [[proposals proposal-counts] (reduce (fn [[proposals proposal-counts] elf]
                                                    (cond
                                                      (no-neighbours? elves elf)
                                                      [proposals proposal-counts]

                                                      (get-first-valid-move elves elf move-order i)
                                                      (let [move (get-first-valid-move elves elf move-order i)
                                                            d (second move)
                                                            p (mapv + elf d)
                                                            old-proposal-count (get proposal-counts p 0)]
                                                        [(assoc proposals elf p) (assoc proposal-counts p (inc old-proposal-count))])

                                                      :else
                                                      [proposals proposal-counts]))
                                                  [{} {}]
                                                  elves)
              new-elves (reduce-kv (fn [new-elves elf proposal]
                                     (if (= 1 (get proposal-counts proposal))
                                       (-> new-elves
                                           (disj elf)
                                           (conj proposal))
                                       new-elves))
                                   elves
                                   proposals)]
          (if (= new-elves elves)
            (inc i)
            (recur new-elves (inc i))))))))

(defn get-n-empty-spaces
  [elves]
  (let [[min-row max-row min-col max-col] (reduce (fn [[min-row max-row min-col max-col] [row column]]
                                                    [(min min-row row) (max max-row row) (min min-col column) (max max-col column)])
                                                  [##Inf ##-Inf ##Inf ##-Inf]
                                                  elves)]
    (- (* (inc (- max-row min-row)) (inc (- max-col min-col))) (count elves))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 110))}
  [input]
  (let [elves (parse-input input)]
    (get-n-empty-spaces (move-elves elves 10))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 20))}
  [input]
  (let [elves (parse-input input)]
    (move-elves elves nil)))

(comment
  (solve-a input)
  ; 4241

  (time (solve-b input))
  ; 1079
  ; "Elapsed time: 9064.302709 msecs"
  )
