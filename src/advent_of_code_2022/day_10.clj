(ns advent-of-code-2022.day-10
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop\n")
(def input (slurp "src/advent_of_code_2022/inputs/day10.txt"))

(defn crt
  [input]
  (let [lines (clojure.string/split-lines input)]
    (loop [X 1
           cycle-number 1
           line-number 0
           what-to-add nil
           signal-strength 0
           drawing ""]
      (let [new-signal-strength (if (= 20 (mod cycle-number 40))
                                  (+ signal-strength (* X cycle-number))
                                  signal-strength)
            pixel-n (mod (dec cycle-number) 40)
            sprite-aligned (or (= pixel-n (dec X))
                               (= pixel-n X)
                               (= pixel-n (inc X)))
            new-drawing (str drawing (if sprite-aligned
                                       "#"
                                       ;; replace "." with " " for easier reading
                                       "."))
            line (nth lines line-number)]
        (cond
          (= cycle-number 240)
          [new-signal-strength new-drawing]

          (not (nil? what-to-add))
          (recur (+ X what-to-add) (inc cycle-number) (inc line-number) nil new-signal-strength new-drawing)

          (= line "noop")
          (recur X (inc cycle-number) (inc line-number) nil new-signal-strength new-drawing)

          ;; addx
          :else
          (recur X (inc cycle-number) line-number (read-string (subs line 5)) new-signal-strength new-drawing))))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 13140))}
  [input]
  (first (crt input)))

(defn solve-b
  [input]
  (->> input
       (crt )
       (second)
       (partition 40)
       (map (partial apply str))))

(comment
  (solve-a input)
  ; 14720

  (solve-b input)
  ; FZBPBFZF
  )


