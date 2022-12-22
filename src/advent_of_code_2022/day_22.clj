(ns advent-of-code-2022.day-22
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "        ...#\n        .#..\n        #...\n        ....\n...#.......#\n........#...\n..#....#....\n..........#.\n        ...#....\n        .....#..\n        .#......\n        ......#.\n\n10R5L5R10L4R5L5")
(def input (slurp "src/advent_of_code_2022/inputs/day22.txt"))

(defn parse-input
  [input]
  (let [[board-input instructions-input] (clojure.string/split input #"\n\n")
        board-lines (clojure.string/split-lines board-input)
        board (reduce (fn [a i]
                        (let [line (clojure.string/split (nth board-lines i) #"")]
                          (reduce (fn [a j]
                                    (let [letter (nth line j)]
                                      (condp = letter
                                        "#" (assoc a [(inc i) (inc j)] :wall)
                                        "." (assoc a [(inc i) (inc j)] :path)
                                        " " (assoc a [(inc i) (inc j)] :void)
                                        (println "invalid character!"))))
                                  a
                                  (range (count line)))))
                      {}
                      (range (count board-lines)))
        instructions (map (fn [i]
                            (if (re-find #"\d+" i)
                              (read-string i)
                              i))
                          (re-seq #"\d+|[RL]" instructions-input))]
    [board instructions]))

(defn get-starting-position
  [board]
  (reduce (fn [_ j]
            (when (= :path (get board [1 j]))
              (reduced [1 j])))
          nil
          (range)))

(defn facing->movement
  [facing]
  (condp = facing
    :right [0 1]
    :up [-1 0]
    :left [0 -1]
    :down [1 0]))

(defn apply-rotation
  [facing rotation]
  (if (= rotation "R")
    (condp = facing
      :right :down
      :up :right
      :left :up
      :down :left)
    (condp = facing
      :right :up
      :up :left
      :left :down
      :down :right)))

(defn get-wrap-around-position-and-facing-flat
  [board position facing]
  (let [[static-position-picking-fn
         dynamic-position-picking-fn
         min-max-fn
         pos-creating-fn]
        (condp = facing
          :up [second first max (fn [d s] [d s])]
          :left [first second max (fn [d s] [s d])]
          :down [second first min (fn [d s] [d s])]
          :right [first second min (fn [d s] [s d])])]
    [(reduce (fn [a p]
               (cond
                 (not= (static-position-picking-fn a) (static-position-picking-fn p))
                 a

                 (= :void (get board p))
                 a

                 ; Same row/column and wall/path
                 :else
                 (pos-creating-fn (min-max-fn (dynamic-position-picking-fn a) (dynamic-position-picking-fn p)) (static-position-picking-fn a))))
             position
             (keys board))
     facing]))

; . 2 1
; . 3 .
; 5 4 .
; 6 . .
(defn get-face-number
  [position]
  (let [[row column] position]
    (cond
      (and (<= 1 row 50) (<= 101 column 150)) 1
      (and (<= 1 row 50) (<= 51 column 100)) 2
      (and (<= 51 row 100) (<= 51 column 100)) 3
      (and (<= 101 row 150) (<= 51 column 100)) 4
      (and (<= 101 row 150) (<= 1 column 50)) 5
      (and (<= 151 row 200) (<= 1 column 50)) 6)))

(defn get-wrap-around-position-and-facing-cube
  [position facing]
  (let [face-number (get-face-number position)
        [row column] position]
    (cond
      ; yeah...
      (and (= face-number 1) (= facing :up))
      ; [6 :up]
      [[200 (- column 100)] :up]

      (and (= face-number 1) (= facing :right))
      ; [4 :left]
      [[(- 151 row) 100] :left]

      (and (= face-number 1) (= facing :down))
      ; [3 :left]
      [[(- column 50) 100] :left]

      (and (= face-number 2) (= facing :up))
      ; [6 :right]
      [[(+ 100 column) 1] :right]

      (and (= face-number 2) (= facing :left))
      ; [5 :right]
      [[(- 151 row) 1] :right]

      (and (= face-number 3) (= facing :left))
      ; [5 :down]
      [[101 (- row 50)] :down]

      (and (= face-number 3) (= facing :right))
      ; [1 :up]
      [[50 (+ 50 row)] :up]

      (and (= face-number 4) (= facing :right))
      ; [1 :left]
      [[(- 151 row) 150] :left]

      (and (= face-number 4) (= facing :down))
      ; [6 :left]
      [[(+ column 100) 50] :left]

      (and (= face-number 5) (= facing :up))
      ; [3 :right]
      [[(+ column 50) 51] :right]

      (and (= face-number 5) (= facing :left))
      ; [2 :right]
      [[(- 151 row) 51] :right]

      (and (= face-number 6) (= facing :right))
      ; [4 :up]
      [[150 (- row 100)] :up]

      (and (= face-number 6) (= facing :down))
      ; [1 :down]
      [[1 (+ column 100)] :down]

      (and (= face-number 6) (= facing :left))
      ; [2 :down]
      [[1 (- row 100)] :down])))

(defn get-wrap-around-position-and-facing
  [board position facing is-cube]
  (if is-cube
    (get-wrap-around-position-and-facing-cube position facing)
    (get-wrap-around-position-and-facing-flat board position facing)))

(defn get-position-and-facing-in-front
  [board position facing is-cube]
  (let [in-front-position (mapv + position (facing->movement facing))
        in-front (get board in-front-position :void)]
    (if (contains? #{:path :wall} in-front)
      [in-front-position facing]
      (get-wrap-around-position-and-facing board position facing is-cube))))

(defn move
  [board position facing is-cube n-moves]
  (reduce (fn [[position facing] _]
            (let [[new-position new-facing] (get-position-and-facing-in-front board position facing is-cube)
                  can-move-forward (= :path (get board new-position))]
              (if can-move-forward
                [new-position new-facing]
                (reduced [position facing]))))
          [position facing]
          (range n-moves)))

(defn get-score
  [position facing]
  (let [facing-score (condp = facing
                       :right 0
                       :down 1
                       :left 2
                       :up 3)
        [row column] position]
    (+ (* 1000 row) (* 4 column) facing-score)))

(defn solve
  [input is-cube]
  (let [[board instructions] (parse-input input)
        [position facing] (reduce (fn [[position facing] instruction]
                                    (if (number? instruction)
                                      (move board position facing is-cube instruction)
                                      [position (apply-rotation facing instruction)]))
                                  [(get-starting-position board) :right]
                                  instructions)]
    (get-score position facing)))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 6032))}
  [input]
  (solve input false))

(defn solve-b
  ;; Test and real input have different layouts
  ;{:test (fn []
  ;         (is= (solve-b test-input) 5031))}
  [input]
  (solve input true))

(comment
  (solve-a input)
  ; 58248

  (solve-b input)
  ; 179091
  )
