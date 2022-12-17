(ns advent-of-code-2022.day-17
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")
(def input (slurp "src/advent_of_code_2022/inputs/day17.txt"))

(defn can-move-left?
  [occupied shape position]
  (let [[x y] position]
    (condp = shape
      :minus (and (> x 1)
                  (not (contains? occupied [(dec x) y])))

      :plus (and (> x 1)
                 (not (contains? occupied [(dec x) (inc y)]))
                 (not (contains? occupied [x y]))
                 (not (contains? occupied [x (+ y 2)])))

      :angle (and (> x 1)
                  (not (contains? occupied [(dec x) y]))
                  (not (contains? occupied [(inc x) (inc y)]))
                  (not (contains? occupied [(inc x) (+ y 2)])))

      :pole (and (> x 1)
                 (not (contains? occupied [(dec x) y]))
                 (not (contains? occupied [(dec x) (inc y)]))
                 (not (contains? occupied [(dec x) (+ y 2)]))
                 (not (contains? occupied [(dec x) (+ y 3)])))

      :square (and (> x 1)
                   (not (contains? occupied [(dec x) y]))
                   (not (contains? occupied [(dec x) (inc y)]))))))

(defn can-move-right?
  [occupied shape position]
  (let [[x y] position]
    (condp = shape
      :minus (and (< x 4)
                  (not (contains? occupied [(+ x 4) y])))

      :plus (and (< x 5)
                 (not (contains? occupied [(+ x 3) (inc y)]))
                 (not (contains? occupied [(+ x 2) y]))
                 (not (contains? occupied [(+ x 2) (+ y 2)])))

      :angle (and (< x 5)
                  (not (contains? occupied [(+ x 3) y]))
                  (not (contains? occupied [(+ x 3) (inc y)]))
                  (not (contains? occupied [(+ x 3) (+ y 2)])))

      :pole (and (< x 7)
                 (not (contains? occupied [(inc x) y]))
                 (not (contains? occupied [(inc x) (inc y)]))
                 (not (contains? occupied [(inc x) (+ y 2)]))
                 (not (contains? occupied [(inc x) (+ y 3)])))

      :square (and (< x 6)
                   (not (contains? occupied [(+ x 2) y]))
                   (not (contains? occupied [(+ x 2) (inc y)]))))))

(defn can-move-down?
  [occupied shape position]
  (let [[x y] position]
    (condp = shape
      :minus (and (not (contains? occupied [x (dec y)]))
                  (not (contains? occupied [(inc x) (dec y)]))
                  (not (contains? occupied [(+ x 2) (dec y)]))
                  (not (contains? occupied [(+ x 3) (dec y)])))

      :plus (and (not (contains? occupied [x y]))
                 (not (contains? occupied [(inc x) (dec y)]))
                 (not (contains? occupied [(+ x 2) y])))

      :angle (and (not (contains? occupied [x (dec y)]))
                  (not (contains? occupied [(inc x) (dec y)]))
                  (not (contains? occupied [(+ x 2) (dec y)])))

      :pole (not (contains? occupied [x (dec y)]))

      :square (and (not (contains? occupied [x (dec y)]))
                   (not (contains? occupied [(inc x) (dec y)]))))))

(defn apply-jet
  [occupied shape position jet]
  (let [is-right (= jet ">")]
    (if is-right
      (if (can-move-right? occupied shape position)
        (mapv + position [1 0])
        position)
      (if (can-move-left? occupied shape position)
        (mapv + position [-1 0])
        position))))

(defn get-new-occupied
  [occupied shape position]
  (condp = shape
    :minus (clojure.set/union occupied #{position
                                         (mapv + position [1 0])
                                         (mapv + position [2 0])
                                         (mapv + position [3 0])})

    :plus (clojure.set/union occupied #{(mapv + position [0 1])
                                        (mapv + position [1 0])
                                        (mapv + position [1 1])
                                        (mapv + position [1 2])
                                        (mapv + position [2 1])})

    :angle (clojure.set/union occupied #{position
                                         (mapv + position [1 0])
                                         (mapv + position [2 0])
                                         (mapv + position [2 1])
                                         (mapv + position [2 2])})

    :pole (clojure.set/union occupied #{position
                                        (mapv + position [0 1])
                                        (mapv + position [0 2])
                                        (mapv + position [0 3])})

    :square (clojure.set/union occupied #{position
                                          (mapv + position [0 1])
                                          (mapv + position [1 0])
                                          (mapv + position [1 1])})))

(defn get-new-tower-height
  [tower-height shape position]
  (max tower-height
       (+ (second position)
          -1
          (condp = shape
            :minus 1
            :plus 3
            :angle 3
            :pole 4
            :square 2))))

(defn draw
  [occupied tower-height]
  (doseq [y (range tower-height 0 -1)]
    (println (apply str (map (fn [x]
                               (if (contains? occupied [x y])
                                 "#"
                                 "."))
                             (range 1 8))))))

(defn tetris
  [input n-rocks]
  (let [jets (clojure.string/split (clojure.string/replace input #"\n" "") #"")
        shapes [:minus :plus :angle :pole :square]]
    (loop [jet-n 0
           shape-n 0
           tower-height 0
           position nil
           occupied #{[1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0]}]
      (if (= shape-n n-rocks)
        tower-height
        (let [jet (nth jets (mod jet-n (count jets)))
              shape (nth shapes (mod shape-n 5))
              position (if position position [3 (+ tower-height 4)])
              position (apply-jet occupied shape position jet)]
          (if (can-move-down? occupied shape position)
            (recur (inc jet-n)
                   shape-n
                   tower-height
                   (mapv + position [0 -1])
                   occupied)
            (recur (inc jet-n)
                   (inc shape-n)
                   (get-new-tower-height tower-height shape position)
                   nil
                   (get-new-occupied occupied shape position))))))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 3068))}
  [input]
  (tetris input 2022))


(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 1514285714288))}
  [input]
  ;(tetris input 1000000000000)
  )

(comment
  (time (solve-a input))
  ; 3235

  (solve-b input)
  ;
  )
