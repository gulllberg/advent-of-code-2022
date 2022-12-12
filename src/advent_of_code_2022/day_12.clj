(ns advent-of-code-2022.day-12
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi")
(def input (slurp "src/advent_of_code_2022/inputs/day12.txt"))

(defn parse-input
  [input start-char start-char-height goal-chars goal-chars-height]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [a i]
              (let [line (map identity (nth lines i))]
                (reduce (fn [a j]
                          (let [c (nth line j)]
                            (cond
                              (= c start-char)
                              (-> a
                                  (assoc :start [i j])
                                  (update :heights assoc [i j] start-char-height))

                              (contains? goal-chars c)
                              (-> a
                                  (update :goals conj [i j])
                                  (update :heights assoc [i j] goal-chars-height))

                              :else
                              (update a :heights assoc [i j] (int c)))))
                        a
                        (range (count line)))))
            {:start   nil
             :goals   #{}
             :heights {}}
            (range (count lines)))))

(defn get-connections
  [heights descending]
  (reduce-kv (fn [connections k h]
               (let [up (mapv + k [-1 0])
                     down (mapv + k [1 0])
                     left (mapv + k [0 -1])
                     right (mapv + k [0 1])]
                 (assoc connections k (filter (fn [c]
                                                (and (contains? heights c)
                                                     (if descending
                                                       (<= h (inc (get heights c)))
                                                       (>= h (dec (get heights c))))))
                                              [up down left right]))))
             {}
             heights))

(defn find-shortest-path
  [connections start goals]
  (loop [[paths-to-test visited] [#{(list start)} #{start}]]
    (if-let [finished-path (first (filter (fn [p]
                                            (contains? goals (first p)))
                                          paths-to-test))]
      (dec (count finished-path))
      (recur (reduce (fn [[new-paths visited] path]
                       (let [new-cs (->> (get connections (first path))
                                         (remove (fn [c]
                                                   (contains? visited c))))]
                         [(reduce conj new-paths (map (fn [c]
                                                        (conj path c))
                                                      new-cs))
                          (reduce conj visited new-cs)]))
                     [#{} visited]
                     paths-to-test)))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 31))}
  [input]
  (let [{start :start goals :goals heights :heights} (parse-input input \S (int \a) #{\E} (int \z))]
    (find-shortest-path (get-connections heights false) start goals )))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 29))}
  [input]
  (let [{start :start goals :goals heights :heights} (parse-input input \E (int \z) #{\S \a} (int \a))]
    (find-shortest-path (get-connections heights true) start goals))
  )

(comment
  (solve-a input)
  ; 456

  (solve-b input)
  ; 454
  )
