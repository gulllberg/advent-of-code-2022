(ns advent-of-code-2022.day-20
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "1\n2\n-3\n3\n-2\n0\n4")
(def input (slurp "src/advent_of_code_2022/inputs/day20.txt"))

(defn parse-input
  [input decryption-key]
  (->> input
       (clojure.string/split-lines)
       (into [])
       (reduce-kv (fn [a i line]
                    (assoc a i {:position i
                                :value    (* decryption-key (read-string line))}))
                  {})))

(defn mix-file
  [state times-to-mix]
  (let [file-length (count state)]
    (reduce (fn [state _]
              (reduce (fn [a k]
                        (let [position (get-in a [k :position])
                              value (get-in a [k :value])
                              ; The (dec file-length) for mod is counter-intuitive, but being first and last in the list is the same thing.
                              new-position (mod (+ position value) (dec file-length))
                              moved-down (< new-position position)
                              min-affected-position (if moved-down new-position (inc position))
                              max-affected-position (if moved-down (dec position) new-position)
                              affected-keys (filter (fn [kk]
                                                      (<= min-affected-position (get-in a [kk :position]) max-affected-position))
                                                    (keys state))]
                          (if (= position new-position)
                            a
                            (reduce (fn [aa kk]
                                      (update-in aa [kk :position] (if moved-down inc dec)))
                                    (assoc-in a [k :position] new-position)
                                    affected-keys))))
                      state
                      (sort (keys state))))
            state
            (range times-to-mix))))

(defn sum-grove-coordinates
  [state]
  (let [file-length (count state)
        position-of-0 (:position (first (filter (fn [v]
                                                  (= 0 (:value v)))
                                                (vals state))))
        relevant-position-1 (mod (+ position-of-0 1000) file-length)
        relevant-position-2 (mod (+ position-of-0 2000) file-length)
        relevant-position-3 (mod (+ position-of-0 3000) file-length)]
    (reduce (fn [a k]
              (+ a
                 (if (= relevant-position-1 (get-in state [k :position]))
                   (get-in state [k :value])
                   0)
                 (if (= relevant-position-2 (get-in state [k :position]))
                   (get-in state [k :value])
                   0)
                 (if (= relevant-position-3 (get-in state [k :position]))
                   (get-in state [k :value])
                   0)))
            0
            (keys state))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 3))}
  [input]
  (let [state (mix-file (parse-input input 1) 1)]
    (sum-grove-coordinates state)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 1623178306))}
  [input]
  (let [state (mix-file (parse-input input 811589153) 10)]
    (sum-grove-coordinates state)))

(comment
  (time (solve-a input))
  ; 2622
  ; "Elapsed time: 5274.746875 msecs"

  (time (solve-b input))
  ; 1538773034088
  ; "Elapsed time: 49589.860458 msecs"
  )

