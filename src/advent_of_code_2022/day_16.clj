(ns advent-of-code-2022.day-16
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II")
(def input (slurp "src/advent_of_code_2022/inputs/day16.txt"))

(defn parse-input
  [input]
  (->> input
       (clojure.string/split-lines)
       (reduce (fn [a line]
                 (let [[id & connections] (re-seq #"[A-Z]{2}" line)
                       flow-rate (read-string (re-find #"\d+" line))]
                   (assoc a id {:flow-rate flow-rate :connections (into #{} connections)})))
               {})))

(defn get-branching-states
  [nodes [position open-valves] [score best-ignored] time-remaining]
  (let [flow-rate (get-in nodes [position :flow-rate])]
    (reduce (fn [a c]
              (assoc a [c open-valves] [score (max best-ignored (if (contains? open-valves position) 0 flow-rate))]))
            (if (or (contains? open-valves position)
                    (<= flow-rate best-ignored))
              {}
              {[position (conj open-valves position)] [(+ score (* (dec time-remaining) flow-rate)) 0]})
            (get-in nodes [position :connections]))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 1651))}
  [input]
  (let [nodes (parse-input input)]
    (loop [time-remaining 30
           ; {[position open-valves] [score best-ignored]}
           states {["AA" #{}] [0 0]}]
      (println time-remaining (count states))
      (if (= 0 time-remaining)
        (apply max (map first (vals states)))
        (recur (dec time-remaining) (reduce-kv (fn [a [position open-valves] [score best-ignored]]
                                                 (reduce-kv (fn [a k v]
                                                              (let [[existing-score _] (get a k [-1 "whatever"])]
                                                                (if (>= existing-score (first v))
                                                                  a
                                                                  (assoc a k v))))
                                                            a
                                                            (get-branching-states nodes [position open-valves] [score best-ignored] time-remaining)))
                                               {}
                                               states))))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 1707))}
  [input]
  1)

(comment
  (time (solve-a input))
  ; 1940
  ; "Elapsed time: 1334.628125 msecs"

  (time (solve-b input))
  ;
  )
