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
  [nodes [position open-valves] score time-remaining]
  (let [flow-rate (get-in nodes [position :flow-rate])]
    (reduce (fn [a c]
              (assoc a [c open-valves] score))
            (if (or (contains? open-valves position)
                    (= flow-rate 0))
              {}
              {[position (conj open-valves position)] (+ score (* (dec time-remaining) flow-rate))})
            (get-in nodes [position :connections]))))

(defn find-best-states
  [nodes states time-remaining]
  (loop [time-remaining time-remaining
         ; {[position open-valves] score}
         states states]
    (println time-remaining (count states))
    (if (= 0 time-remaining)
      states
      (recur (dec time-remaining) (reduce-kv (fn [a [position open-valves] score]
                                               (reduce-kv (fn [a k v]
                                                            (let [existing-score (get a k -1)]
                                                              (if (>= existing-score v)
                                                                a
                                                                (assoc a k v))))
                                                          a
                                                          (get-branching-states nodes [position open-valves] score time-remaining)))
                                             {}
                                             states)))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 1651))}
  [input]
  (let [nodes (parse-input input)]
    (->> (find-best-states nodes {["AA" #{}] 0} 30)
         (vals)
         (apply max))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 1707))}
  [input]
  (let [nodes (parse-input input)
        _ (println "first go")
        states-after-one-go (find-best-states nodes {["AA" #{}] 0} 26)
        _ (println "second go")]
    (->> (find-best-states nodes
                           (reduce-kv (fn [a k v]
                                        (let [new-k ["AA" (second k)]
                                              existing-score (get a new-k -1)]
                                          (if (>= existing-score v)
                                            a
                                            (assoc a new-k v))))
                                      {}
                                      states-after-one-go)
                           26)
         (vals)
         (apply max))))

(comment
  (time (solve-a input))
  ; 1940
  ; "Elapsed time: 1334.628125 msecs"

  (time (solve-b input))
  ; 2469
  ; "Elapsed time: 54968.148834 msecs"
  )
