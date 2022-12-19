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
  [nodes state time-remaining]
  (let [position (:position state)
        flow-rate (get-in nodes [position :flow-rate])]
    (reduce (fn [a c]
              (conj a (-> state
                          (assoc :position c)
                          (update :best-ignored max (if (contains? (:open-valves state) position) 0 flow-rate)))))
            (if (or (contains? (:open-valves state) position)
                    (<= flow-rate (:best-ignored state)))
              #{}
              #{(-> state
                    (update :open-valves conj position)
                    (assoc :best-ignored 0)
                    (update :score + (* (dec time-remaining) flow-rate)))})
            (get-in nodes [position :connections]))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 1651))}
  [input]
  (let [nodes (parse-input input)]
    (loop [time-remaining 30
           states #{{:position "AA" :open-valves #{} :best-ignored 0 :score 0}}]
      (println time-remaining (count states))
      (if (= 0 time-remaining)
        (apply max (map :score states))
        (recur (dec time-remaining) (reduce clojure.set/union (map (fn [state]
                                                                     (get-branching-states nodes state time-remaining))
                                                                   states)))))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 1))}
  [input]
  1)

(comment
  (time (solve-a input))
  ; 1940
  ; "Elapsed time: 236141.871125 msecs"
  ;30 1
  ;29 5
  ;28 6
  ;27 23
  ;26 47
  ;25 93
  ;24 173
  ;23 320
  ;22 591
  ;21 1033
  ;20 1870
  ;19 3260
  ;18 5697
  ;17 9970
  ;16 17125
  ;15 29472
  ;14 49878
  ;13 84399
  ;12 141002
  ;11 234282
  ;10 386391
  ;9 631463
  ;8 1025523
  ;7 1648143
  ;6 2625107
  ;5 4136770
  ;4 6444099
  ;3 9913705
  ;2 15041482
  ;1 22478796
  ;0 33048506

  (time (solve-b input))
  ;
  )
