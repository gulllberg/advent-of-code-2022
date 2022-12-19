(ns advent-of-code-2022.day-19
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\nBlueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.")
(def input (slurp "src/advent_of_code_2022/inputs/day19.txt"))

(defn parse-input
  [input]
  (map (fn [line]
         (map read-string (re-seq #"\d+" line)))
       (clojure.string/split-lines input)))

;; Note: Factory can only produce one robot at a time, so no need to check multiple moves at once on a state
;; This also means you should not produce more robots of one kind than maximum required resource it produces to build any robot
(defn get-branching-states
  [blueprint state]
  (let [[_ ore-robot-ore-cost clay-robot-ore-cost obsidian-robot-ore-cost obsidian-robot-clay-cost geode-robot-ore-cost geode-robot-obsidian-cost] blueprint
        [ore clay obsidian geode ore-robots clay-robots obsidian-robots geode-robots forbidden-moves] state
        forbidden-moves (reduce conj forbidden-moves (remove nil? [(when (>= ore-robots (max ore-robot-ore-cost clay-robot-ore-cost obsidian-robot-ore-cost geode-robot-ore-cost)) :ore-robot)
                                                                   (when (>= clay-robots obsidian-robot-clay-cost) :clay-robot)
                                                                   (when (>= obsidian-robots geode-robot-obsidian-cost) :obsidian-robot)]))
        can-build-ore-robot (and (>= ore ore-robot-ore-cost) (not (contains? forbidden-moves :ore-robot)) )
        can-build-clay-robot (and (>= ore clay-robot-ore-cost) (not (contains? forbidden-moves :clay-robot)) )
        can-build-obsidian-robot (and (>= ore obsidian-robot-ore-cost) (>= clay obsidian-robot-clay-cost) (not (contains? forbidden-moves :obsidian-robot)) )
        can-build-geode-robot (and (>= ore geode-robot-ore-cost) (>= obsidian geode-robot-obsidian-cost) (not (contains? forbidden-moves :geode-robot)))]
    (if (= (count forbidden-moves) 4)
      []
      (remove nil? [(when can-build-ore-robot [(- ore ore-robot-ore-cost) clay obsidian geode (inc ore-robots) clay-robots obsidian-robots geode-robots #{}])
                    (when can-build-clay-robot [(- ore clay-robot-ore-cost) clay obsidian geode ore-robots (inc clay-robots) obsidian-robots geode-robots #{}])
                    (when can-build-obsidian-robot [(- ore obsidian-robot-ore-cost) (- clay obsidian-robot-clay-cost) obsidian geode ore-robots clay-robots (inc obsidian-robots) geode-robots #{}])
                    (when can-build-geode-robot [(- ore geode-robot-ore-cost) clay (- obsidian geode-robot-obsidian-cost) geode ore-robots clay-robots obsidian-robots (inc geode-robots) #{}])
                    ; If you could build something but did not you might not build it until you've built something else
                    (let [new-forbidden-moves (reduce conj forbidden-moves (remove nil? [(when can-build-ore-robot :ore-robot) (when can-build-clay-robot :clay-robot) (when can-build-obsidian-robot :obsidian-robot) (when can-build-geode-robot :geode-robot)]))]
                      (when-not (= (count new-forbidden-moves) 4)
                        [ore clay obsidian geode ore-robots clay-robots obsidian-robots geode-robots new-forbidden-moves]))]))))

(defn solve-one-blueprint
  [blueprint max-time]
  (println "starting blueprint" blueprint)
  (loop [time-passed 0
         ; ore - clay - obsidian - geode - ore robots - clay robots - obsidian robots - geode robots - forbidden moves
         states #{[0 0 0 0 1 0 0 0 #{}]}]
    (println time-passed (count states))
    (if (= time-passed max-time)
      (apply max (map (fn [s]
                        (nth s 3))
                      states))
      (let [new-states (reduce (fn [a s]
                                 (let [branching-states (get-branching-states blueprint s)
                                       [_ _ _ _ ore-robots clay-robots obsidian-robots geode-robots _] s]
                                   (reduce conj a (map (fn [m]
                                                         (-> m
                                                             (update 0 + ore-robots)
                                                             (update 1 + clay-robots)
                                                             (update 2 + obsidian-robots)
                                                             (update 3 + geode-robots)))
                                                       branching-states))))
                               #{}
                               states)]
        (recur (inc time-passed) new-states)))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 33))}
  [input]
  (let [blueprints (parse-input input)]
    (reduce (fn [a blueprint]
              (+ a (* (first blueprint) (solve-one-blueprint blueprint 24))))
            0
            blueprints)))

(defn solve-b
  ;{:test (fn []
  ;         (is= (solve-one-blueprint [1 4 2 3 14 2 7] 32) 56)
  ;         (is= (solve-one-blueprint [2 2 3 3 8 3 12] 32) 62))}
  [input]
  (let [blueprints (parse-input input)]
    (reduce (fn [a blueprint]
              (* a (solve-one-blueprint blueprint 32)))
            1
            (take 3 blueprints))))

(comment
  (time (solve-a input))
  ; 1365
  ; "Elapsed time: 3699.290875 msecs"

  (time (solve-b input))
  ; 4864
  ; "Elapsed time: 88437.999792 msecs"
  )

