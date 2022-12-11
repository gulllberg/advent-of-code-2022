(ns advent-of-code-2022.day-11
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1\n")
(def input (slurp "src/advent_of_code_2022/inputs/day11.txt"))

(defn get-operation
  [line]
  ; Could maybe be done nicer with something like
  ; ((eval (read-string "(fn [old] (* old old))")) 5)
  ; and parsing out the operation and numbers/input from line
  (cond
    (= line "  Operation: new = old * old")
    (fn [old]
      (* old old))

    (clojure.string/starts-with? line "  Operation: new = old *")
    (fn [old]
      (* old (read-string (re-find #"\d+" line))))

    (clojure.string/starts-with? line "  Operation: new = old +")
    (fn [old]
      (+ old (read-string (re-find #"\d+" line))))))

(defn parse-input
  [input]
  (->> (clojure.string/split input #"\n\n")
       (map clojure.string/split-lines)
       (reduce (fn [[s m-n] m]
                 (let [id (re-find #"\d+" (first m))
                       start-items (reverse (map read-string (re-seq #"\d+" (second m))))
                       operation (get-operation (nth m 2))
                       test-number (read-string (re-find #"\d+" (nth m 3)))
                       test-fn (fn [n] (= 0 (mod n test-number)))
                       true-throw-target (re-find #"\d+" (nth m 4))
                       false-throw-target (re-find #"\d+" (nth m 5))]
                   [(assoc s id {:id                 id
                                 :items              start-items
                                 :operation          operation
                                 :test-fn            test-fn
                                 :true-throw-target  true-throw-target
                                 :false-throw-target false-throw-target
                                 :inspected-counter  0})
                    (* m-n test-number)]))
               [{} 1])))

(defn one-round
  [state worry-level-divisor magic-number]
  (let [monkeys (sort (keys state))]
    (reduce (fn [state id]
              (reduce (fn [state worry-level]
                        (let [new-worry-level (-> ((get-in state [id :operation]) worry-level)
                                                   (/ worry-level-divisor)
                                                  (Math/floor)
                                                  (long)
                                                  (mod magic-number))
                              test-result ((get-in state [id :test-fn]) new-worry-level)
                              target (if test-result (get-in state [id :true-throw-target]) (get-in state [id :false-throw-target]))]
                          (-> state
                              (update-in [id :inspected-counter] inc)
                              (update-in [id :items] drop-last)
                              (update-in [target :items] conj new-worry-level))))
                      state
                      (reverse (get-in state [id :items]))))
            state
            monkeys)))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 10605))}
  [input]
  (let [[state magic-number] (parse-input input)
        state (reduce (fn [state _]
                        (one-round state 3 magic-number))
                      state
                      (range 20))]
    (->> state
         (vals)
         (map :inspected-counter)
         (sort)
         (reverse)
         (take 2)
         (apply *))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 2713310158))}
  [input]
  (let [[state magic-number] (parse-input input)
        state (reduce (fn [state _]
                        (one-round state 1 magic-number))
                      state
                      (range 10000))]
    (->> state
         (vals)
         (map :inspected-counter)
         (sort)
         (reverse)
         (take 2)
         (apply *))))

(comment
  (solve-a input)
  ; 110264

  (solve-b input)
  ; 23612457316
  )


