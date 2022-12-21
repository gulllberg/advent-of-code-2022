(ns advent-of-code-2022.day-21
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt\ndvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4\npppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32")
(def input (slurp "src/advent_of_code_2022/inputs/day21.txt"))

(defn parse-input
  [input]
  (->> input
       (clojure.string/split-lines)
       (reduce (fn [a line]
                 (let [[id & deps] (re-seq #"[a-z]+" line)
                       number (when-let [n (re-find #"\d+" line)]
                                (read-string n))
                       operation (re-find #"[+*/\-]" line)]
                   (assoc a id {:number    number
                                :deps      deps
                                :operation operation})))
               {})))

(defn get-number
  [monkeys id]
  (if-let [number (get-in monkeys [id :number])]
    number
    (let [deps (get-in monkeys [id :deps])]
      (when (and (get-in monkeys [(first deps) :number])
                 (get-in monkeys [(second deps) :number]))
        (eval (read-string (str "(" (get-in monkeys [id :operation]) " " (get-in monkeys [(first deps) :number]) " " (get-in monkeys [(second deps) :number]) ")")))))))

(defn find-monkey-numbers
  [monkeys]
  (loop [monkeys monkeys]
    (let [new-monkeys (reduce (fn [a id]
                                (assoc-in a [id :number] (get-number a id)))
                              monkeys
                              (keys monkeys))]
      (if (= monkeys new-monkeys)
        new-monkeys
        (recur new-monkeys)))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 152))}
  [input]
  (-> (parse-input input)
      (find-monkey-numbers)
      (get-number "root")))

(defn get-human-number-operations
  [monkeys dep-id result]
  (let [next-id (reduce-kv (fn [_ id v]
                             (when (contains? (into #{} (:deps v)) dep-id)
                               (reduced id)))
                           nil
                           monkeys)]
    (if (= next-id "root")
      result
      (recur monkeys
             next-id
             (conj result [(get-in monkeys [next-id :operation])
                           (get-number monkeys (first (disj (into #{} (get-in monkeys [next-id :deps])) dep-id)))
                           (= (first (get-in monkeys [next-id :deps])) dep-id)])))))

(defn find-human-number
  [root-number human-number-operations]
  (reduce (fn [result [operation number left-hand]]
            (condp = operation
              "*" (/ result number)
              "/" (if left-hand (* result number) (/ number result))
              "+" (- result number)
              "-" (if left-hand (+ result number) (- number result))))
          root-number
          (reverse human-number-operations)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 301))}
  [input]
  (let [monkeys (find-monkey-numbers (assoc-in (parse-input input) ["humn" :number] nil))
        root-deps (get-in monkeys ["root" :deps])
        root-number (or (get-in monkeys [(first root-deps) :number]) (get-in monkeys [(second root-deps) :number]))]
    (find-human-number root-number (get-human-number-operations monkeys "humn" []))))

(comment
  (solve-a input)
  ; 24947355373338

  (solve-b input)
  ; 3876907167495
  )
