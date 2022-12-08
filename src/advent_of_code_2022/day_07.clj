(ns advent-of-code-2022.day-07
  (:require [ysera.test :refer [is= is is-not]]))

(def test-input "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k\n")
(def input (slurp "src/advent_of_code_2022/inputs/day07.txt"))

(defn get-name
  [location name]
  (if (= location "/")
    (str "/" name)
    (str location "/" name)))

(defn parse-input
  [input]
  (first (reduce (fn [[state location] line]
                   (cond
                     (= line "$ ls")
                     [state location]

                     (= line "$ cd /")
                     [state "/"]

                     (= line "$ cd ..")
                     [state (get-in state [location :parent])]

                     (clojure.string/starts-with? line "$ cd ")
                     [state (get-name location (subs line 5))]

                     (clojure.string/starts-with? line "dir ")
                     (let [dir-name (get-name location (subs line 4))]
                       [(-> state
                            (assoc dir-name {:name     dir-name
                                             :type     :dir
                                             :size     0
                                             :children []
                                             :parent   location})
                            (update-in [location :children] conj dir-name))
                        location])

                     ;; File in ls
                     :else
                     (let [parts (clojure.string/split line #" ")
                           file-size (read-string (first parts))
                           file-name (get-name location (second parts))]
                       [(-> state
                            (assoc file-name {:name     file-name
                                              :type     :file
                                              :size     file-size
                                              :children []
                                              :parent   location})
                            (update-in [location :children] conj file-name))
                        location])))
                 [{"/" {:name     "/"
                        :type     :dir
                        :size     0
                        :children []
                        :parent   nil}}
                  "/"]
                 (clojure.string/split-lines input))))

(defn get-size
  {:test (fn []
           (is= (get-size (parse-input test-input) "/a/e") 584)
           (is= (get-size (parse-input test-input) "/a") 94853)
           (is= (get-size (parse-input test-input) "/d") 24933642)
           (is= (get-size (parse-input test-input) "/") 48381165))}
  [state name]
  (+ (get-in state [name :size])
     (reduce (fn [a v]
               (+ a (get-size state v)))
             0
             (get-in state [name :children]))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 95437))}
  [input]
  (let [state (parse-input input)]
    (reduce (fn [a k]
              (let [size (get-size state k)]
                (if (and (= :dir (get-in state [k :type]))
                         (<= size 100000))
                  (+ a size)
                  a)))
            0
            (keys state))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 24933642))}
  [input]
  (let [state (parse-input input)
        disk-size 70000000
        current-used (get-size state "/")
        current-free-disk (- disk-size current-used)
        required-disk-size 30000000
        amount-to-free (- required-disk-size current-free-disk)]
    (->> (keys state)
         (keep (fn [k]
                 (let [size (get-size state k)]
                   (when (and (= :dir (get-in state [k :type]))
                              (>= size amount-to-free))
                     size))))
         (sort)
         (first))))

(comment
  (solve-a input)
  ; 1453349

  (solve-b input)
  ; 2948823
  )

