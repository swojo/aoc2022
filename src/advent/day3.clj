(ns advent.day3
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]
    [clojure.string :as str]))


(def sample-input
  "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")


(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map
         (fn [rucksack]
           (let [num-items (count rucksack)
                 compartments (split-at (/ num-items 2) rucksack)]
             {:compartment1 (first compartments)
              :compartment2 (second compartments)})))))


(defn compartments-as-sets
  [{:keys [:compartment1 :compartment2]}]
  {:compartment1 (set compartment1)
   :compartment2 (set compartment2)})

(defn rucksack-set
  [{:keys [:compartment1 :compartment2] :as rucksack}]
  (set/union compartment1 compartment2))


(defn common-item
  [coll]
  (let [intersection-set (apply set/intersection coll)]
    (first intersection-set)))

(defn compartment-intersection
  [rucksack]
  (let [intersection-set (set/intersection (set (:compartment1 rucksack)) (set (:compartment2 rucksack)))]
    (first intersection-set)))


(defn calc-priority
  [letter]
  (let [ascii_val (int letter)]
    (cond
      (and (>= ascii_val (int \a)) (<= ascii_val (int \z))) (+ 1 (- ascii_val (int \a)))
      (and (>= ascii_val (int \A)) (<= ascii_val (int \Z))) (+ 27 (- ascii_val (int \A))))))


(defn solve
  [input]
  (->> (parse-input input)
       (map compartments-as-sets)
       (map vals)
       (map common-item)
       (map calc-priority)
       (reduce + 0)))

(defn solve-part2
  [input]
  (->> (parse-input input)
       (map compartments-as-sets)
       (map rucksack-set)
       (partition 3)
       (map common-item)
       (map calc-priority)
       (reduce + 0)
       ))

(comment 
  (+ 1 (- (int \b) (int \a)))
  (+ 1 (int \A))

  (let [input (parse-input sample-input)
        intersections (map compartment-intersection input)
        priorities (map calc-priority intersections)
        ]
    (reduce + 0 priorities)
    )

  (let [input (parse-input sample-input)
        compartments-as-sets (map compartments-as-sets input)
        ]
     (vals (first compartments-as-sets)))
  (solve sample-input)
  (solve (slurp (io/resource "inputDay3.txt")))
  (solve-part2 sample-input)
  (solve-part2 (slurp (io/resource "inputDay3.txt")))
  #__)

