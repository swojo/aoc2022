(ns advent.day4
  (:require
    [clojure.string :as str]
    [clojure.java.io :as io]
    ))


(def sample-input
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn parse-input
  [input]
  (let [lines (str/split-lines input)
        pairs (map
                (fn [line]
                  (str/split line #","))
                lines)
        pair-vectors (map
                       (fn [pair]
                         (map
                           (fn [range]
                             (str/split range #"-"))
                           pair))
                       pairs)
        as-numbers (map
                     (fn [pair]
                       (map
                         (fn [assignment]
                           (map parse-long assignment))
                         pair))
                     pair-vectors)]
    as-numbers))


(defn is-one-assignment-contained-by-other
  [assignment1 assignment2]

  (cond
    (and  (<= (first assignment1) (first assignment2))
          (>= (last assignment1) (last assignment2)))
    true
    (and  (>= (first assignment1) (first assignment2))
          (<= (last assignment1) (last assignment2)))
    true
    :else false))

(defn solve
  [input]
  (let [pairs (parse-input input)
        contained (map (fn [pair] ( is-one-assignment-contained-by-other (first pair) (last pair))) pairs)
        only-true (filter true? contained)
        ]
    
    (count only-true)))

(comment 
  (solve sample-input)
  (solve (slurp (io/resource "inputDay4.txt")))

  
  #__)
