(ns advent.day4
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))


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
  [pair]

  (let [assignment1 (first pair)
        assignment2 (last pair)
        full-overlap
        (cond
          (and  (<= (first assignment1) (first assignment2))
                (>= (last assignment1) (last assignment2)))
          true
          (and  (>= (first assignment1) (first assignment2))
                (<= (last assignment1) (last assignment2)))
          true
          :else false)]
    full-overlap))


(defn overlap-exists
  [pair]
  (let [assignment1 (first pair)
        assignment2 (last pair)
        partial-overlap
        (cond
          (and  (<= (first assignment1) (first assignment2))
                (>= (last assignment1) (first assignment2)))
          true
          (and (>= (first assignment1) (first assignment2))
               (<= (first assignment1) (last assignment2)))
          true
          :else false)]
    partial-overlap))


(defn solve-part1
  [input]
  (let [pairs (parse-input input)
        contained (map is-one-assignment-contained-by-other pairs)
        only-true (filter true? contained)]

    (count only-true)))

(defn solve-part2
  [input]
  (let [pairs (parse-input input)
        overlap (map overlap-exists pairs)
        only-true (filter true? overlap)]

    (count only-true)))


(comment 
  (solve-part1 sample-input)
  (solve-part1 (slurp (io/resource "inputDay4.txt")))

  (solve-part2 sample-input)
  (solve-part2 (slurp (io/resource "inputDay4.txt")))

  #__)
