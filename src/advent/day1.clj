(ns advent.day1
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))


(def sample-input
  "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")


(defn parse-input
  [input]
  (let [split-by-elf (str/split input #"\n\n")
        split-by-intake (mapv
                          (fn [elf-string]
                            (str/split-lines elf-string))
                          split-by-elf)
        calories-as-int (map
                          (fn [elf-vector]
                            (map
                              (fn [calorie-str]
                                (parse-long calorie-str))
                              elf-vector))
                          split-by-intake)]
    calories-as-int))


(defn max-of-coll
  [coll]
  (apply max coll))


(defn sum
  [coll]
  (reduce (fn [acc item]
            (+ acc item))
          0 coll))

(defn solve-part1
  [input]
  (let [calorie-vec-per-elf (parse-input input)
        total-cal-per-elf (map sum calorie-vec-per-elf)
        abs-max-cal (max-of-coll total-cal-per-elf)]
    abs-max-cal))

(defn solve-part2
  [input]
  (let [calorie-vec-per-elf (parse-input input)
        total-cal-per-elf (map sum calorie-vec-per-elf)
        top-three (take 3 (sort > total-cal-per-elf))
        part2-sol (sum top-three)
        ]
   part2-sol))

(comment
  (solve-part2 sample-input)
  (tap>  (slurp (io/resource "inputDay1.txt")))
  (solve-part2 (slurp (io/resource "inputDay1.txt")))
    #__)
