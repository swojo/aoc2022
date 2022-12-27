(ns advent.day2
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(def sample-input
  "A Y
B X
C Z")

(defn parse-expected-outcome
  [in]
  (case in
    "X" :lose
    "Y" :tie
    "Z" :win))

(defn parse-move
  [in]
  (case in
    ("A" "X") :rock
    ("B" "Y") :paper
    ("C" "Z") :scissors))

(defn eval-round
  [{:keys [:opponent :self] :as round}]
  (let [outcome (if (= opponent self)
                  :tie
                  (case [opponent self]
                    [:scissors :rock] :win
                    [:paper :scissors] :win
                    [:rock :paper] :win
                    :lose))]
    (assoc round :outcome outcome)))

(defn move-points
  [{:keys [:self]}]
  (case self
    :rock 1
    :paper 2
    :scissors 3))

(defn outcome-points
  [{:keys [:outcome]}]
  (case outcome
    :lose 0
    :tie 3
    :win 6))

(defn calc-score
  [round]
  (assoc round :score
         (+ (move-points round)
            (outcome-points round))))

(defn eval-round
  [{:keys [:opponent :self] :as round}]
  (let [outcome (if (= opponent self)
                  :tie
                  (case [opponent self]
                    [:scissors :rock] :win
                    [:paper :scissors] :win
                    [:rock :paper] :win
                    :lose))]
    (assoc round :outcome outcome)))

(defn determine-move
  [{:keys [:opponent :outcome] :as round}]
  (let [self (if (= outcome :tie)
                  opponent
                  (case [opponent outcome]
                    [:scissors :lose] :paper
                    [:scissors :win] :rock
                    [:paper :lose] :rock
                    [:paper :win] :scissors
                    [:rock :lose] :scissors
                    [:rock :win] :paper
                    :lose))]
    (assoc round :self self)
  ))

(defn parse-line-part1
  [line]
  (let [[opponent self] (str/split line #"\s+")]
    {:opponent (parse-move opponent)
     :self (parse-move self)}))


(defn parse-input-part1
  [input]
  (->> (str/split-lines input)
       (map parse-line-part1)))

(defn parse-line-part2
  [line]
  (let [[opponent outcome] (str/split line #"\s+")]
    {:opponent (parse-move opponent)
     :outcome (parse-expected-outcome outcome)}))

(defn parse-input-part2
  [input]
  (->> (str/split-lines input)
       (map parse-line-part2)))

(defn solve-part1 [input] (->> (parse-input-part1 input)
     (map eval-round)
     (map calc-score)
     (map :score)
     (reduce + 0)))

(defn solve-part2 [input]
  (->> (parse-input-part2 input)
       (map determine-move)
       (map calc-score)
       (map :score)
       (reduce + 0)
       )
  )

(comment
   (parse-line-part1 (first (str/split-lines sample-input)))
   (determine-move (parse-line-part2 (last (str/split-lines sample-input))))
   (solve-part2 sample-input)
   (solve-part2 (slurp (io/resource "inputDay2.txt")))
   (parse-input-part2 sample-input)
  

   (parse-move "X")
   (eval-round {:opponent :rock,
                :self :rock} )
   (solve-part1 sample-input)
   (solve-part1 (slurp (io/resource "inputDay2.txt")))
 #__ )


