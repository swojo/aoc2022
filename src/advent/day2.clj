(ns advent.day2
  (:require
    [clojure.core :as c]
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
  (if (:outcome round)
    round
    (let [outcome (if (= opponent self)
                    :tie
                    (case [opponent self]
                      [:scissors :rock] :win
                      [:paper :scissors] :win
                      [:rock :paper] :win
                      :lose))]
      (assoc round :outcome outcome))))


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
  (if (get round :self)
    round
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
    (assoc round :self self))))


(defn parse-line-part1
  [line]
  (let [[opponent self] (str/split line #"\s+")]
    {:opponent (parse-move opponent)
     :self (parse-move self)}))


(defn parse-line-part2
  [line]
  (let [[opponent outcome] (str/split line #"\s+")]
    {:opponent (parse-move opponent)
     :outcome (parse-expected-outcome outcome)}))


(defn parse-input
  [input parse-line-fn]
  (->> (str/split-lines input)
       (map parse-line-fn)))


(defn solve-part
  [input parse-line-fn]
  (->> (parse-input input parse-line-fn)
       (map determine-move)
       (map eval-round)
       (map calc-score)
       (map :score)
       (reduce + 0)))

(comment
   (parse-line-part1 (first (str/split-lines sample-input)))
   (determine-move (parse-line-part2 (last (str/split-lines sample-input))))
   (parse-input sample-input parse-line-part1) 
   (parse-input sample-input parse-line-part2) 

   (parse-move "X")
   (eval-round {:opponent :rock,
                :self :rock} )
   (solve-part sample-input parse-line-part1)
   (solve-part sample-input parse-line-part2)
   (solve-part (slurp (io/resource "inputDay2.txt")) parse-line-part1 )
   (solve-part (slurp (io/resource "inputDay2.txt")) parse-line-part2)
 #__ )


