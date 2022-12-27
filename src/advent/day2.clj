(ns advent.day2
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

(def sample-input
  "A Y
B X
C Z")

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

(defn parse-line
  [line]
  (let [[opponent self] (str/split line #"\s+")]
    {:opponent (parse-move opponent)
     :self (parse-move self)}))


(defn parse-input
  [input]
  (->> (str/split-lines input)
       (map parse-line)))

(defn solve-part1 [input] (->> (parse-input input)
     (map eval-round)
     (map calc-score)
     (map :score)
     (reduce + 0)))

(comment
   (parse-line (first (str/split-lines sample-input)))

   (parse-move "X")
   (eval-round {:opponent :rock,
                :self :rock} )
   (solve-part1 sample-input)
   (solve-part1 (slurp (io/resource "inputDay2.txt")))
 #__ )


