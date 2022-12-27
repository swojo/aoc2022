(ns advent.day2
  (:require
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
  [opp self]
  (if (= opp self)
    :tie
    (case [opp self]
      [:scissors :rock] :win
      [:paper :scissors] :win
      [:rock :paper] :win
      :lose)))


(defn calc-points
  [move outcome]
  (let [points-for-move
        (case move
          :rock 1
          :paper 2
          :scissors 3)
        points-for-outcome
        (case outcome
          :lose 0
          :tie 3
          :win 6)
        total-points
        (+ points-for-move points-for-outcome)]
    total-points))


(defn parse-line
  [line]
  (mapv parse-move (str/split line #"\s+")))


(comment
   (parse-move "X")
   (eval-round :rock :rock )
   (calc-points :rock :win)
   (let [rounds (str/split-lines sample-input)

         ])
   (let [line-moves (parse-line "A Y")
         outcome (apply eval-round line-moves)
         points-for-round (calc-points 
                           (last line-moves)
                           outcome)
         ]
     points-for-round)
    
 #__ )


