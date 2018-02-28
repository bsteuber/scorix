(ns scorix.describe
  (:require [goog.string :as gstring]
            [goog.string.format]))

(defn format-short-non-trump-opponent-suit [length opponent]
  (let [length-str (case length
                     0 "Void"
                     1 "Singleton"
                     2 "Doubleton")
        opponent-str (if (= length 2)
                       (str " " (name opponent))
                       "")]
    (gstring/format "%s in%s opponent suit in suit contract"
                    length-str
                    opponent-str)))

(def text
  {:total-points "Total points"
   :high-card-points "High card points"
   :A-T-count "A and T count is %d"
   :blank "Blank %s"
   :length "Long %s suit"
   :AQJ "AQJ occurances"
   :partner-suit-honors "Honors in partner suit"
   :short-in-partner-suit "Short %s in partner suit"
   :gap-in-right-suit "%s in right opponent suit"
   :gap-in-left-suit "%s in left opponent suit"
   :length-in-opponent-suit "Long %s in opponent suit"
   :blank-honors-in-left-opponent-suit "Blank %s in left opponent suit"
   :T-9-count-in-NT "T and 9 count in NT is %d"
   :QJxx-in-NT "%s in NT"
   :covered-T-in-right-suit-in-NT "Covered T in right opponent suit in NT"
   :QJT9-or-QJT9-in-opponent-suit-in-NT "%s in opponent suit in NT"
   :JTxx-or-J9xx-in-right-opponent-suit-in-NT "%s in right opponent suit in NT"
   :trump-suit-length "Trump suit length is %d"
   :more-or-less-trumps-than-promised "Promised %s trumps, got %s"
   :high-trump-cards "High trump cards"
   :honors-in-non-trump-suits "Honors in non-trump suits"
   :short-non-trump-suits "Short non-trump suits"
   :short-non-trump-opponent-suit format-short-non-trump-opponent-suit
   :trump-basic-correction "Basic correction for suit contracts"})

(def extra-info
  {:total-points ["Sum of points required for bids:"
                  "Solid opening -> 13"
                  "4 in suit -> 25"
                  "3NT -> 25"
                  "5 in suit -> 29"
                  "6 in suit -> 33"
                  "6NT ->34"
                  "(0.5 is rounded to 0 and 0.75 is rounded to 1)"]
   :high-card-points ["High cards are counted as follows:"
                      "A -> 4"
                      "K -> 3"
                      "Q -> 2"
                      "J -> 1"]
   :A-T-count ["The number of A and T are counted and compared to the average of 2:"
               "More than 2 ->  0.25 per extra card"
               "Less than 2 -> -0.25 per missing card"]
   :blank ["Blank honors or honor combinations weaken your hand:"
           "A, K, Q -> -1"
           "J -> -0.75"
           "T -> -0.25"
           "QJ -> -1"
           "AK, AKQ, AJ, KQ, KQJ, KJ, QT, JT -> -0.5"
           "AKJ, AQJ, AQ, AT, KT, Qx, Jx, Tx -> -0.25"]
   :length ["Long suits give extra points depending on their strength:"
            "Weak 5 card suit -> 0.5"
            "At least reasonable 4 card suit -> 0.5"
            "Weak 6 card suit -> 1"
            "Reasonable 5 card suit -> 1"
            "Weak or reasonable 7+ card suit -> 2"
            "Reasonable 6 card suit -> 2"
            "Strong 5 card suit -> 1.5"
            "Strong 6+ card suit -> 3"]
   :AQJ ["Any occurance of AQJ combination -> 0.25"]
   :partner-suit-honors ["1  honor  in partner suit -> 0.5"
                         "2+ honors in partner suit -> 1"]
   :short-in-partner-suit ["Singleton in partner suit -> -1"
                           "Void in partner suit      -> -2"]
   :gap-in-right-suit ["KQx, KJx           ->  1"
                       "Kx, Qxx, AJx, AKT, AQ ->  0.5"]
   :gap-in-left-suit ["KQx, KJx           -> -1"
                      "Kx, Qxx, AJx, AKT, AQ -> -0.5"]
   :length-in-opponent-color ["Previous length points are reduced by up to 1."
                              "Gap-free lengths are still counted."]
   :blank-honors-in-left-opponent-suit ["Blank KJ -> -1"
                                        "Blank AJ -> -0.25"]
   :T-9-count-in-NT ["In NT, the number of T and 9 are counted and compared to the average of 2."
                     "More than 2 ->  0.25 per extra card"
                     "Less than 2 -> -0.25 per missing card"]
   :QJxx-in-NT ["In NT a previously non-biddable QJxx becomes a bit (0.25) stronger."]
   :covered-T-in-right-suit-in-NT ["Assumed the right opponent has all missing honors, the T can make a trick."]
   :QJT9-or-QJT9-in-opponent-suit-in-NT ["In NT when the opponents are developing the suit, the 9 or 8 in QJT9 or QJT8 might make a trick."]
   :JTxx-or-J9xx-in-right-opponent-suit-in-NT ["In NT when the right opponent has most of the honors, the T or 9 in JTxx or J9xx can duck and possibly make a trick."]
   :trump-suit-length ["Each trump card starting from the 6th -> 1"]
   :more-or-less-trumps-than-promised ["Each extra trump card -> 1"
                                       "Each missing trump card -> -1"]
   :high-trump-cards ["A -> 1.5"
                      "K, Q, J -> 1"
                      "T -> 0.75"
                      "9 -> 0.25"
                      "The lowest card of the suit is excluded. Maximum 2.5 points."]
   :honors-in-non-trump-suits ["A    ->  0.5"
                               "Q, J -> -0.5"]
   :short-non-trump-suits ["Doubleton -> 0.5"
                           "Singleton -> 1.5"
                           "Void -> 3"]
   :short-non-trump-opponent-suit ["Short in an opponent suit -> 0.5"
                                   "Exception: Doubleton in suit bid by right opponent, as left is likely to be short, too."
                                   "If it is clear from the bidding left has at least three cards, you can manually count the 0.5 (not covered in this software)."]
   :trump-basic-correction ["This substraction is necessary for all suit contracts. It compensates for the many trump honor points, trump suit lengh points, and short other suit points added below"]})

(defn format [[_ key & args]]
  (let [fn-or-format-string (get text key)]
    (if (fn? fn-or-format-string)
      (apply fn-or-format-string args)
      (apply gstring/format fn-or-format-string args))))
