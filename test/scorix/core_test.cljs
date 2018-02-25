(ns scorix.core-test
  (:require [cljs.test :refer [deftest is]]
            [scorix.core :as scorix]))

(deftest basic
  (is (= (scorix/basic-points
          ["xxxxx" "xxxx" "xxx" "x"])
         [["High card points" 0]
          ["A and T count is 0" -0.5]]))
  (is (= (scorix/basic-points
          ["AT9xx" "xxxx" "xxx" "x"])
         [["High card points" 4]]))
  (is (= (scorix/basic-points
          ["A" "xxxx" "xxxx" "xxxx"])
         [["High card points" 4]
          ["A and T count is 1" -0.25]
          ["Blank A/K/Q" -1]]))
  (is (= (scorix/basic-points
          ["QJ" "xxxx" "xxxx" "xxx"])
         [["High card points" 3]
          ["A and T count is 0" -0.5]
          ["Blank QJ" -1]]))
  (is (= (scorix/basic-points
          ["T9" "xxxx" "xxxx" "xxx"])
         [["High card points" 0]
          ["A and T count is 1" -0.25]
          ["Blank AKJ/AQJ/AQ/AT/KT/Qx/Jx/Tx" -0.25]]))
  (is (= (scorix/basic-points
          ["AKxxx" "Qxxx" "AJ" "JT"])
         [["High card points" 15]
          ["A and T count is 3" 0.25]
          ["Blank AK/AKQ/AJ/KQ/KQJ/KJ/QT/JT" -0.5]
          ["Blank AK/AKQ/AJ/KQ/KQJ/KJ/QT/JT" -0.5]])))

(deftest okay-suit
  (is (not (scorix/okay-suit? "xxxx")))
  (is (not (scorix/okay-suit? "KTxxx")))
  (is (scorix/okay-suit? "Axxx"))
  (is (scorix/okay-suit? "KJxx"))
  (is (scorix/okay-suit? "QJTx")))

(deftest strong-suit
  (is (not (scorix/strong-suit? "xxxx")))
  (is (not (scorix/strong-suit? "KTxxx")))
  (is (scorix/strong-suit? "AKJxx"))
  (is (scorix/strong-suit? "AKQTxx"))
  (is (scorix/strong-suit? "KQJTxx")))

(deftest ground-length
  (is (= (scorix/ground-length-points "xxxxx")
         [["Weak five card suit" 0.5]]))
  (is (= (scorix/ground-length-points "KTxxx")
         [["Weak five card suit" 0.5]]))
  (is (= (scorix/ground-length-points "Axxx")
         [["Okay or strong four card suit" 0.5]]))
  (is (= (scorix/ground-length-points "AKQJ")
         [["Okay or strong four card suit" 0.5]]))
  (is (= (scorix/ground-length-points "Kxxxxx")
         [["Weak six card suit" 1]]))
  (is (= (scorix/ground-length-points "xxxxxx")
         [["Weak six card suit" 1]]))
  (is (= (scorix/ground-length-points "Axxxx")
         [["Okay five card suit" 1]]))
  (is (= (scorix/ground-length-points "KJxxx")
         [["Okay five card suit" 1]]))
  (is (= (scorix/ground-length-points "xxxxxxx")
         [["Weak or okay seven card suit or longer" 2]]))
  (is (= (scorix/ground-length-points "Axxxxxx")
         [["Weak or okay seven card suit or longer" 2]]))
  (is (= (scorix/ground-length-points "Axxxxx")
         [["Okay six card suit" 2]]))
  (is (= (scorix/ground-length-points "QJTxxx")
         [["Okay six card suit" 2]]))
  (is (= (scorix/ground-length-points "AKJxx")
         [["Strong five card suit" 1.5]]))
  (is (= (scorix/ground-length-points "KQJTx")
         [["Strong five card suit" 1.5]]))
  (is (= (scorix/ground-length-points "KQJTxx")
         [["Strong six card suit or longer" 3]]))
  (is (= (scorix/ground-length-points "AKQJTxx")
         [["Strong six card suit or longer" 3]]))
  (is (empty? (scorix/ground-length-points "xxxx"))))

(deftest ground-honors
  (is (= (scorix/ground-honor-points "AKJTx")
         [["Suit with AKJ" 0.25]]))
  (is (= (scorix/ground-honor-points "AKJ")
         [["Suit with AKJ" 0.25]]))
  (is (empty? (scorix/ground-honor-points "AKQ"))))

(deftest ground-partner-suit
  (is (empty? (scorix/ground-partner-suit-points "xx")))
  (is (= (scorix/ground-partner-suit-points "")
         [["Chikane in partner color" -2]]))
  (is (= (scorix/ground-partner-suit-points "x")
         [["Single in partner color" -1]]))
  (is (= (scorix/ground-partner-suit-points "J")
         [["Honor in partner color" 0.5]
          ["Single in partner color" -1]]))
  (is (= (scorix/ground-partner-suit-points "Ax")
         [["Honor in partner color" 0.5]]))
  (is (= (scorix/ground-partner-suit-points "AKJ")
         [["Two or more honors in partner color" 1]])))

(deftest ground-right-opponent-suit
  (is (empty? (scorix/ground-right-opponent-suit-points "")))
  (is (= (scorix/ground-right-opponent-suit-points "Kxx")
         [["Kx/Qxx/AJx/AKT/AQ in right opponent color" 0.5]]))
  (is (= (scorix/ground-right-opponent-suit-points "KQ9")
         [["KQx/KJx in right opponent color" 1]]))
  (is (= (scorix/ground-right-opponent-suit-points "KQxx")
         [["KQx/KJx in right opponent color" 1]
          #_["Non-solid length in opponent color" -0.5]]))
  (is (= (scorix/ground-right-opponent-suit-points "Axxx")
         [#_["Non-solid length in opponent color" -0.5]]))
  (is (= (scorix/ground-right-opponent-suit-points "xxxxxx")
         [["Non-solid length in opponent color" -1]])))

(deftest ground-left-opponent-suit
  (is (empty? (scorix/ground-left-opponent-suit-points "")))
  (is (= (scorix/ground-left-opponent-suit-points "Kxx")
         [["Kx/Qxx/AJx/AKT/AQ in left opponent color" -0.5]]))
  (is (= (scorix/ground-left-opponent-suit-points "KQx")
         [["KQx/KJx in left opponent color" -1]]))
  (is (= (scorix/ground-left-opponent-suit-points "KQxx")
         [["KQx/KJx in left opponent color" -1]
          #_["Non-solid length in opponent color" -0.5]]))
  (is (= (scorix/ground-left-opponent-suit-points "Axxx")
         [#_["Non-solid length in opponent color" -0.5]]))
  (is (= (scorix/ground-left-opponent-suit-points "AKQxx")
         [#_["Non-solid length in opponent color" -1]])))

(deftest ground
  (is (= (scorix/ground-points
          ["Axx" "Kxx" "xxxx" "xxx"]
          [ nil   nil   nil    nil])
         [["High card points" 7]
          ["A and T count is 1" -0.25]]))
  (is (= (scorix/ground-points
          ["Axxx" "Kxx" "xxxx" "xx"]
          [ nil    nil   nil    nil])
         [["High card points" 7]
          ["A and T count is 1" -0.25]
          ["Okay or strong four card suit" 0.5]]))
  (is (= (scorix/ground-points
          ["Axxx" "Kxx" "xxxx" "xx"]
          [ nil   :left  nil    nil])
         [["High card points" 7]
          ["A and T count is 1" -0.25]
          ["Okay or strong four card suit" 0.5]
          ["Kx/Qxx/AJx/AKT/AQ in left opponent color" -0.5]]))
  (is (= (scorix/ground-points
          ["Axxx" "Kxx"    "xxxx" "xx"]
          [ nil   :partner  nil    nil])
         [["High card points" 7]
          ["A and T count is 1" -0.25]
          ["Okay or strong four card suit" 0.5]
          ["Honor in partner color" 0.5]]))
  (is (= (scorix/ground-points
          ["Axxx" "Kxx"  "xxxx" "xx"]
          [ nil   :right  nil    nil])
         [["High card points" 7]
          ["A and T count is 1" -0.25]
          ["Okay or strong four card suit" 0.5]
          ["Kx/Qxx/AJx/AKT/AQ in right opponent color" 0.5]]))
  (is (= (scorix/ground-points
          ["AK"       "x" "KQTxx" "AJ9xx"]
          [:partner   nil  nil     nil])
         [["High card points" 17]
          ["A and T count is 3" 0.25]
          ["Blank AK/AKQ/AJ/KQ/KQJ/KJ/QT/JT" -0.5]
          ["Two or more honors in partner color" 1]
          ["Okay five card suit" 1]
          ["Okay five card suit" 1]])))

(deftest no-trump
  (is (= (scorix/no-trump-points
          ["ATxx" "QJT8x" "9xx" "xx"]
          [ nil    nil     nil   nil])
         [["High card points" 7]
          ["A and T count is 3" 0.25]
          ["Okay or strong four card suit" 0.5]
          ["Okay five card suit" 1]
          ["T and 9 count in NT is 3" 0.25]
          ["QJxx in NT" 0.25]]))
  (is (= (scorix/no-trump-points
          ["ATx"  "QJT9x" "JTxx"  "xx"]
          [:right :left   :right  :partner])
         [["High card points" 8]
          ["A and T count is 4" 0.5]
          ["Okay five card suit" 1]
          ["Kx/Qxx/AJx/AKT/AQ in left opponent color" -0.5]
          ["T and 9 count in NT is 4" 0.5]
          ["QJxx in NT" 0.25]
          ["QJT9 or QJT8 in opponent color in NT" 0.5]
          ["JTxx or J9xx in right opponent color in NT" 0.25]]))
  (is (= (scorix/no-trump-points
          ["K"      "QT"     "QJ98xx"  "AQ8x"]
          [:partner :partner  nil       nil])
         [["High card points" 14]
          ["Blank A/K/Q" -1]
          ["Blank AK/AKQ/AJ/KQ/KQJ/KJ/QT/JT" -0.5]
          ["Honor in partner color" 0.5]
          ["Single in partner color" -1]
          ["Honor in partner color" 0.5]
          ["Weak six card suit" 1]
          ["Okay or strong four card suit" 0.5]
          ["QJxx in NT" 0.25]]))
  (is (= (scorix/no-trump-points
          ["AJxx"     "AKJx"  "Kx"     "xxx"]
          [ nil        nil    :partner  nil])
         [["High card points" 16]
          ["Okay or strong four card suit" 0.5]
          ["Okay or strong four card suit" 0.5]
          ["Suit with AKJ" 0.25]
          ["Honor in partner color" 0.5]
          ["T and 9 count in NT is 0" -0.5]])))

(deftest trump-points
  (is (= (scorix/trump-points
          ["ATxxxx" "xxx" "xx"   "xx"]
          [:trump    nil  :left :right]
          4)
         [["High card points" 4]
          ["Basic trump points correction" -1.5]
          ["Trump suit length is 6" 1]
          ["Promised 4 trumps, got 6" 2]
          ["Trump honors (A->1.5, K/Q/J-> 1, T->0.75, 9->0.25, lowest card excluded, max 2.5)" 2.25]
          ["Short other suits (Double->0.5, Single->1.5, Chikane->3)" 1]
          ["Short opponent suits (except double in right opponent suit)" 0.5]]))
  (is (= (scorix/trump-points
          ["AKQxxx" "KTx" "xxxx"  ""]
          [:trump    nil   nil   :left]
          4)
         [["High card points" 12]
          ["Basic trump points correction" -1.5]
          ["Trump suit length is 6" 1]
          ["Promised 4 trumps, got 6" 2]
          ["Trump honors (A->1.5, K/Q/J-> 1, T->0.75, 9->0.25, lowest card excluded, max 2.5)" 2.5]
          ["Other suit honors (A->0.5, Q/J->-0.5)" 0]
          ["Short other suits (Double->0.5, Single->1.5, Chikane->3)" 3]
          ["Short opponent suits (except double in right opponent suit)" 0.5]])))

(deftest random-hand
  (let [hand (scorix/random-hand)]
    (is (vector? hand))
    (is (= (count hand)
           4))
    (is (= (count (apply str hand))
           13))))
