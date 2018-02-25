(ns scorix.core
  (:require [clojure.string :as str]))

(def val->str
  (partial get "AKQJT98xxxxxx"))

(defn random-hand []
  (let [deck (for [suit (range 4)
                   val (range 13)]
               [suit val])]
    (->> deck
         shuffle
         (take 13)
         sort
         (partition-by first)
         (map (fn [suit]
                (->> suit
                     (map (fn [card]
                            (val->str (second card))))
                     (apply str))))
         vec)))

(defn card-score [card]
  (case card
    "A" 4
    "K" 3
    "Q" 2
    "J" 1
    0))

(defn score [results]
  (->> results
       (map second)
       (reduce +)))

(defn high-card-points [hand]
  (let [points (->> hand
                    (apply concat)
                    (map card-score)
                    (reduce +))]
    [["High card points" points]]))

(defn ace-ten-points [hand]
  (let [A-T-count (->> hand
                       (apply concat)
                       (filter #{"A" "T"})
                       count)
        points (* 0.25 (- A-T-count 2))
        reason (str "A and T count is " A-T-count)]
    (when-not (zero? points)
      [[reason points]])))

(defn short-honor-suit-points [suit]
  (condp re-matches suit
    #"[AKQ]"
    [["Blank A/K/Q" -1]]
    #"J"
    [["Blank J" -0.75]]
    #"T"
    [["Blank T" -0.25]]
    #"QJ"
    [["Blank QJ" -1]]
    #"AK|AKQ|AJ|KQ|KQJ|KJ|QT|JT"
    [["Blank AK/AKQ/AJ/KQ/KQJ/KJ/QT/JT" -0.5]]
    #"AKJ|AQJ|AQ|AT|KT|Q.|J.|T."
    [["Blank AKJ/AQJ/AQ/AT/KT/Qx/Jx/Tx" -0.25]]
    nil))

(defn short-honor-points [hand]
  (mapcat short-honor-suit-points hand))

(defn basic-points [hand]
  (concat (high-card-points hand)
          (ace-ten-points hand)
          (short-honor-points hand)))

(defn okay-suit? [suit]
  (re-find #"A|KQ|KJ|QJT" suit))

(defn strong-suit? [suit]
  (re-find #"AKQ|AKJ|AQJT|KQJT" suit))

(defn ground-length-points [suit]
  (let [length (count suit)
        okay-or-strong? (okay-suit? suit)
        weak? (not okay-or-strong?)
        strong? (strong-suit? suit)
        weak-or-okay? (not strong?)
        okay? (and okay-or-strong? weak-or-okay?)]
    (cond
      (and (= length 5) weak?)
      [["Weak five card suit" 0.5]]
      (and (= length 4) okay-or-strong?)
      [["Okay or strong four card suit" 0.5]]
      (and (= length 6) weak?)
      [["Weak six card suit" 1]]
      (and (= length 5) okay?)
      [["Okay five card suit" 1]]
      (and (>= length 7) weak-or-okay?)
      [["Weak or okay seven card suit or longer" 2]]
      (and (= length 6) okay?)
      [["Okay six card suit" 2]]
      (and (= length 5) strong?)
      [["Strong five card suit" 1.5]]
      (and (>= length 6) strong?)
      [["Strong six card suit or longer" 3]])))

(defn ground-honor-points [suit]
  (when (re-find #"AKJ" suit) ; TODO: blank? AKQ?
    [["Suit with AKJ" 0.25]]))

(def honors #{"A" "K" "Q" "J"})

(defn ground-partner-honor-points [suit]
  (let [honor-count (->> suit
                         (filter honors)
                         count)]
    (case honor-count
      0 nil
      1 [["Honor in partner color" 0.5]]
      [["Two or more honors in partner color" 1]])))

(defn ground-partner-shortness-points [suit]
  (case (count suit)
    0 [["Chikane in partner color" -2]]
    1 [["Single in partner color" -1]]
    nil))

(defn ground-partner-suit-points [suit]
  (concat (ground-partner-honor-points suit)
          (ground-partner-shortness-points suit)))

(defn ground-right-opponent-suit-honor-points [suit]
  (cond
    (re-find #"^K(Q|J)." suit)
    [["KQx/KJx in right opponent color" 1]]
    (re-find #"^(K.|Q..|AJ.|AKT|AQ)" suit)
    [["Kx/Qxx/AJx/AKT/AQ in right opponent color" 0.5]]))

(defn ground-left-opponent-suit-honor-points [suit]
  (cond
    (re-find #"^K(Q|J)." suit)
    [["KQx/KJx in left opponent color" -1]]
    (re-find #"^(K.|Q..|AJ.|AKT|AQ)" suit)
    [["Kx/Qxx/AJx/AKT/AQ in left opponent color" -0.5]]))

(defn ground-opponent-suit-length-points [suit]
  (when-not (okay-suit? suit) ; TODO: solid = ok? strong? other?
    (let [length-points (score (ground-length-points suit))
          discount (min length-points 1)]
      (when (pos? discount)
        [["Non-solid length in opponent color" (- discount)]]))))

(defn ground-left-opponent-double-points [suit]
  (case suit
    "KJ" [["Blank KJ in left opponent color" -1]]
    "AJ" [["Blank AJ in left opponent color" -0.25]]
    nil))

(defn ground-right-opponent-suit-points [suit]
  (concat
   (ground-right-opponent-suit-honor-points suit)
   (ground-opponent-suit-length-points suit)))


(defn ground-left-opponent-suit-points [suit]
  (concat
   (ground-left-opponent-suit-honor-points suit)
   (ground-left-opponent-double-points suit)
   (ground-opponent-suit-length-points suit)))

(defn ground-suit-points [suit info]
  (concat
   (when-not (= info :trump)
     (ground-length-points suit))
   (ground-honor-points suit)
   (case info
     :left    (ground-left-opponent-suit-points suit)
     :partner (ground-partner-suit-points suit)
     :right   (ground-right-opponent-suit-points suit)
     nil)))

(defn ground-points [hand suit-infos]
  (concat (basic-points hand)
          (mapcat ground-suit-points hand suit-infos)))

(defn no-trump-T-9-points [hand]
  (let [T-9-count (->> hand
                       (apply concat)
                       (filter #{"T" "9"})
                       count)
        points (* 0.25 (- T-9-count 2))
        reason (str "T and 9 count in NT is " T-9-count)]
    (when-not (zero? points)
      [[reason points]])))

(defn no-trump-QJxx-suit-points [suit]
  (when (re-find #"QJ.." suit)
    [["QJxx in NT" 0.25]]))

(defn no-trump-right-opponent-suit-covered-T-points [suit info]
  (when (and (= info :right)
             (re-find #"T9" suit)
             (>= (count suit) 2))   ; TODO: definition of covered?
    [["Covered T in right opponent color in NT" 0.5]]))

(defn no-trump-opponent-suit-QJT9-or-QJT8-points [suit info]
  (when (and (#{:left :right} info)
             (re-find #"QJT(9|8)" suit))
    [["QJT9 or QJT8 in opponent color in NT" 0.5]]))

(defn no-trump-right-opponent-suit-JTxx-or-J9xx-points [suit info]
  (when (and (= info :right)
             (re-find #"J(T|9).." suit))
    [["JTxx or J9xx in right opponent color in NT" 0.25]]))

(defn no-trump-suit-points [suit info]
  (concat (no-trump-QJxx-suit-points suit)
          (no-trump-right-opponent-suit-covered-T-points suit info)
          (no-trump-opponent-suit-QJT9-or-QJT8-points suit info)
          (no-trump-right-opponent-suit-JTxx-or-J9xx-points suit info)))

(defn no-trump-points [hand suit-infos]
  (concat (ground-points hand suit-infos)
          (no-trump-T-9-points hand)
          (mapcat no-trump-suit-points hand suit-infos)))

(defn trump-suit-length-points [suit]
  (let [length (count suit)
        points (- length 5)
        reason (str "Trump suit length is " length)]
    (when (pos? points)
      [[reason points]])))

(defn trump-suit-promised-points [suit promised]
  (let [length (count suit)
        points (- length promised)
        reason (str "Promised " promised
                    " trumps, got " length)]
    (when-not (zero? points)
      [[reason points]])))

(defn trump-suit-honors [suit]
  (let [points (->> suit
                    butlast
                    (map (fn [card]
                           (case card
                             "A" 1.5
                             ("K" "Q" "J") 1
                             "T" 0.75
                             "9" 0.25
                             0)))
                    (reduce +)
                    (min 2.5))
        reason "Trump honors (A->1.5, K/Q/J-> 1, T->0.75, 9->0.25, lowest card excluded, max 2.5)"]
    (when (pos? points)
      [[reason points]])))

(defn non-trump-suits [hand suit-infos]
  (->> (map (fn [suit info]
              (when-not (= info :trump)
                suit))
            hand
            suit-infos)
       (remove nil?)))

(defn trump-other-suit-honors [hand suit-infos]
  (let [other-suits (apply str (non-trump-suits hand suit-infos))
        other-honors (filter honors other-suits)
        points (->> other-honors
                    (map (fn [card]
                           (case card
                             "A" 0.5
                             "K" 0
                             ("Q" "J") -0.5)))
                    (reduce +))
        reason "Other suit honors (A->0.5, Q/J->-0.5)"]
    (when-not (empty? other-honors)
      [[reason points]])))

(defn trump-other-short-suits [hand suit-infos]
  ;; TODO: substract partner short suits - definition?
  (let [other-suits (non-trump-suits hand suit-infos)
        points (->> other-suits
                    (map count)
                    (map {2 0.5
                          1 1.5
                          0 3})
                    (remove nil?)
                    (reduce +))
        reason "Short other suits (Double->0.5, Single->1.5, Chikane->3)"]
    (when (pos? points)
      [[reason points]])))

(defn trump-opponent-short-suits [hand suit-infos]
  (let [points (->> (map (fn [suit info]
                           (let [length (count suit)]
                             (cond
                               (and (= info :left)
                                    (<= length 2))
                               0.5
                               (and (= info :right)
                                    ;; TODO: if left has at least 3 double is fine
                                    (<= length 1))
                               0.5
                               :else
                               0)))
                         hand suit-infos)
                    (reduce +))
        reason "Short opponent suits (except double in right opponent suit)"]
    (when (pos? points)
      [[reason points]])))

(defn trump-suit-points [suit info promised]
  (when (= info :trump)
    (concat (trump-suit-length-points suit)
            (trump-suit-promised-points suit promised)
            (trump-suit-honors suit))))

(defn trump-points [hand suit-infos promised]
  (concat (ground-points hand suit-infos)
          [["Basic trump points correction" -1.5]]
          (mapcat trump-suit-points
                  hand
                  suit-infos
                  (repeat promised))
          (trump-other-suit-honors hand suit-infos)
          (trump-other-short-suits hand suit-infos)
          (trump-opponent-short-suits hand suit-infos)
          ;; TODO: partner short suits
          ))
