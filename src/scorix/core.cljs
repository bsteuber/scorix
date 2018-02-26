(ns scorix.core
  (:require [clojure.string :as str]))

(defn random-hand []
  (let [deck (for [suit (range 4)
                   val (range 13)]
               [suit val])
        format-card (partial get "AKQJT98xxxxxx")]
    (->> deck
         shuffle
         (take 13)
         sort
         (partition-by first)
         (map (fn [suit]
                (->> suit
                     (map (fn [card]
                            (format-card (second card))))
                     (apply str))))
         vec)))

(defn card-score [card]
  (case card
    "A" 4
    "K" 3
    "Q" 2
    "J" 1
    0))

(defn sum [f coll]
  (->> coll
       (map f)
       (reduce +)))

(defn score [results]
  (sum first results))

(defn high-card-points [hand]
  (let [points (->> hand
                    (apply concat)
                    (sum card-score))]
    [[points :high-card-points]]))

(defn ace-ten-points [hand]
  (let [A-T-count (->> hand
                       (apply concat)
                       (filter #{"A" "T"})
                       count)
        points (* 0.25 (- A-T-count 2))]
    (when-not (zero? points)
      [[points :A-T-count A-T-count]])))

(defn short-honor-suit-points [suit]
  (when-let [points (condp re-matches suit
                      #"[AKQ]" -1
                      #"J" -0.75
                      #"T" -0.25
                      #"QJ" -1
                      #"AK|AKQ|AJ|KQ|KQJ|KJ|QT|JT" -0.5
                      #"AKJ|AQJ|AQ|AT|KT|Q.|J.|T." -0.25
                      nil)]
    [[points :blank suit]]))

(defn short-honor-points [hand]
  (mapcat short-honor-suit-points hand))

(defn basic-points [hand]
  (concat (high-card-points hand)
          (ace-ten-points hand)
          (short-honor-points hand)))

(defn reasonable-suit? [suit]
  (re-find #"A|KQ|KJ|QJT" suit))

(defn strong-suit? [suit]
  (re-find #"AKQ|AKJ|AQJT|KQJT" suit))

(defn ground-length-points [suit info]
  (when-not (= info :trump)
    (let [length (count suit)
          reasonable-or-strong? (reasonable-suit? suit)
          weak? (not reasonable-or-strong?)
          strong? (strong-suit? suit)
          weak-or-reasonable? (not strong?)
          reasonable? (and reasonable-or-strong?
                           weak-or-reasonable?)
          points (cond
                   (and (= length 5) weak?) 0.5
                   (and (= length 4) reasonable-or-strong?) 0.5
                   (and (= length 6) weak?) 1
                   (and (= length 5) reasonable?) 1
                   (and (>= length 7) weak-or-reasonable?) 2
                   (and (= length 6) reasonable?) 2
                   (and (= length 5) strong?) 1.5
                   (and (>= length 6) strong?) 3)]
      (when points
        [[points :length suit]]))))

(defn ground-AQJ-points [hand]
  (let [points (->> hand
                    (filter (partial re-find #"AQJ"))
                    count
                    (* 0.25))]
    (when (pos? points)
      [[points :AQJ]])))

(def honors #{"A" "K" "Q" "J"})

(defn ground-partner-honor-points [suit]
  (let [honor-count (->> suit
                         (filter honors)
                         count)
        points (case honor-count
                 0 nil
                 1 0.5
                 1)]
    (when points
      [[points :partner-suit-honors honor-count]])))

(defn ground-partner-shortness-points [suit]
  (when-let [points (case (count suit)
                 0 -2
                 1 -1
                 nil)]
    [[points :short-in-partner-suit suit]]))

(defn ground-partner-suit-points [suit info]
  (when (= info :partner)
    (concat (ground-partner-honor-points suit)
            (ground-partner-shortness-points suit))))

(defn ground-right-opponent-suit-honor-points [suit]
  (condp re-find suit
    #"^K(Q|J)."
    :>> #(vector [1 :gap-in-right-suit (first %)])
    #"^(K.|Q..|AJ.|AKT|AQ)"
    :>> #(vector [0.5 :gap-in-right-suit (first %)])
    nil))

(defn ground-left-opponent-suit-honor-points [suit]
  (condp re-find suit
    #"^K(Q|J)."
    :>> #(vector [-1 :gap-in-left-suit (first %)])
    #"^(K.|Q..|AJ.|AKT|AQ)"
    :>> #(vector [-0.5 :gap-in-left-suit (first %)])
    nil))

(defn solid? [suit]
  ; first gap ends the length, recalc
  )

(defn ground-opponent-suit-length-points [suit]
  (when-not (reasonable-suit? suit) ; TODO: solid = ok? strong? other?
    (let [length-points (score (ground-length-points suit nil))
          discount (min length-points 1)]
      (when (pos? discount)
        [[(- discount) :length-in-opponent-suit suit]]))))

(defn ground-left-opponent-double-points [suit]
  (when-let [points (case suit
                 "KJ" -1
                 "AJ" -0.25
                 nil)]
    [[points :blank-honors-in-left-opponent-suit suit]]))

(defn ground-right-opponent-suit-points [suit info]
  (when (= info :right)
    (concat
     (ground-right-opponent-suit-honor-points suit)
     (ground-opponent-suit-length-points suit))))

(defn ground-left-opponent-suit-points [suit info]
  (when (= info :left)
    (concat
     (ground-left-opponent-suit-honor-points suit)
     (ground-left-opponent-double-points suit)
     (ground-opponent-suit-length-points suit))))

(defn ground-points [hand suit-infos]
  (concat (basic-points hand)
          (mapcat ground-length-points hand suit-infos)
          (ground-AQJ-points hand)
          (mapcat ground-partner-suit-points hand suit-infos)
          (mapcat ground-left-opponent-suit-points hand suit-infos)
          (mapcat ground-right-opponent-suit-points hand suit-infos)))

(defn no-trump-T-9-points [hand]
  (let [T-9-count (->> hand
                       (apply concat)
                       (filter #{"T" "9"})
                       count)
        points (* 0.25 (- T-9-count 2))
        reason (str  T-9-count)]
    (when-not (zero? points)
      [[points :T-9-count-in-SA T-9-count]])))

(defn no-trump-QJxx-suit-points [suit]
  (when-let [matching (and (not (reasonable-suit? suit))
                      (re-find #"QJ.." suit))]

    (when [[0.25 :QJxx-in-NT matching]])))

(defn no-trump-right-opponent-suit-covered-T-points [suit info]
  ;; 16 cases, right has all other honors - will the T make a trick?
  (when (and (= info :right)
             (re-find #"T9" suit)
             (>= (count suit) 2))   ; TODO: definition of covered?
    [[0.5 :covered-T-in-right-suit-in-NT]]))

(defn no-trump-opponent-suit-QJT9-or-QJT8-points [suit info]
  (when-let [matching (and (#{:left :right} info)
                           (re-find #"QJT(9|8)" suit))]
    [[0.5 :QJT9-or-QJT9-in-opponent-suit-in-NT (first matching)]]))

(defn no-trump-right-opponent-suit-JTxx-or-J9xx-points [suit info]
  (when-let [matching (and (= info :right)
                           (re-find #"J(T|9).." suit))]
    [[0.25 :JTxx-or-J9xx-in-right-opponent-suit-in-NT (first matching)]]))

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
        points (- length 5)]
    (when (pos? points)
      [[points :trump-suit-length length]])))

(defn trump-suit-promised-points [suit promised]
  (let [length (count suit)
        points (- length promised)
        more-or-less (if (pos? points)
                       "more"
                       "less")
        extra-or-missing (if (pos? points)
                           points
                           (- points))]
    (when-not (zero? points)
      [[points :more-or-less-trumps-than-promised extra-or-missing more-or-less]])))

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
                    (min 2.5))]
    (when (pos? points)
      [[points :high-trump-cards]])))

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
                    (reduce +))]
    (when-not (empty? other-honors)
      [[points :honors-in-non-trump-suits]])))

(defn trump-other-short-suits [hand suit-infos]
  ;; TODO: substract partner short suits, min 0
  (let [other-suits (non-trump-suits hand suit-infos)
        points (->> other-suits
                    (map count)
                    (map {2 0.5
                          1 1.5
                          0 3})
                    (remove nil?)
                    (reduce +))]
    (when (pos? points)
      [[points :short-non-trump-suits]])))

(defn trump-opponent-short-suits [hand suit-infos]
  (let [points (->> (map (fn [suit info]
                           (let [length (count suit)]
                             (cond
                               (and (= info :left)
                                    (<= length 2))
                               0.5
                               (and (= info :right)
                                    ;; TODO: if left has at least 3 cards, doubleton is reasonable
                                    (<= length 1))
                               0.5
                               :else
                               0)))
                         hand suit-infos)
                    (reduce +))]
    (when (pos? points)
      [[points :short-non-trump-opponent-suits]])))

(defn trump-suit-points [suit info promised]
  (when (= info :trump)
    (concat (trump-suit-length-points suit)
            (trump-suit-promised-points suit promised)
            (trump-suit-honors suit))))

(defn trump-points [hand suit-infos promised]
  (concat (ground-points hand suit-infos)
          [[-1.5 :trump-basic-correction]]
          (mapcat trump-suit-points
                  hand
                  suit-infos
                  (repeat promised))
          (trump-other-suit-honors hand suit-infos)
          (trump-other-short-suits hand suit-infos)
          (trump-opponent-short-suits hand suit-infos)
          ;; TODO: partner short suits
          ;; Points minus
          ;; length plus (need half extra length)
          ))
