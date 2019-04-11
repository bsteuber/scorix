(ns stubid.core
  (:require [scorix.core :as scorix]))

(def spades 0)
(def hearts 1)
(def diamonds 2)
(def clubs 3)
(def no-trump -1)

(defmulti eval-rule :key)

(def ^:dynamic *debug?* false)

(defn apply-rule [rule hand bid bids]
  (let [[key & args] (if (vector? rule)
                       rule
                       [rule])
        _ (when *debug?*
            (print "apply" key bid))
        [level suit] bid
        res          (eval-rule {:key   key
                                 :args  args
                                 :bids  bids
                                 :bid   bid
                                 :level level
                                 :suit  suit
                                 :hand  hand})]
    (when *debug?*
      (println " ->" (boolean res)))
    res))

(def all-bids
  (for [level (range 1 8)
        suit [clubs diamonds hearts spades no-trump]]
    [level suit]))

(defn possible-bids [rule hand bids]
  (->> all-bids
       (filter (fn [bid]
                 (apply-rule rule hand bid bids)))))

(defmethod eval-rule :else [_]
  true)

(defmethod eval-rule :not [{:keys [hand bid bids args]}]
  (not (apply-rule (first args) hand bid bids)))

(defmethod eval-rule :and [{:keys [hand bid bids args]}]
  (every? (fn [rule]
            (apply-rule rule hand bid bids))
          args))

(defmethod eval-rule :or [{:keys [hand bid bids args]}]
  (some (fn [rule]
          (apply-rule rule hand bid bids))
        args))

(defn eval-numeric [{:keys [hand bid bids args]}]
  (->> args
       (map (fn [rule-or-number]
              (if (number? rule-or-number)
                rule-or-number
                (apply-rule rule-or-number hand bid bids))))))

(defmethod eval-rule :<= [options]
  (apply <= (eval-numeric options)))

(defmethod eval-rule :>= [options]
  (apply >= (eval-numeric options)))

(defmethod eval-rule := [options]
  (apply = (eval-numeric options)))

(defmethod eval-rule :cond [{:keys [hand bid bids args]}]
  (when-let [rule (->> args
                       (partition 2)
                       (some (fn [[match-rule rule]]
                               (when (apply-rule match-rule hand bid bids)
                                 rule))))]
    (apply-rule rule hand bid bids)))

(defn no-bids-yet? [bids]
  (not (some :bid bids)))

(defmethod eval-rule :first-bid [{:keys [bids]}]
  (no-bids-yet? bids))

(defmethod eval-rule :undisturbed [{:keys [bids]}]
  (not (some (fn [{:keys [player bid]}]
               (and bid
                    (#{:left :right} player)))
             bids)))

(defmethod eval-rule :partner-opened [{:keys [bids]}]
  (->> bids
       (filter :bid)
       first
       :player
       (= :partner)))

(defn find-longest-suits [hand]
  (let [lengths (map count hand)
        longest-length (apply max lengths)
        longest-suits (->> lengths
                           (map-indexed (fn [suit length]
                                          (when (= length longest-length)
                                            suit)))
                           (filter some?))]
    [longest-suits longest-length]))

(defmethod eval-rule :suit-bid [{:keys [suit]}]
  (not= no-trump suit))

(defmethod eval-rule :nt-bid [{:keys [suit]}]
  (= no-trump suit))

(defmethod eval-rule :suit [{:keys [suit]}]
  suit)

(defmethod eval-rule :level [{:keys [level]}]
  level)

(defn round-points [points]
  (js/Math.round (- points 0.1)))

(defmethod eval-rule :gcp [{:keys [hand]}]
  (round-points
   (scorix/score
    (scorix/ground-points hand [nil nil nil nil]))))

(defmethod eval-rule :length [{:keys [hand args]}]
  (let [[_ length] (find-longest-suits hand)]
    length))

(defmethod eval-rule :partner-length [{:keys [hand suit args bids]}]
  (let [[min-len max-len] args
        max-len           (or max-len 13)
        length            (count (hand suit))]
    (and
     (<= min-len length max-len)
     (->> bids
          (some (fn [{:keys [player bid]}]
                  (and bid (= player :partner))))))))

(defn trump-suit-info [trump-suit]
  (mapv (fn [suit]
          (when (= suit trump-suit)
            :trump))
        (range 4)))

(defmethod eval-rule :partner-yp-range [{:keys [hand suit args bids]}]
  (let [[partner-length
         min-yp
         max-yp]   args
        max-yp     (or max-yp 100)
        yp         (scorix/trump-points hand (trump-suit-info suit) partner-length)
        rounded-yp (round-points yp)]
    (<= min-yp rounded-yp max-yp)))

(defmethod eval-rule :longest-suit-bid [{:keys [hand suit]}]
  (some #{suit} (first (find-longest-suits hand))))

(defmethod eval-rule :highest-suit-bid [{:keys [hand suit]}]
  (= suit (first (first (find-longest-suits hand)))))

(defmethod eval-rule :distribution-4441 [{:keys [hand]}]
  (let [[longest-suits _] (find-longest-suits hand)]
    (= 3 (count longest-suits))))

(defmethod eval-rule :majors-44 [{:keys [hand]}]
  (->> hand
       (take 2)
       (map count)
       (every? #{4})))

(defn suit-hcp [cards]
  (->> cards
       (map {"A" 4
             "K" 3
             "Q" 2
             "J" 1})
       (filter some?)
       (reduce +)))

(defn find-best-suits [hand suits]
  (let [hcps (->> hand
                  (mapv suit-hcp))
        best-hcp (->> suits
                      (map hcps)
                      (apply max))]
    (->> suits
         (filter (fn [suit]
                   (= (hcps suit)
                      best-hcp))))))

(def major-suit? #{0 1})
(def minor-suit? #{2 3})

(defmethod eval-rule :lowest-best-minor [{:keys [hand suit]}]
  (let [[longest-suits _] (find-longest-suits hand)]
    (->> longest-suits
         (filter minor-suit?)
         (find-best-suits hand)
         last
         (= suit))))

(defmethod eval-rule :best-longest-suit [{:keys [hand suit]}]
  (let [[longest-suits _] (find-longest-suits hand)
        best-suits        (find-best-suits hand longest-suits)]
    (some #{suit} best-suits)))

(defmethod eval-rule :major [{:keys [suit]}]
  (major-suit? suit))

(defmethod eval-rule :minor [{:keys [suit]}]
  (minor-suit? suit))

(defn bid-on-level [level suits]
  (for [suit suits]
    [level suit]))

(defn nt-hand? [hand]
  (let [lengths (map count hand)]
    (and
     (every? #(> % 1) lengths)
     (<= (->> lengths
              (filter #(= % 2))
              count)
         1))))

(defmethod eval-rule :nt-distribution [{:keys [hand]}]
  (nt-hand? hand))
