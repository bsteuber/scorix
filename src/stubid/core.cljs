(ns stubid.core
  (:require-macros [stubid.core])
  (:require [scorix.core :as scorix]
            [clojure.string :as str]))

(def ^:dynamic *context* {})

(defn warn [& args]
  (apply js/console.warn args))

(defn prev-bids []
  (:prev-bids *context*))

(def pass :pass)

(def clubs 0)
(def diamonds 1)
(def hearts 2)
(def spades 3)
(def no-trump 4)
(def minor? #{clubs diamonds})
(def major? #{hearts spades})
(def suit? #{clubs diamonds hearts spades})

(defn as-set [x]
  (if (set? x)
    x
    #{x}))

(defn enumerate-bids [pattern]
  (if (= pattern :pass)
    [:pass]
    (let [[level suit-or-set] pattern]
      (for [suit (as-set suit-or-set)]
        [level suit]))))

(defn not-pass? [{:keys [bid]}]
  (not= :pass bid))

(defn bid-allowed? [pattern bid]
  (let [allowed? (as-set pattern)]
    (allowed? bid)))

(defn last-bid [bids]
  (->> bids
       reverse
       (filter not-pass?)
       first
       :bid))

(defn prev-bids-match? [patterns bids]
  (->> bids
       (filter not-pass?)

       (map :bid)
       (map bid-allowed? patterns)
       (every? some?)))

(defn match-bids [prev-pattern bid-pattern]
  (when (prev-bids-match? prev-pattern (prev-bids))
    (->> (enumerate-bids bid-pattern)
         (filter bid-allowed?))))

(defn try-bids [rule-prev rule-bid pred]
  (let [allowed (filter bids (fn [bid]
                               (binding [*context* (assoc *context*
                                                          :bid bid)]
                                 (pred))))]
    (when (> (count allowed 1))
      (warn "Multiple bids allowed by rule" rule-prev rule-bid))
    (first allowed)))


;; (def spades 0)
;; (def hearts 1)
;; (def diamonds 2)
;; (def clubs 3)
;; (def no-trump -1)

;; (defmulti eval-rule :key)

;; (def ^:dynamic *debug?* false)
;; (def ^:dynamic *indent* 0)

;; (defn dbg [& args]
;;   (when *debug?*
;;     (let [space (str/join (repeat (* 2 *indent*) " "))]
;;       (apply println space args))))

;; (defn apply-rule [rule hand bids]
;;   (let [[key & args] (if (vector? rule)
;;                        rule
;;                        [rule])
;;         _ (dbg "start" key (subs (pr-str args) 0 45) ":")
;;         res          (binding [*indent* (inc *indent*)]
;;                        (eval-rule {:key   key
;;                                    :args  args
;;                                    :bids  bids
;;                                    :hand  hand}))]
;;     (dbg "end" key "->" res)
;;     res))

;; (defn eval-args [{:keys [hand bids args]}]
;;   (->> args
;;        (map (fn [rule-or-constant]
;;               (if (or (vector? rule-or-constant)
;;                       (keyword? rule-or-constant))
;;                 (apply-rule rule-or-constant hand bids)
;;                 rule-or-constant)))))

;; (def all-bids
;;   (for [level (range 1 8)
;;         suit [clubs diamonds hearts spades no-trump]]
;;     [level suit]))

;; (def pass [-1 -1])

;; (defmethod eval-rule :else [_]
;;   true)

;; (defmethod eval-rule :fail [{:keys [args]}]
;;   (apply println "FAIL:" args))

;; (defmethod eval-rule :not [{:keys [hand bids args]}]
;;   (not (apply-rule (first args) hand bids)))

;; (defmethod eval-rule :and [{:keys [hand bids args]}]
;;   (every? (fn [rule]
;;             (apply-rule rule hand bids))
;;           args))

;; (defmethod eval-rule :or [{:keys [hand bids args]}]
;;   (some (fn [rule]
;;           (apply-rule rule hand bids))
;;         args))

;; (defmethod eval-rule :<= [options]
;;   (apply <= (eval-args options)))

;; (defmethod eval-rule :>= [options]
;;   (apply >= (eval-args options)))

;; (defmethod eval-rule := [options]
;;   (apply = (eval-args options)))

;; (defmethod eval-rule :element [options]
;;   (let [[elt coll] (eval-args options)]
;;     (some #{elt} coll)))

;; (defmethod eval-rule :suit-under [options]
;;   (apply > (eval-args options)))

;; (defmethod eval-rule :suit-over [options]
;;   (apply < (eval-args options)))

;; (defmethod eval-rule :cond [{:keys [hand bids args]}]
;;   (when-let [rule (->> args
;;                        (partition 2)
;;                        (some (fn [[match-rule rule]]
;;                                (when (apply-rule match-rule hand bids)
;;                                  rule))))]
;;     (apply-rule rule hand bids)))

;; (defn no-bids-yet? [bids]
;;   (not (some :bid bids)))

;; (defmethod eval-rule :opening? [{:keys [bids]}]
;;   (no-bids-yet? bids))

;; (defmethod eval-rule :bids-match? [{:keys [args bids]}]
;;   (->> bids
;;        (filter :bid)
;;        (map (fn [{:keys [player bid]}]
;;               (let [[level suit] bid]
;;                 [player level suit])))
;;        (= args)))

;; (defmethod eval-rule :undisturbed? [{:keys [bids]}]
;;   (not (some (fn [{:keys [player bid]}]
;;                (and bid
;;                     (#{:left :right} player)))
;;              bids)))

;; (defmethod eval-rule :partner-opened? [{:keys [bids]}]
;;   (->> bids
;;        (filter :bid)
;;        first
;;        :player
;;        (= :partner)))

;; (defn find-longest-suits [hand]
;;   (let [lengths (map count hand)
;;         longest-length (apply max lengths)
;;         longest-suits (->> lengths
;;                            (map-indexed (fn [suit length]
;;                                           (when (= length longest-length)
;;                                             suit)))
;;                            (filter some?))]
;;     [longest-suits longest-length]))

;; (defmethod eval-rule :color? [context]
;;   (let [[suit] (eval-args context)]
;;     (not= no-trump suit)))

;; (defmethod eval-rule :nt? [context]
;;   (let [[suit] (eval-args context)]
;;     (= no-trump suit)))

;; (defmethod eval-rule :suit [{:keys [suit]}]
;;   suit)

;; (defmethod eval-rule :level [{:keys [level]}]
;;   level)

;; (defmethod eval-rule :bid [context]
;;   (let [[level suit] (eval-args context)]
;;     [level suit]))

;; (defmethod eval-rule :pass [_]
;;   pass)

;; (defn round-points [points]
;;   (js/Math.round (- points 0.1)))

;; (defmethod eval-rule :gp [{:keys [hand]}]
;;   (round-points
;;    (scorix/score
;;     (scorix/ground-points hand [nil nil nil nil]))))

;; (defmethod eval-rule :length [{:keys [hand args]}]
;;   (let [[_ length] (find-longest-suits hand)]
;;     length))

;; (defmethod eval-rule :length-in [{:keys [hand]
;;                                   :as context}]
;;   (let [[suit] (eval-args context)]
;;     (when (and suit (not= no-trump suit))
;;       (count (hand suit)))))

;; (defmethod eval-rule :length-over-suit [{:keys [hand]
;;                                          :as context}]
;;   (let [[suit] (eval-args context)]
;;     (->> (range 4)
;;          (filter (partial > suit))
;;          (map (comp count hand))
;;          (apply max 0))))

;; (defmethod eval-rule :length-under-suit [{:keys [hand]
;;                                          :as context}]
;;   (let [[suit] (eval-args context)]
;;     (->> (range 4)
;;          (filter (partial < suit))
;;          (map (comp count hand))
;;          (apply max 0))))

;; (defmethod eval-rule :partner-opened? [{:keys [bids]}]
;;   (let [bids (filter :bid bids)]
;;     (and (= 1 (count bids))
;;          (= :partner (:player (first bids))))))

;; (defn last-partner-bid [bids]
;;   (->> bids
;;        (filter :bid)
;;        (filter (comp #{:partner} :player))
;;        last
;;        :bid))

;; (defmethod eval-rule :partner-level [{:keys [bids]}]
;;   (first (last-partner-bid bids)))

;; (defmethod eval-rule :partner-suit [{:keys [bids]}]
;;   (second (last-partner-bid bids)))

;; (defn trump-suit-info [trump-suit]
;;   (mapv (fn [suit]
;;           (when (= suit trump-suit)
;;             :trump))
;;         (range 4)))

;; (defmethod eval-rule :yp [{:keys [hand] :as context}]
;;   (let [[promised suit] (eval-args context)
;;         yp              (scorix/trump-points hand (trump-suit-info suit) promised)
;;         rounded-yp      (round-points (scorix/score yp))]
;;     rounded-yp))

;; (def majors [spades hearts])
;; (def minors [diamonds clubs])

;; (defn longest-of [hand suits]
;;   (let [length (->> suits
;;                     (map (comp count hand))
;;                     (apply max))
;;         suits (->> suits
;;                    (filter #(= length (count (hand %)))))]
;;     (when (seq suits)
;;       [length suits])))

;; (defmethod eval-rule :longest-length [{:keys [hand] :as context}]
;;   (let [[suits]    (eval-args context)
;;         [length _] (longest-of hand suits)]
;;     length))

;; (defmethod eval-rule :highest-longest [{:keys [hand] :as context}]
;;   (let [[suits]        (eval-args context)
;;         [length suits] (longest-of hand suits)]
;;     (first suits)))

;; (defmethod eval-rule :lowest-longest [{:keys [hand] :as context}]
;;   (let [[suits]        (eval-args context)
;;         [length suits] (longest-of hand suits)]
;;     (last suits)))

;; (defmethod eval-rule :distribution-4441? [{:keys [hand]}]
;;   (let [[longest-suits _] (find-longest-suits hand)]
;;     (= 3 (count longest-suits))))

;; (defmethod eval-rule :majors-44? [{:keys [hand]}]
;;   (->> hand
;;        (take 2)
;;        (map count)
;;        (every? #{4})))

;; (defn suit-hcp [cards]
;;   (->> cards
;;        (map {"A" 4
;;              "K" 3
;;              "Q" 2
;;              "J" 1})
;;        (filter some?)
;;        (reduce +)))

;; (defn find-best-suits [hand suits]
;;   (let [hcps (->> hand
;;                   (mapv suit-hcp))
;;         best-hcp (->> suits
;;                       (map hcps)
;;                       (apply max))]
;;     (->> suits
;;          (filter (fn [suit]
;;                    (= (hcps suit)
;;                       best-hcp))))))

;; (def major-suit? #{0 1})
;; (def minor-suit? #{2 3})

;; (defmethod eval-rule :biddable-major-5+ [{:keys [hand]}]
;;   (->> [spades hearts]
;;        (filter (fn [suit]
;;                  (let [cards (hand suit)]
;;                    (when
;;                        (and
;;                         (>= (count cards) 5)
;;                         (scorix/reasonable-suit? cards))
;;                      suit))))
;;        seq))

;; (defmethod eval-rule :majors [_]
;;   majors)

;; (defmethod eval-rule :minors [_]
;;   minors)

;; (defmethod eval-rule :diamonds-or-higher [_]
;;   [spades hearts diamonds])

;; (defmethod eval-rule :all-suits [_]
;;   [spades hearts diamonds clubs])

;; (defn nt-hand? [hand]
;;   (let [lengths (map count hand)]
;;     (and
;;      (every? #(> % 1) lengths)
;;      (<= (->> lengths
;;               (filter #(= % 2))
;;               count)
;;          1))))

;; (defmethod eval-rule :nt-distribution? [{:keys [hand]}]
;;   (nt-hand? hand))

(defn ->interval [{:keys [min max]}]
  [(or min 0) (or max 40)])

(defn check-interval [[x-min x-max]]
  (when-not (<= x-min x-max)
    (throw "Impossible interval configuration")))

(defn interval-restrict [[x-min x-max] [y-min y-max]]
  [(max x-min y-min)
   (min x-max y-max)])

(defn interval+ [[x-min x-max] [y-min y-max]]
  [(+ x-min y-min)
   (+ x-max y-max)])

(defn interval- [[x-min x-max] [y-min y-max]]
  [(- x-min y-max)
   (- x-max y-min)])
