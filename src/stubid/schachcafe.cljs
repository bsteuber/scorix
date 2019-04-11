(ns stubid.schachcafe
  (:require [stubid.core :as stu :refer [spades hearts diamonds clubs no-trump]]))

(def weak-minor-5?
  [:and
   [:>= :length 9]
   :minor?
   [:<= :gp 14]])

(def weak-major-4?
  [:and
   [:>= :length 8]
   :major?
   [:<= :gp 14]])

(def weak-minor-4?
  [:and
   [:= :length 8]
   :minor?
   [:<= :gp 12]])

(def weak-3?
  [:and
   [:= :length 7]
   [:<= 8 :gp 12]])

(def weak-2?
  [:and
   [:= :length 6]
   [:<= 8 :gp 12]])

(def suit-1?
  [:<= 13 :gp 20])

(def suit-1-openings
  [:cond
   [:>= :length 5]     :highest-suit-bid?
   :distribution-4441? :lowest-best-minor?
   :majors-44?         [:= :suit hearts]
   :else               :best-longest-suit?])

(def suit-openings
  [:and
   :longest-suit-bid?
   [:cond
    weak-minor-5? [:= :level 5]
    weak-major-4? [:= :level 4]
    weak-minor-4? [:= :level 4]
    weak-3?       [:= :level 3]
    weak-2?       [:= :level 2]
    suit-1?       [:and [:= :level 1] suit-1-openings]]])

(def nt-2?
  [:and
   [:<= 21 :gp 22]
   :nt-distribution?])

(def nt-1?
  [:>= :gp 21])

(def nt-openings
  [:cond
   nt-2? [:= :level 2]
   nt-1? [:= :level 1]])

(def openings
  [:and
   :opening?
   [:cond
    [:color? :suit] suit-openings
    [:nt? :suit]    nt-openings]])

(def partner-1-suit?
  [:and [:= :partner-level 1] [:color? :partner-suit]])

(def can-raise?
  [:and
   [:>= [:length-in :partner-suit] 4]
   [:>= [:yp 4 :partner-suit] 5]])

(def yp-5-9?
  [:<= 5 [:yp 4 :partner-suit] 9])

(def raise-level-2
  [:and
   [:= :level 2]
   [:= :suit :partner-suit]])

(def raise-partner
  [:cond
   yp-5-9? raise-level-2])

(def can-respond?
  [:>= :gp 5])

(def level-1-new-suit?
  [:>= [:length-over-suit :partner-suit] 4])

(def level-1-new-suit-responses
  [:and
   [:= :level 1]
   [:suit-over :suit :partner-suit]
   [:>= [:length-in :suit] 4]])

(def level-2-new-suit?
  [:and
   [:>= :gp 10]
   [:>= [:length-under-suit :partner-suit] 4]])

(def level-2-new-suit-responses
  [:and
   [:= :level 2]
   [:suit-under :suit :partner-suit]
   [:>= [:length-in :suit] 4]])

(def level-1-nt?
  [:and
   [:<= :gp 9]])

(def level-1-nt-response
  [:and [:nt? :suit] [:= :level 1]])

(def respond-suit-1
  [:cond
   level-1-new-suit? level-1-new-suit-responses
   level-2-new-suit? level-2-new-suit-responses
   level-1-nt?       level-1-nt-response
   ])

(def suit-1-responses
  [:cond
   can-raise?   raise-partner
   can-respond? respond-suit-1])

(def partner-1-nt?
  [:and
   [:nt? :partner-suit]
   [:= :partner-level 1]])

(def nt-1-major-responses
  [:element :suit :biddable-major-5+])

(def partner-1-nt-responses
  [:and
   [:= :level 2]
   [:cond
    [:<= :gp 3]        [:= :suit clubs]
    :biddable-major-5+ nt-1-major-responses
    :else              [:= :suit diamonds]]])

(def responses
  [:and
   :partner-opened?
   [:cond
    partner-1-suit? suit-1-responses
    partner-1-nt?   partner-1-nt-responses]])

(def bids-1-nt-2-clubs?
  [:bids-match?
   [:me 1 no-trump]
   [:partner 2 clubs]])

(def cont-1-nt-2-clubs
  [:cond
   [:>= :gp 25]
   [:and [:= :level 2] [:= :suit diamonds]]

   [:>= [:length-in spades] 5]
   [:bid-match? 2 spades]

   [:>= [:length-in hearts] 5]
   [:bid-match? 2 hearts]

   [:>= [:length-in diamonds] 5]
   [:bid-match? 3 diamonds]

   [:>= [:length-in clubs] 5]
   [:bid-match? 3 clubs]

   [:and [:<= 23 :gp 24] :nt-distribution?]
   [:bid-match? 2 no-trump]

   :else
   [:fail "Oups, I opened 1NT with 4441 :("]])

(def bids-1-nt-2-diamonds?
  [:bids-match?
   [:me 1 no-trump]
   [:partner 2 diamonds]])

(def cont-1-nt-2-diamonds
  )

(def nt-1-continuation
  [:cond
   bids-1-nt-2-clubs?    cont-1-nt-2-clubs
   bids-1-nt-2-diamonds? cont-1-nt-2-diamonds])

(def rules
  [:or
   openings
   responses
   nt-1-continuation])

(defn bid-valid?
  ([hand bid]
   (bid-valid? hand bid []))
  ([hand bid bids]
   (stu/apply-rule rules hand bid bids)))

(defn bids
  ([hand]
   (bids hand []))
  ([hand prev-bids]
   (stu/possible-bids rules hand prev-bids)))
