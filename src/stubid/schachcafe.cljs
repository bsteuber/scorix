(ns stubid.schachcafe
  (:require [stubid.core :as stu :refer [spades hearts diamonds clubs no-trump
                                         minors majors gp suit-length]]
            [clojure.string :as str]))

(defn weak-minor-5 []
  (for [suit minors]
    [[:and
      [:>= (suit-length suit) 9]
      [:<= (gp) 14]]
     [:bid 5 suit]]))

(defn weak-major-4 []
  (for [suit stu/majors]
    [[:and
      [:>= (suit-length suit) 8]
      [:<= (gp) 14]]
     [:bid 4 suit]]))

(defn weak-minor-4 []
  (for [suit stu/minors]
    [[:and
      [:= (suit-length suit) 8]
      [:<= (gp) 12]]
     [:bid 4 suit]]))

(defn weak-3 []
  (for [suit stu/suits]
    [[:and
      [:= (suit-length suit) 7]
      [:>= (gp) 8]
      [:<= (gp) 12]]
     [:bid 3 suit]]))

(defn weak-2 []
  (for [suit (reverse stu/suits)]
    [[:and
      [:= (suit-length suit) 6]
      [:>= (gp) 8]
      [:<= (gp) 12]]
     [:bid 2 suit]]))

(defn suit-1 []
  [[:>= (gp) 13]
   [:cond
    (for [suit (reverse stu/suits)]
      [[:>= (suit-length suit) 5]
       [:bid 1 suit]])
    [:distribution-4441?
     [:cond
      (for [suit stu/minors]
        [[:>= (suit-length suit) 4]
         [:bid 1 suit]])]]
    [:majors-44?
     [:bid 1 hearts]]
    (for [suit stu/suits]
      [[:and
        [:>= (suit-length suit) 4]
        [:best-longest-suit? suit]]
       [:bid 1 suit]])]])

(defn suit-openings []
  [:cond
   (weak-minor-5)
   (weak-major-4)
   (weak-minor-4)
   (weak-3)
   (weak-2)
   (suit-1)
   [:else
    :pass]])

;; (def nt-2?
;;   [:and
;;    [:<= 21 (gp) 22]
;;    :nt-distribution?])

;; (def nt-1?
;;   [:>= (gp) 21])

;; (def nt-openings
;;   [:cond
;;    nt-2? [:bid 2 no-trump]
;;    nt-1? [:bid 1 no-trump]])

(defn openings []
  [:or
   #_nt-openings
   (suit-openings)])

;; (def bids-1suit?
;;   [:and [:= :last-partner-level 1] [:color? :last-partner-suit]])

;; (def can-raise?
;;   [:and
;;    [:>= [:length-in :last-partner-suit] 4]
;;    [:>= [:yp 4 :last-partner-suit] 5]])

;; (def yp-5-9?
;;   [:<= 5 [:yp 4 :last-partner-suit] 9])

;; (def yp-10-11?
;;   [:<= 10 [:yp 4 :last-partner-suit] 11])

;; (def yp-12+?
;;   [:<= 12 [:yp 4 :partner-suit]])

;; (def raise-partner
;;   [:cond
;;    yp-5-9?   [:bid 2 :last-partner-suit]
;;    yp-10-11? [:bid 3 :last-partner-suit]
;;    yp-12+?   [:bid 2 no-trump]])

;; (def can-respond?
;;   [:>= (gp) 5])

;; (def level-1-new-suit?
;;   [:>= [:length-over-suit :partner-suit] 4])

;; (def level-1-new-suit-responses
;;   [:and
;;    [:= :level 1]
;;    [:suit-over :suit :partner-suit]
;;    [:>= [:length-in :suit] 4]])

;; (def level-2-new-suit?
;;   [:and
;;    [:>= (gp) 10]
;;    [:>= [:length-under-suit :partner-suit] 4]])

;; (def level-2-new-suit-responses
;;   [:and
;;    [:= :level 2]
;;    [:suit-under :suit :partner-suit]
;;    [:>= [:length-in :suit] 4]])

;; (def level-1-nt?
;;   [:and
;;    [:<= (gp) 9]])

;; (def level-1-nt-response
;;   [:and [:nt? :suit] [:= :level 1]])

;; (def respond-suit-1
;;   [:cond
;;    level-1-new-suit? level-1-new-suit-responses
;;    level-2-new-suit? level-2-new-suit-responses
;;    level-1-nt?       level-1-nt-response
;;    ])

;; (def cont-1suit
;;   [:cond
;;    can-raise?   raise-partner
;;    can-respond? respond-suit-1])

;; (def bids-1nt?
;;   [:and
;;    [:nt? :partner-suit]
;;    [:= :partner-level 1]])

;; (def nt-1-major-responses
;;   [:element :suit :biddable-major-5+])

;; (def cont-1nt
;;   [:and
;;    [:= :level 2]
;;    [:cond
;;     [:<= (gp) 3]        [:= :suit clubs]
;;     :biddable-major-5+ nt-1-major-responses
;;     :else              [:= :suit diamonds]]])

;; (def bids-2nt?
;;   [:and
;;    [:nt? :partner-suit]
;;    [:= :partner-level 2]])

;; (def cont-2nt
;;   [:and [:>= (gp) 3]
;;    [:cond
;;     [:>= [:longest-length :diamonds-or-higher] 5]
;;     [:bid-match? 3 [:highest-longest :diamonds-or-higher]]

;;     :else
;;     [:bid-match? 3 clubs]]])

;; (def responses
;;   [:and
;;    :partner-opened?
;;    [:cond
;;     bids-1suit? cont-1suit
;;     bids-1nt?   cont-1nt
;;     bids-2nt?   cont-2nt]])

;; (def bids-1nt-2c?
;;   [:bids-match?
;;    [:me 1 no-trump]
;;    [:partner 2 clubs]])

;; (def cont-1nt-2c
;;   [:cond
;;    [:>= (gp) 25]
;;    [:and [:= :level 2] [:= :suit diamonds]]

;;    [:>= [:length-in spades] 5]
;;    [:bid-match? 2 spades]

;;    [:>= [:length-in hearts] 5]
;;    [:bid-match? 2 hearts]

;;    [:>= [:length-in diamonds] 5]
;;    [:bid-match? 3 diamonds]

;;    [:>= [:length-in clubs] 5]
;;    [:bid-match? 3 clubs]

;;    [:and [:<= 23 (gp) 24] :nt-distribution?]
;;    [:bid-match? 2 no-trump]

;;    :else
;;    [:fail "Oups, I opened 1NT with 4441 :("]])

;; (def bids-1nt-2d?
;;   [:bids-match?
;;    [:me 1 no-trump]
;;    [:partner 2 diamonds]])

;; (def cont-1nt-2d
;;   [:cond
;;    [:>= [:longest-length :majors] 5]
;;    [:bid-match? 2 [:highest-longest :majors]]

;;    :nt-distribution?
;;    [:bid-match? 2 no-trump]

;;    [:>= [:longest-length :minors] 5]
;;    [:bid-match? 3 [:highest-longest :minors]]])

;; (def bids-1nt-2c-2d?
;;   [:bids-match?
;;    [:partner 1 no-trump]
;;    [:me 2 clubs]
;;    [:partner 2 diamonds]])

;; (def cont-1nt-2c-2d
;;   [:bid-match? 2 hearts])

;; (def bids-1nt-2d-2nt?
;;   [:bids-match?
;;    [:partner 1 no-trump]
;;    [:me 2 diamonds]
;;    [:partner 2 no-trump]])

;; (def cont-1nt-2d-2nt
;;   [:cond
;;    [:>= [:longest-length :diamonds-or-higher] 5]
;;    [:bid-match? 3 [:highest-longest :diamonds-or-higher]]

;;    :else
;;    [:bid-match? 3 clubs]])

;; (def bids-1nt-2d-2nt-3c?
;;   [:bids-match?
;;    [:me 1 no-trump]
;;    [:partner 2 diamonds]
;;    [:me 2 no-trump]
;;    [:partner 3 clubs]])

;; (def cont-1nt-2d-2nt-3c
;;   [:cond
;;    [:>= [:longest-length :diamonds-or-higher] 4]
;;    [:bid-match? 3 [:lowest-longest :diamonds-or-higher]]])

;; (def bids-1nt-2c-2nt?
;;   [:bids-match?
;;    [:partner 1 no-trump]
;;    [:me 2 clubs]
;;    [:partner 2 no-trump]])

;; (def cont-1nt-2c-2nt
;;   [:and [:>= (gp) 1]
;;    [:cond
;;     [:>= [:longest-length :diamonds-or-higher] 5]
;;     [:bid-match? 3 [:highest-longest :diamonds-or-higher]]

;;     :else
;;     [:bid-match? 3 clubs]]])

;; (def bids-1nt-2c-2nt-3c?
;;   [:bids-match?
;;    [:me 1 no-trump]
;;    [:partner 2 clubs]
;;    [:me 2 no-trump]
;;    [:partner 3 clubs]])

;; (def cont-1nt-2c-2nt-3c
;;   [:cond
;;    [:>= [:longest-length :diamonds-or-higher] 4]
;;    [:bid-match? 3 [:lowest-longest :diamonds-or-higher]]])


;; (def nt-1-continuation
;;   [:cond
;;    bids-1nt-2c?        cont-1nt-2c
;;    bids-1nt-2c-2d?     cont-1nt-2c-2d
;;    bids-1nt-2c-2nt?    cont-1nt-2c-2nt
;;    bids-1nt-2c-2nt-3c? cont-1nt-2c-2nt-3c
;;    bids-1nt-2d?        cont-1nt-2d
;;    bids-1nt-2d-2nt?    cont-1nt-2d-2nt
;;    bids-1nt-2d-2nt-3c? cont-1nt-2d-2nt-3c])

;; (def bids-2nt-3c?
;;   [:bids-match?
;;    [:me 2 no-trump]
;;    [:partner 3 clubs]])

;; (def cont-2nt-3c
;;   [:cond
;;    [:>= [:longest-length :diamonds-or-higher] 4]
;;    [:bid-match? 3 [:lowest-longest :diamonds-or-higher]]])

;; (def nt-2-continuation
;;   [:cond
;;    bids-2nt-3c? cont-2nt-3c])

;; (def rules
;;   [:or
;;    openings
;;    responses
;;    nt-1-continuation
;;    nt-2-continuation])

(defn system []
  (cond
    (stu/opening?) (openings)))
