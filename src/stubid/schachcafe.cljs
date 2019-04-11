(ns stubid.schachcafe
  (:require [stubid.core :as stu :refer [spades hearts diamonds clubs no-trump]]))

(def weak-minor-5
  [:and
   [:>= :length 9]
   :minor
   [:<= :gcp 14]])

(def weak-major-4
  [:and
   [:>= :length 8]
   :major
   [:<= :gcp 14]])

(def weak-minor-4
  [:and
   [:= :length 8]
   :minor
   [:<= :gcp 12]])

(def weak-3
  [:and
   [:= :length 7]
   [:<= 8 :gcp 12]])

(def weak-2
  [:and
   [:= :length 6]
   [:<= 8 :gcp 12]])

(def suit-1
  [:<= 13 :gcp 20])

(def suit-1-openings
  [:cond
   [:>= :length 5]     :highest-suit-bid
   :distribution-4441 :lowest-best-minor
   :majors-44         [:= :suit hearts]
   :else              :best-longest-suit])

(def suit-openings
  [:and
   :longest-suit-bid
   [:cond
    weak-minor-5 [:= :level 5]
    weak-major-4 [:= :level 4]
    weak-minor-4 [:= :level 4]
    weak-3       [:= :level 3]
    weak-2       [:= :level 2]
    suit-1       [:and [:= :level 1] suit-1-openings]]])

(def nt-2
  [:and
   [:<= 21 :gcp 22]
   :nt-distribution])

(def nt-1
  [:>= :gcp 21])

(def nt-openings
  [:cond
   nt-2 [:= :level 2]
   nt-1 [:= :level 1]])

(def openings
  [:and
   :first-bid
   [:cond
    :suit-bid suit-openings
    :nt-bid   nt-openings]])

(def opener-2-response
  [:and
   [:<= 5 [:yp 4 :?partner-suit] 9]
   [:>= [:length :?partner-suit] 4]])

(def responses
  [:and
   :undisturbed
   #_[:partner-opened :?partner-suit]
   #_[:cond
    opener-2-response [:and [:suit :?partner-suit][:level 2]]]])

(def rules
  [:or
   openings
  #_responses])

(defn bid-valid? [hand bid]
  (stu/apply-rule rules hand bid []))

(defn bids
  ([hand]
   (bids hand []))
  ([hand prev-bids]
   (stu/possible-bids rules hand prev-bids)))
