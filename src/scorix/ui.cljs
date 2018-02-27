(ns scorix.ui
  (:require [clojure.string :as str]
            [reagent.core :as reagent]
            [scorix.core :as scorix]
            [scorix.describe :as describe]))

(defn suit-icon [index]
  (get "♠♥♦♣" index))

(defonce state
  (reagent/atom {:hand ["" "" "" ""]
                 :infos [nil nil nil nil]
                 :eval-type :ground
                 :promised-trumps 3}))

(def info-text
  {nil "Not bid"
   :left "Bid by left"
   :partner "Bid by partner"
   :right "Bid by right"
   :trump "Trump"})

(defn suit-info-button [index info]
  ^{:key [index info]}
  [:button.btn.mt-2.mr-1
   {:class (if (= info
                  (get-in @state [:infos index]))
             :btn-primary
             :btn-outline-secondary)
    :type :button
    :on-click (fn [_]
                (swap! state assoc-in [:infos index] info))}
   (info-text info)])

(defn suit-input [index]
  ^{:key index}
  [:div.row
   [:div.col-lg-3
    [:div.input-group.mt-3.mt-lg-1
     [:div.input-group-prepend
      [:span.input-group-text
       {:class (if (#{0 3} index)
                 ""
                 "text-danger")
        :style {:font-size "20px"
                :width "1.6em"
                }}
       (suit-icon index)]]
     [:input.form-control
      {:on-change (fn [x]
                    (swap! state assoc-in [:hand index] x.target.value))
       :type :text
       :placeholder (get ["e.g. AKQxx"
                          "e.g. JT8x"
                          "e.g. xxx"
                          "e.g. x"]
                         index)
       :tab-index (inc index)
       :value (get-in @state [:hand index])}]]]
   [:div.col-lg-9
    (doall (map (partial suit-info-button index)
                [nil :left :partner :right :trump]))]
   ])

(defn suit-inputs []
  [:div.form-group
   (doall (map suit-input (range 4)))])

(defn eval-type-radio [eval-type]
  [:input.form-check-input.mb-1
   {:type :radio
    :checked (= (:eval-type @state)
                eval-type)
    :on-change identity}])

(defn radio-label [text]
  [:label.form-check-label text])

(defn eval-type-input []
  [:div.form
   [:div.form-group.mb-0.5>div.form-check-inline
    {:on-click (fn [x]
                 (swap! state assoc :eval-type :ground))}
    (eval-type-radio :ground)
    (radio-label "No contract yet")]
   [:div.form-group.mb-0.5>div.form-check-inline
    {:on-click (fn [x]
                 (swap! state assoc :eval-type :no-trump))}
    (eval-type-radio :no-trump)
    (radio-label "NT contract")]
   [:form.form-group>div.form-check-inline
    {:on-click (fn [x]
                 (swap! state assoc :eval-type :trump))}
    (eval-type-radio :trump)
    (radio-label "Suit contract with")
    [:input.form-control.ml-1.mr-1
     {:value (:promised-trumps @state)
      :style {:width "3em"}
      :on-change (fn [x]
                   (swap! state assoc :promised-trumps
                          x.target.value))}]
    (radio-label "promised trumps")]])

(defn check-13-cards []
  (let [card-count (->> @state
                        :hand
                        (apply concat)
                        count)]
    (cond
      (< card-count 13)
      [:div.alert.alert-primary
       (str "Please enter 13 cards - got " card-count)]
      (> card-count 13)
      [:div.alert.alert-warning
       (str "Please enter 13 cards - got " card-count)])))

(def legal-chars "AKQJT98765432x")

(def legal? (set legal-chars))

(defn check-card-chars []
  (let [first-illegal-char (->> @state
                                :hand
                                (apply concat)
                                (remove legal?)
                                first)]
    (when first-illegal-char
      [:div.alert.alert-danger
       (str "Illegal character: \"" first-illegal-char
            "\" - must be one of \"" legal-chars "\"")])))


(defn check-suit-sorted [suit]
  (when-not (empty? suit)
    (let [compressed-x (str/replace suit #"x+" "x")
          indices (map (partial str/index-of legal-chars) compressed-x)
          sorted? (apply < indices)
          message (str "Suit not sorted from highest to lowest: " suit)]
      (when-not sorted?
        [:div.alert.alert-danger message]))))

(defn check-hand-sorted []
  (some check-suit-sorted (:hand @state)))

(defn check-trump-suit []
  (let [trump-suits-count (->> @state
                               :infos
                               (filter #{:trump})
                               count)
        eval-type (:eval-type @state)
        message (cond
                  (and (pos? trump-suits-count)
                       (not= eval-type :trump))
                  "Please select \"Suit contract\" or remove trump suit"
                  (> trump-suits-count 1)
                  "Please select only one trump suit"
                  (and (zero? trump-suits-count)
                       (= eval-type :trump))
                  "Please select a trump suit")]
    (when message
      [:div.alert.alert-warning message])))

(defn check-inputs []
  (or (check-card-chars)
      (check-hand-sorted)
      (check-trump-suit)
      (check-13-cards)))

(defn tabular-line? [line]
  (re-find #"->" line))

(defn format-extra-info [lines]
  [:div.card.mt-2>div.card-body
   (->> lines
        (partition-by tabular-line?)
        (map (fn [grouped]
               (if (tabular-line? (first grouped))
                 [:div.row.mt-6
                  (for [line grouped]
                    (let [[rule points] (str/split line #"->")]
                      (list
                       [:div.col-6.mb-2 rule]
                       [:div.col-2 "→"]
                       [:div.col-4.col-sm-4
                        {:class (when (re-find #"^ *-" points)
                                  :text-danger)}
                        (str/replace points "^ *" (fn [spaces]
                                                    (->> "\u200c"
                                                         (repeat (count spaces))
                                                         (apply str))))])))]
                 (for [line grouped]
                   [:div line])))))])

(defn format-reason [[points reason :as fact]]
  (let [show? (reagent/atom false)]
    [(fn []
       [:div
         (describe/format fact)
         [:button.btn.btn-link
          {:on-click (fn [_]
                       (swap! show? not))}
          (if @show?
            "Hide"
            "Info")]
         (when @show?
           [format-extra-info (describe/extra-info reason)])])]))

(defn format-result [result]
  [:div.row>div.col-lg-9>table.table.table-striped
   [:col.w-75]
   [:col.w-25]
   [:thead>tr
    [:th "Total points"]
    [:th (scorix/score result)]]
   [:tbody
    (doall (map-indexed (fn [index [points :as fact]]
                          ^{:key index}
                          [:tr
                           [:td
                            [format-reason fact]]
                           [:td
                            {:class (when (neg? points)
                                      :text-danger)}
                            points]])
                        result))]])

(defn eval-result []
  (let [score-fn (case (:eval-type @state)
                   :ground scorix/ground-points
                   :no-trump scorix/no-trump-points
                   :trump #(scorix/trump-points %1 %2
                                                (:promised-trumps @state)))
        result (score-fn (:hand @state)
                         (:infos @state))]
    (format-result result)))

(defn eval-output []
  (let [error (check-inputs)]
    [:div
     error
     (when-not error
       (eval-result))]))

(defn page []
  [:div.container
   [:h1.display-4.mt-4 "Scorix Bridge Calculator"]
   [:button.btn.btn-link {:on-click (fn [_]
                                      (swap! state assoc :hand (scorix/random-hand)))}
    "Generate random hand"]
   [suit-inputs]
   [eval-type-input]
   [eval-output]])
