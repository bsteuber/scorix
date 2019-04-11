(ns stubid.ui
  (:require [reagent.core :as reagent]
            [scorix.core :as scorix]
            [scorix.ui :as scorix-ui]
            [stubid.core :as stu :refer [no-trump spades hearts diamonds clubs]]
            [stubid.schachcafe :as sc]))

(defonce state (reagent/atom {}))

(def player-key
  {0 :partner
   1 :left
   2 :right
   3 :me})

(def player-name
  {0 "N"
   1 "W"
   2 "E"
   3 "S"})

(defn current-dealer []
  (:dealer @state 3))

(defn player-hand [player]
  (let [dealer (current-dealer)]
    [:<>
     [:div.col-sm-4]
     [:div.col-sm-4
      [:h4.text-center (player-name player)]
      (for [suit (range 4)]
        (let [get-value #(get-in @state [:hands player suit])
              set-value #(swap! state assoc-in [:hands player suit] %)]
          ^{:key suit}
          [scorix-ui/suit-cards suit get-value set-value]))
      [:span.d-flex.justify-content-center.align-items-center
       [:div.form-group.form-check-inline
        {:on-click (fn [_]
                     (swap! state assoc :dealer player))}
        [:input
         {:type :radio
          :read-only true
          :checked (= dealer player)}]
        [:label.ml-1.mt-1 "Dealer"]]]]]))

(defn random-hands []
  (vec (scorix/random-deal)))

(defn bid-order []
  (case (current-dealer)
    0 [0 2 3 1]
    1 [1 0 2 3]
    2 [2 3 1 0]
    3 [3 1 0 2]))

(def next-player
  {0 2
   1 0
   2 3
   3 1})

(defn prev-bids-for-player [for-player prev-bids]
  (map (fn [bid-info]
         (when bid-info
           (let [{:keys [player bid]} bid-info
                 new-player (case (player-key for-player)
                              :partner (case player
                                         :partner :me
                                         :left    :right
                                         :right   :left
                                         :me      :partner)
                              :left (case player
                                      :partner :left
                                      :left    :me
                                      :right   :partner
                                      :me      :right)
                              :right (case player
                                       :partner :right
                                       :left    :partner
                                       :right   :me
                                       :me      :left)
                              :me player)]
             {:bid bid
              :player new-player})))
       prev-bids))

(defn calculate-bids []
  (loop [prev-bids []
         player    (current-dealer)]
    (if (and
         (>= (count prev-bids) 4)
         (->> prev-bids
              (take-last 3)
              (every? (fn [bid-info]
                        (not (:bid bid-info))))))
      prev-bids
      (let [hand (get-in @state [:hands player])
            prev (prev-bids-for-player player prev-bids)
            bids (sc/bids hand prev)]
        (recur (conj prev-bids {:player (player-key player)
                                :bid (first bids)})
               (next-player player))))))

(defn format-bid [[level suit]]
  [:span level
   [:span {:class (when (#{1 2} suit)
                    :text-danger)}
    (if (= suit stu/no-trump)
      "NT"
      (scorix-ui/suit-icon suit))]])

(defn bid-table-body []
  (let [bids (calculate-bids)
        parted (partition 4 4 [nil nil nil nil] bids)]
    [:tbody
     (->> parted
          (map-indexed (fn [index row]
                         ^{:key index}
                         [:tr
                          (->> row
                               (map-indexed
                                (fn [index bid-data]
                                  ^{:key index}
                                  [:td.text-center
                                   (when bid-data
                                     (let [{:keys [bid]} bid-data]
                                       (if bid
                                         [format-bid bid]
                                         "-")))]))
                               seq)]))
          seq)]))

(defn bids []
  (let [hand-counts   (->> @state
                           :hands
                           (map (fn [hand]
                                  (->> hand
                                       flatten
                                       (map count)
                                       (reduce +)))))
        all-hands-13? (every? #{13} hand-counts)]
    [:div
     [:h3.text-center "Bids"]
     (cond
       all-hands-13?
       [:table.table
        [:thead
         [:tr
          (for [player (bid-order)]
            ^{:key player}
            [:th.text-center (player-name player)])]]
        [bid-table-body]]

       :else
       [:div.alert.alert-info "Please enter 4 times 13 cards" [:br]
        [:ul
         (for [player (range 4)]
           ^{:key player}
           [:li (player-name player) ": " (nth hand-counts player)])]])]))

(defn page []
  [:div.container
   [:h2.text-center "Bidding Trainer"]
   [:button.btn.btn-link {:on-click (fn []
                                      (swap! state assoc :hands (random-hands)))}
    "Generate deal"]
   [:div.row
    [:div.col-lg-8>div.row
     (for [player (range 4)]
       ^{:key player}
       [player-hand player])]
    [:div.col-lg-4
     [bids]]]])
