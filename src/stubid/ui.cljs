(ns stubid.ui
  (:require [re-frame.core :as rf]
            [scorix.core :as scorix]
            [scorix.ui :as scorix-ui]
            [stubid.core :as stu :refer [no-trump spades hearts diamonds clubs]]
            [stubid.schachcafe :as sc]))

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

(rf/reg-sub :current-dealer
            (fn [db _]
              (:dealer db 3)))

(rf/reg-sub :get-player-hand
            (fn [db [_ player]]
              (get-in db [:hands player])))

(rf/reg-sub :get-player-suit
            (fn [db [_ player suit]]
              (get-in db [:hands player suit])))

(rf/reg-event-db :set-player-suit
                 (fn [db [_ player suit cards]]
                   (assoc-in db [:hands player suit] cards)))

(rf/reg-event-db :set-dealer
                 (fn [db [_ dealer]]
                   (assoc db :dealer dealer)))

(defn player-hand [player]
  (let [dealer @(rf/subscribe [:current-dealer])]
    [:<>
     [:div.col-sm-4]
     [:div.col-sm-4
      [:h4.text-center (player-name player)]
      (for [suit (range 4)]
        (let [get-value (fn []
                          @(rf/subscribe [:get-player-suit player suit]))
              set-value (fn [value]
                          (rf/dispatch [:set-player-suit player suit value]))]
          ^{:key suit}
          [scorix-ui/suit-cards suit get-value set-value]))
      [:span.d-flex.justify-content-center.align-items-center
       [:div.form-group.form-check-inline
        {:on-click (fn []
                     (rf/dispatch [:set-dealer player]))}
        [:input
         {:type :radio
          :read-only true
          :checked (= dealer player)}]
        [:label.ml-1.mt-1 "Dealer"]]]]]))

(rf/reg-sub :config
            (fn [db _]
              (:config db)))

(defn random-hands []
  (let [{:keys [min-hcp-n
                min-hcp-s
                min-hcp-ns]
         :or   {min-hcp-n  0
                min-hcp-s  0
                min-hcp-ns 0}
         :as config} @(rf/subscribe [:config])]
    (loop [tries 0]
      (if (> tries 10000)
        (js/alert "Couldn't find valid deal")
        (let [deal   (scorix/random-deal)
              hcp-n  (scorix/calc-high-card-points (nth deal 0))
              hcp-s  (scorix/calc-high-card-points (nth deal 3))
              hcp-ns (+ hcp-n hcp-s)]
          (if (and
               (or (not min-hcp-n)
                   (>= hcp-n min-hcp-n))
               (or (not min-hcp-s)
                   (>= hcp-s min-hcp-s))
               (or (not min-hcp-ns)
                   (>= hcp-ns min-hcp-ns)))
            (vec deal)
            (recur (inc tries))))))))

(defn bid-order []
  (case @(rf/subscribe [:current-dealer])
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
  (binding [stu/*debug?* false]
    (loop [prev-bids []
           player    @(rf/subscribe [:current-dealer])]
      (if (and
           (>= (count prev-bids) 4)
           (->> prev-bids
                (take-last 3)
                (every? (fn [bid-info]
                          (not (:bid bid-info))))))
        prev-bids
        (let [hand @(rf/subscribe [:get-player-hand player])
              prev (prev-bids-for-player player prev-bids)
              bids (sc/bids hand prev)]
          (recur (conj prev-bids {:player (player-key player)
                                  :bid (first bids)})
                 (next-player player)))))))

(defn format-bid [[level suit]]
  [:span {:style {:font-size :x-large}}
   level
   (if (= suit stu/no-trump)
     "NT"
     [:span {:style {:font-size :xx-large}
             :class (when (#{1 2} suit)
                      :text-danger)}
      (scorix-ui/suit-icon suit)])])

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
                                  [:td.text-center.align-middle
                                   (when bid-data
                                     (let [{:keys [bid]} bid-data]
                                       (if bid
                                         [format-bid bid]
                                         "-")))]))
                               seq)]))
          seq)]))

(rf/reg-sub :hands
            (fn [db _]
              (:hands db)))

(defn bids []
  (let [hands         @(rf/subscribe [:hands])
        hand-counts   (->> hands
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
            [:th.text-center {:style {:width "25%"}}
             (player-name player)])]]
        [bid-table-body]]

       :else
       [:div.alert.alert-info "Please enter 4 times 13 cards" [:br]
        [:ul
         (for [player (range 4)]
           ^{:key player}
           [:li (player-name player) ": " (nth hand-counts player)])]])]))

(rf/reg-event-db
 :set-config
 (fn [db [_ key value]]
   (assoc-in db [:config key] (let [parsed (js/parseInt value)]
                                (when (js/Number.isInteger parsed)
                                  parsed)))))

(defn text-editor [label key]
  [:span.row
   [:span.col>label.mr-3 label]
   [:span.col>input.form-control
    {:value (key @(rf/subscribe [:config]))
     :on-change (fn [evt]
                  (rf/dispatch [:set-config key evt.target.value]))}]])

(rf/reg-event-db :generate-hand
                 (fn [db _]
                   (assoc db :hands (random-hands))))

(defn deal-generator []
  [:div
   [text-editor "Min HCP N/S" :min-hcp-ns]
   [text-editor "Min HCP N"   :min-hcp-n]
   [text-editor "Min HCP S"   :min-hcp-s]
   [:button.btn.btn-link {:on-click (fn [] (rf/dispatch [:generate-hand]))}
    "Generate deal"]])

(defn page []
  [:div.container
   [:h2.text-center "Bidding Trainer"]
   [:div.row
    [:div.col-lg-7>div.row
     (for [player (range 4)]
       ^{:key player}
       [player-hand player])]
    [:div.col-lg-1]
    [:div.col-lg-4
     [deal-generator]
     [:div.mt-4
      [bids]]]]])
