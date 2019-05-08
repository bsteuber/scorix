(ns stubid.constraints-test
  (:require-macros [cljs.test :refer [deftest is]]
                   [clojure.string :as str])
  (:require [stubid.constraints :as c]
            [stubid.core :as stu]))

(deftest negations
  (is (= (c/run-restrictor [:not [:not [:= :x 1]]])
         {:x (c/interval 1 1)}))
  (is (= (c/run-restrictor [:and
                            [:not [:and
                                   [:< :x 4]
                                   [:>= :y 12]]]
                            [:= :x 3]])
         {:x (c/interval 3 3)
          :y (c/interval 0 11)})))

(deftest and-complexity
  (is (= (c/run-solver [:= :x 1])
         {:x 1}))
  (is (= (c/current-steps)
         2))
  (is (c/run-solver [:and
                     [:= :x 1]
                     [:= :y 2]]))
  (is (= (c/current-steps) 4))
  (is (= (c/run-solver [:and
                        [:= :x 1]
                        [:= :y 1]
                        [:= :z 1]])
         {:x 1
          :y 1
          :z 1}))
  (is (= (c/current-steps) 6))
  (is (= (c/run-solver [:and
                        [:= :x 1]
                        [:= :y 1]
                        [:= :z 1]
                        [:= :a 1]
                        [:= :b 1]])
         {:x 1
          :y 1
          :z 1
          :a 1
          :b 1}))
  (is (= (c/current-steps) 10)))

(deftest or-complexity
  (is (= (c/run-solver [:and
                        [:= :x 5]
                        [:or
                         [:= :x 1]
                         [:= :y 2]]])
         {:x 5
          :y 2}))
  (is (= (c/current-steps) 4))
  (is (= (c/run-solver [:and
                        [:= :x 5]
                        [:= :y 4]
                        [:or
                         [:= :x 1]
                         [:= :y 2]
                         [:= :z 6]]])
         {:x 5
          :y 4
          :z 6}))
  (is (= (c/current-steps) 6))
  (is (= (c/run-solver [:and
                        [:or
                         [:< :x 5]
                         [:> :y 4]]
                        [:or
                         [:> :x 5]
                         [:= :y 2]]
                        [:= :x 7]
                        [:= :y 7]])
         {:x 7
          :y 7}))
  (is (= (c/current-steps) 10))
  (is (= (c/run-solver [:and
                        [:or
                         [:< :x 5]
                         [:> :y 4]]
                        [:or
                         [:< :x 5]
                         [:= :y 7]]
                        [:or
                         [:< :x 5]
                         [:> :a 4]]
                        [:or
                         [:< :x 5]
                         [:= :z :a]]
                        [:= :x 7]
                        [:= :y 7]
                        [:= :z 7]
                        [:= :a 7]])
         {:x 7
          :y 7
          :z 7
          :a 7}))
  (is (= (c/current-steps) 24)))

(deftest cond-complexity
  (is (= (c/run-solver
          [:and
           [:= :x 4]
           [:cond
            [[:= :y 1]
             [:= :x 4]]
            [:else
             [:= :x 9]]]])
         {:x 4
          :y 1}))
  (is (= (c/current-steps)
         6))

  (is (= (c/run-solver
          [:and
           [:= :x 4]
           [:cond
            [[:> :y 3]
             [:= :x 6]]
            [:else
             [:>= :y 2]]]])
         {:x 4
          :y 2}))
  (is (= (c/current-steps)
         13))

  (is (= (c/run-solver
          [:and
           [:= :x 4]
           [:cond
            [[:> :y 3]
             [:= :x 6]]
            [[:< :y 2]
             [:= :x 5]]
            [:else
             [:= :x 4]]]])
         {:x 4
          :y 2}))
  (is (= (c/current-steps)
         22))

  (is (= (c/run-solver
          [:and
           [:= :x 4]
           [:cond
            [[:> :y 3]
             [:= :x 6]]
            [[:< :y 1]
             [:= :x 5]]
            [[:= :y 1]
             [:= :x 19]]
            [:else
             [:= :x 4]]]])
         {:x 4
          :y 2}))
  (is (= (c/current-steps)
         41)))


  (deftest equal
    (let [solution (c/run-solver [:= :x 23])]
      (is (= solution {:x 23}))))

  (deftest sum
    (let [solution (c/run-solver [:and
                                  [:sum 40 :x :y]
                                  [:= :x :y]])]
      (is (= solution {:x 20
                       :y 20}))
      (is (< (c/current-steps) 77))))

  (deftest lengths
    (is (c/run-restrictor [:and
                           [:<= 6 [:suit-length 0 0]]
                           [:<= 7 [:suit-length 0 1]]
                           stu/length-constraints]))
    (is (c/run-solver [:and
                       [:<= 6 [:suit-length 0 0]]
                       [:<= 7 [:suit-length 0 1]]
                       stu/length-constraints]))
    (is (not
         (c/run-restrictor [:and
                            [:<= 7 [:suit-length 0 0]]
                            [:<= 7 [:suit-length 0 1]]
                            stu/length-constraints])))

    (is (c/run-restrictor [:and
                           [:<= 7 [:suit-length 0 0]]
                           [:or
                            [:<= 7 [:suit-length 0 1]]
                            [:= 6 [:suit-length 0 1]]]
                           stu/length-constraints])))

(deftest conditional
  (let [rule [:and
              [:= :y 4]
              [:cond
               [[:<= :x 5] [:= :y 5]]
               [:else      [:= :y 4]]]]]
    (is (= (c/run-restrictor rule)
           {:x (c/interval 6 40)
            :y (c/interval 4 4)}))
    (is (= (c/run-solver rule)
           {:x 6
            :y 4}))))

(defn run-checker [rule solution]
  (c/check-rule (c/build-rule rule) solution (constantly true)))

(deftest checkers
  (is (run-checker :else {}))
  (is (run-checker [:not
                    [:<= :x :y]]
                   {:x 3
                    :y 2}))
  (let [or-rule [:or
                 [:= :x 1]
                 [:= :x 2]]]
    (is (run-checker or-rule {:x 1}))
    (is (run-checker or-rule {:x 2}))
    (is (not (run-checker or-rule {:x 3}))))
  (let [and-rule [:and
                  [:= :x 1]
                  [:= :y 2]]]
    (is (run-checker and-rule {:x 1 :y 2}))
    (is (not (run-checker and-rule {:x 1 :y 3})))
    (is (not (run-checker and-rule {:x 2 :y 2}))))
  (let [cond-rule [:cond
                   [[:<= :x 5]
                    [:= :y 10]]
                   [:else
                    [:= :y 20]]]]
    (is (run-checker cond-rule {:x 5 :y 10}))
    (is (not (run-checker cond-rule {:x 5 :y 20})))
    (is (not (run-checker cond-rule {:x 6 :y 10})))
    (is (run-checker cond-rule {:x 6 :y 20})))
  (is (run-checker [:>= :x 10] {:x 10}))
  (is (run-checker [:>= :x 10] {:x 20}))
  (is (not (run-checker [:>= :x 10] {:x 9})))
  (is (run-checker [:> :x 10] {:x 11}))
  (is (run-checker [:> :x 10] {:x 20}))
  (is (not (run-checker [:> :x 10] {:x 10})))
  (let [sum-rule [:sum 10 :x :y]]
    (is (run-checker sum-rule {:x 1 :y 9}))
    (is (run-checker sum-rule {:x 7 :y 3}))
    (is (not (run-checker sum-rule {:x 7 :y 2})))
    (is (not (run-checker sum-rule {:x 7 :y 4})))))
