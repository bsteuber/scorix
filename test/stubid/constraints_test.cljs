(ns stubid.constraints-test
  (:require-macros [cljs.test :refer [deftest is]]
                   [clojure.string :as str])
  (:require [stubid.constraints :as c]
            [stubid.constraints.bridge :as bridge]))

(deftest equal
  (let [solution (c/run-solver [:= :x 23])]
    (is (= solution {:x 23}))))

(deftest sum
  (let [solution (c/run-solver [:and
                                [:sum 40 :x :y]
                                [:= :x :y]])]
    (is (= solution {:x 20
                     :y 20}))))

(deftest lengths
  (is (c/run-restrictor [:and
                         [:<= 6 [:suit-length 0 0]]
                         [:<= 7 [:suit-length 0 1]]
                         bridge/length-constraints]))

  (is (not
       (c/run-restrictor [:and
                          [:<= 7 [:suit-length 0 0]]
                          [:<= 7 [:suit-length 0 1]]
                          bridge/length-constraints])))

  (is (c/run-restrictor [:and
                         [:<= 7 [:suit-length 0 0]]
                         [:or
                          [:<= 7 [:suit-length 0 1]]
                          [:= 6 [:suit-length 0 1]]]
                         bridge/length-constraints])))

(deftest conditional
  (is (= (c/run-restrictor
          [:and
           [:= :y 4]
           [:cond
            [[:<= :x 5]
             [:= :y 5]]
            [:else
             [:= :y 4]]]])
         {:x (c/interval 6 40)
          :y (c/interval 4 4)})))
