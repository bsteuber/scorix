(ns stubid.core-test
  (:require-macros [cljs.test :refer [deftest is]]
                   [clojure.string :as str])
  (:require [stubid.core :as s]))

;; (deftest no-bids-yet
;;   (is (stu/no-bids-yet? nil))
;;   (is (stu/no-bids-yet? [{:player :partner}
;;                          {:player :right}]))
;;   (is (not (stu/no-bids-yet? [{:player :partner
;;                                :bid    [1 :nt]}
;;                               {:player :right}]))))

;; (deftest find-longest-suits
;;   (is (= (stu/find-longest-suits ["AKQJT" "975" "82" "976"])
;;          [[0] 5]))
;;   (is (= (stu/find-longest-suits ["AKQJ" "KT85" "842" "98"])
;;          [[0 1] 4]))
;;   (is (= (stu/find-longest-suits ["8" "AKQJT5" "" "975432"])
;;          [[1 3] 6])))

;; (deftest hcp
;;   (is (= (stu/suit-hcp "AKQJ") 10))
;;   (is (= (stu/suit-hcp "QJ962") 3)))

;; (deftest find-best-suits
;;   (is (= (stu/find-best-suits ["82" "AKQJT" "9" "97543"] [1 3])
;;          [1]))
;;   (is (= (stu/find-best-suits ["82" "AJT98" "9" "KQT32"] [1 3])
;;          [1 3])))

;; (deftest intervals
;;   (is (= (stu/interval-restrict [3 29] [5 35])
;;          [5 29]))
;;   (is (= (stu/interval+ [3 29] [5 35])
;;          [8 64]))
;;   (is (= (stu/interval- [15 25] [5 5])
;;          [10 20]))
;;   (is (= (stu/interval- [15 25] [0 0])
;;          [15 25]))
;;   (is (= (stu/interval- [15 15] [0 5])
;;          [10 15])))
