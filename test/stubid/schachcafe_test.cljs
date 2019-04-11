(ns stubid.schachcafe-test
  (:require-macros [cljs.test :refer [deftest is]])
  (:require [stubid.core :as stu :refer [spades hearts diamonds clubs no-trump]]
            [stubid.schachcafe :as sc]))

(deftest pass-hands
  (is (empty? (sc/bids ["82" "AJT98" "9" "KQT32"]))))

(deftest minor-5-opening
  (is (= (sc/bids ["86" "9" "AJ9865432" "5"])
         [[5 diamonds]]))
  (is (= (sc/bids ["86" "9" "5" "KT9865432"])
         [[5 clubs]])))

(deftest major-4-opening
  (is (= (sc/bids ["9" "AJ9865432" "86" "5"])
         [[4 hearts]]))
  (is (= (sc/bids ["KT986543" "863" "9" "5"])
         [[4 spades]])))

(deftest minor-4-opening
  (is (= (sc/bids ["86" "9" "AJ986543" "52"])
         [[4 diamonds]]))
  (is (= (sc/bids ["86" "Q9" "5" "KT965432"])
         [[4 clubs]])) )

(deftest weak-3-opening
  (is (= (sc/bids ["9" "AKJ9854" "8632" "5"])
         [[3 hearts]]))
  (is (= (sc/bids ["AKT9854" "863" "J9" "5"])
         [[3 spades]])))

(deftest weak-2-opening
  (is (= (sc/bids ["94" "AK9854" "8632" "5"])
         [[2 hearts]]))
  (is (= (sc/bids ["AKT985" "T863" "J9" "5"])
         [[2 spades]])))

(deftest suit-1-opening
  (is (= (sc/bids ["AJ943" "AK98" "862" "5"])
         [[1 spades]]))
  (is (= (sc/bids ["AJ94" "AK98" "863" "52"])
         [[1 hearts]]))
  (is (= (sc/bids ["AJ94" "AK98" "8632" "5"])
         [[1 diamonds]]))
  (is (= (sc/bids ["AJ94" "863" "AK98" "52"])
         [[1 diamonds]]))
  (is (= (sc/bids ["AK94" "863" "AJ98" "52"])
         [[1 spades]]))
  (is (= (sc/bids ["AK94" "863" "AK98" "52"])
         [[1 diamonds]
          [1 spades]])))

(deftest nt-2-opening
  (is (= (sc/bids ["AJ94" "AK98" "A86" "A1"])
         [[2 no-trump]])))

(deftest nt-1-opening
  (is (= (sc/bids ["AJ94" "AK98" "AK6" "A1"])
         [[1 no-trump]]))
  (is (= (sc/bids ["AJ94" "AK98" "AJ62" "A"])
         [[1 no-trump]])))

(deftest level-1-responses
  (is (= (sc/bids ["AJ94" "J986" "876" "92"] [{:player :partner
                                               :bid [1 spades]}])
         [[2 spades]])))