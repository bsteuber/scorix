(ns scorix.describe-test
  (:require [scorix.describe :as describe]
            [cljs.test :refer-macros [deftest is]]))

(deftest format
  (is (= (describe/format [11 :high-card-points])
         "High card points"))
  (is (= (describe/format [1 :blank "A"])
         "Blank A"))
  (is (= (describe/format [0.5 :short-non-trump-opponent-suit 0 :left])
         "Void in opponent suit in suit contract"))
  (is (= (describe/format [0 :short-non-trump-opponent-suit 2 :right])
         "Doubleton in right opponent suit in suit contract")))
