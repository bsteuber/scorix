(ns scorix.describe-test
  (:require [scorix.describe :as describe]
            [cljs.test :refer-macros [deftest is]]))

(deftest format
  (is (= (describe/format [11 :high-card-points])
         "High card points"))
  (is (= (describe/format [1 :blank "A"])
         "Blank A")))
