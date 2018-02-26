(ns scorix.run-tests
  (:require [cljs.test :refer [run-all-tests]]
            [scorix.core-test]
            [scorix.describe-test]))

(defn ^:export run-tests []
  (run-all-tests #"scorix.*-test*"))

(enable-console-print!)
(set! *main-cli-fn* run-tests)
