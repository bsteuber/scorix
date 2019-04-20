(ns stubid.tests
  (:require [cljs.test :refer [run-tests]]
            [cljs-test-display.core :as test-display]
            [scorix.core-test]
            [stubid.core-test]
            [stubid.constraints-test]
            [stubid.schachcafe-test]))

(defn run []
  (js/console.clear)
  (run-tests
   (test-display/init! "app")
   ;; 'scorix.core-test
   ;; 'stubid.core-test
   ;; 'stubid.schachcafe-test
   'stubid.constraints-test))
