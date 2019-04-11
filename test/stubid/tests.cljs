(ns stubid.tests
  (:require [cljs.test :refer [run-tests]]
            [cljs-test-display.core :as test-display]
            [stubid.core-test]
            [stubid.schachcafe-test]))

(defn run []
  (run-tests
   (test-display/init! "app")
   'stubid.core-test
   'stubid.schachcafe-test))
