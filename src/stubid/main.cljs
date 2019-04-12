(ns stubid.main
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [stubid.ui :as ui]))

(enable-console-print!)

(defn ^:export run []
  (when-let [node (.getElementById js/document "container")]
    (r/render [ui/page] node)))

(defn ^:export reload []
  (println "reloaded")
  (js/console.clear)
  (rf/clear-subscription-cache!)
  (run))
