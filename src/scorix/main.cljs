(ns scorix.main
  (:require [reagent.dom :as rd]
            [scorix.ui :as ui]))

(enable-console-print!)

(defn ^:export run []
  (when-let [node (.getElementById js/document "container")]
    (rd/render [ui/page] node)))

(run)
