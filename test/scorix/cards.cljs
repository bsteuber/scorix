(ns scorix.cards
  (:require-macros [devcards.core :refer [defcard-rg]])
  (:require [reagent.core :as reagent]
            [scorix.ui :as ui]))

(defcard-rg page
  [ui/page])

(defcard-rg suit-input
  [ui/suit-inputs])

(defcard-rg type-input
  [ui/eval-type-input])

(defcard-rg output
  [ui/eval-output])
