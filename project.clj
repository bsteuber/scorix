(defproject scorix "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.339"]
                 [devcards "0.2.5"]
                 [reagent "0.10.0"]
                 [cljsjs/react "16.11.0-0"]
                 [cljsjs/react-dom "16.11.0-0"]]
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.19"]]
  :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                    "target"]
  :cljsbuild
  {:builds
   [{:id "test"
     :source-paths ["src" "test"]
     :notify-command ["node"
                      "target/test.js"]
     :compiler     {:output-to     "target/test.js"
                    :output-dir    "target/test"
                    :main          scorix.run-tests
                    :optimizations :none
                    :target        :nodejs}}
    {:id "dev"
     :source-paths ["src"]
     :figwheel true
     :compiler {:main       "scorix.main"
                :asset-path "js/compiled/dev_out"
                :output-to  "resources/public/js/compiled/scorix.js"
                :output-dir "resources/public/js/compiled/dev_out"
                :source-map-timestamp true}}
    {:id "devcards"
     :source-paths ["src" "test"]
     :figwheel {:devcards true}
     :compiler {:main       "scorix.cards"
                :asset-path "js/compiled/devcards_out"
                :output-to  "resources/public/js/compiled/scorix_cards.js"
                :output-dir "resources/public/js/compiled/devcards_out"
                :source-map-timestamp true}}
    {:id "prod"
     :source-paths ["src"]
     :compiler {:main       "scorix.main"
                :asset-path "js/compiled/prod_out"
                :output-to  "resources/public/js/compiled/scorix.js"
                :output-dir "resources/public/js/compiled/prod_out"
                :optimizations :advanced}}]})
