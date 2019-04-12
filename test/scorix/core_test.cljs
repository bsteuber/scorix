(ns scorix.core-test
  (:require [cljs.test :refer [deftest is]]
            [scorix.core :as scorix]))

(deftest basic
  (is (= (scorix/basic-points
          ["xxxxx" "xxxx" "xxx" "x"])
         [[0 :high-card-points]
          [-0.5 :A-T-count 0]]))
  (is (= (scorix/basic-points
          ["AT9xx" "xxxx" "xxx" "x"])
         [[4 :high-card-points]]))
  (is (= (scorix/basic-points
          ["A" "xxxx" "xxxx" "xxxx"])
         [[4 :high-card-points]
          [-0.25 :A-T-count 1]
          [-1 :blank "A"]]))
  (is (= (scorix/basic-points
          ["QJ" "xxxx" "xxxx" "xxx"])
         [[3 :high-card-points]
          [-0.5 :A-T-count 0]
          [-1 :blank "QJ"]]))
  (is (= (scorix/basic-points
          ["T9" "xxxx" "xxxx" "xxx"])
         [[0 :high-card-points]
          [-0.25 :A-T-count 1]
          [-0.25 :blank "T9"]]))
  (is (= (scorix/basic-points
          ["AKxxx" "Qxxx" "AJ" "JT"])
         [[15 :high-card-points]
          [0.25 :A-T-count 3]
          [-0.5 :blank "AJ"]
          [-0.5 :blank "JT"]])))

(deftest reasonable-suit
  (is (not (scorix/reasonable-suit? "xxxx")))
  (is (not (scorix/reasonable-suit? "KTxxx")))
  (is (scorix/reasonable-suit? "Axxx"))
  (is (scorix/reasonable-suit? "KJxx"))
  (is (scorix/reasonable-suit? "QJTx")))

(deftest strong-suit
  (is (not (scorix/strong-suit? "xxxx")))
  (is (not (scorix/strong-suit? "KTxxx")))
  (is (scorix/strong-suit? "AKJxx"))
  (is (scorix/strong-suit? "AKQTxx"))
  (is (scorix/strong-suit? "KQJTxx")))

(deftest ground-length
  (is (= (scorix/ground-length-points "xxxxx" nil)
         [[0.5 :length "xxxxx"]]))
  (is (= (scorix/ground-length-points "KTxxx" nil)
         [[0.5 :length "KTxxx"]]))
  (is (= (scorix/ground-length-points "Axxx" nil)
         [[0.5 :length "Axxx"]]))
  (is (= (scorix/ground-length-points "AKQJ" nil)
         [[0.5 :length "AKQJ"]]))
  (is (= (scorix/ground-length-points "Kxxxxx" nil)
         [[1 :length "Kxxxxx"]]))
  (is (= (scorix/ground-length-points "xxxxxx" nil)
         [[1 :length "xxxxxx"]]))
  (is (= (scorix/ground-length-points "Axxxx" nil)
         [[1 :length "Axxxx"]]))
  (is (= (scorix/ground-length-points "KJxxx" nil)
         [[1 :length "KJxxx"]]))
  (is (= (scorix/ground-length-points "xxxxxxx" nil)
         [[2 :length "xxxxxxx"]]))
  (is (= (scorix/ground-length-points "Axxxxxx" nil)
         [[2 :length "Axxxxxx"]]))
  (is (= (scorix/ground-length-points "Axxxxx" nil)
         [[2 :length "Axxxxx"]]))
  (is (= (scorix/ground-length-points "QJTxxx" nil)
         [[2 :length "QJTxxx"]]))
  (is (= (scorix/ground-length-points "AKJxx" nil)
         [[1.5 :length "AKJxx"]]))
  (is (= (scorix/ground-length-points "KQJTx" nil)
         [[1.5 :length "KQJTx"]]))
  (is (= (scorix/ground-length-points "KQJTxx" nil)
         [[3 :length "KQJTxx"]]))
  (is (= (scorix/ground-length-points "AKQJTxx" nil)
         [[3 :length "AKQJTxx"]]))
  (is (empty? (scorix/ground-length-points "xxxx" nil))))

(deftest ground-honors
  (is (= (scorix/ground-AQJ-points ["AQJTx" "AQJ" "xxxx" "x"])
         [[0.5 :AQJ]]))
  (is (= (scorix/ground-AQJ-points ["AQJ" "xxxxx" "xxxxx" ""])
         [[0.25 :AQJ]])))

(deftest gap-free-length
  (is (nil? (scorix/gap-free-length "")))
  (is (nil? (scorix/gap-free-length "A")))
  (is (= (scorix/gap-free-length "AKQJT98")
         7))
  (is (= (scorix/gap-free-length "KQT9875")
         4))
  (is (= (scorix/gap-free-length "AQJT9")
         4)))

(deftest ground-partner-suit
  (is (empty? (scorix/ground-partner-suit-points "xx" :partner)))
  (is (= (scorix/ground-partner-suit-points "" :partner)
         [[-2 :short-in-partner-suit ""]]))
  (is (= (scorix/ground-partner-suit-points "x" :partner)
         [[-1 :short-in-partner-suit "x"]]))
  (is (= (scorix/ground-partner-suit-points "J" :partner)
         [[0.5 :partner-suit-honors 1]
          [-1 :short-in-partner-suit "J"]]))
  (is (= (scorix/ground-partner-suit-points "Ax" :partner)
         [[0.5 :partner-suit-honors 1]]))
  (is (= (scorix/ground-partner-suit-points "AKJ" :partner)
         [[1 :partner-suit-honors 3]])))

(deftest ground-right-opponent-suit
  (is (empty? (scorix/ground-right-opponent-suit-points "" :right)))
  (is (= (scorix/ground-right-opponent-suit-points "Kxx" :right)
         [[0.5 :gap-in-right-suit "Kx"]]))
  (is (= (scorix/ground-right-opponent-suit-points "KQ9" :right)
         [[1 :gap-in-right-suit "KQ9"]]))
  (is (= (scorix/ground-right-opponent-suit-points "KQxx" :right)
         [[1 :gap-in-right-suit "KQx"]
          [-0.5 :length-in-opponent-suit "KQxx"]]))
  (is (= (scorix/ground-right-opponent-suit-points "AKxxxx" :right)
         [[-1 :length-in-opponent-suit "AKxxxx"]]))
  (is (empty? (scorix/ground-right-opponent-suit-points "AKQJT9xx" :right)))
  (is (= (scorix/ground-right-opponent-suit-points "xxxxxx" :right)
         [[-1 :length-in-opponent-suit "xxxxxx"]]))
  (is (empty? (scorix/ground-right-opponent-suit-points "98765432" :right))))

(deftest ground-left-opponent-suit
  (is (empty? (scorix/ground-left-opponent-suit-points "" :left)))
  (is (= (scorix/ground-left-opponent-suit-points "Kxx" :left)
         [[-0.5 :gap-in-left-suit "Kx"]]))
  (is (= (scorix/ground-left-opponent-suit-points "KQx" :left)
         [[-1 :gap-in-left-suit "KQx"]]))
  (is (= (scorix/ground-left-opponent-suit-points "KQxx" :left)
         [[-1 :gap-in-left-suit "KQx"]
          [-0.5 :length-in-opponent-suit "KQxx"]]))
  (is (= (scorix/ground-left-opponent-suit-points "Axxx" :left)
         [[-0.5 :length-in-opponent-suit "Axxx"]]))
  (is (= (scorix/ground-left-opponent-suit-points "AKQxx" :left)
         [[-1 :length-in-opponent-suit "AKQxx"]])))

(deftest ground
  (is (= (scorix/ground-points
          ["Axx" "Kxx" "xxxx" "xxx"]
          [ nil   nil   nil    nil])
         [[7 :high-card-points]
          [-0.25 :A-T-count 1]]))
  (is (= (scorix/ground-points
          ["Axxx" "Kxx" "xxxx" "xx"]
          [ nil    nil   nil    nil])
         [[7 :high-card-points]
          [-0.25 :A-T-count 1]
          [0.5 :length "Axxx"]]))
  (is (= (scorix/ground-points
          ["Axxx" "Kxx" "xxxx" "xx"]
          [ nil   :left  nil    nil])
         [[7 :high-card-points]
          [-0.25 :A-T-count 1]
          [0.5 :length "Axxx"]
          [-0.5 :gap-in-left-suit "Kx"]]))
  (is (= (scorix/ground-points
          ["Axxx" "Kxx"    "xxxx" "xx"]
          [ nil   :partner  nil    nil])
         [[7 :high-card-points]
          [-0.25 :A-T-count 1]
          [0.5 :length "Axxx"]
          [0.5 :partner-suit-honors 1]]))
  (is (= (scorix/ground-points
          ["Axxx" "Kxx"  "xxxx" "xx"]
          [ nil   :right  nil    nil])
         [[7 :high-card-points]
          [-0.25 :A-T-count 1]
          [0.5 :length "Axxx"]
          [0.5 :gap-in-right-suit "Kx"]]))
  (is (= (scorix/ground-points
          ["AK"       "x" "KQTxx" "AJ9xx"]
          [:partner   nil  nil     nil])
         [[17 :high-card-points]
          [0.25 :A-T-count 3]
          [-0.5 :blank "AK"]
          [1 :length "KQTxx"]
          [1 :length "AJ9xx"]
          [1 :partner-suit-honors 2]])))

(deftest no-trump
  (is (= (scorix/no-trump-points
          ["ATxx" "QJT8x" "9xx" "xx"]
          [ nil    nil     nil   nil])
         [[7 :high-card-points]
          [0.25 :A-T-count 3]
          [0.5 :length "ATxx"]
          [1 :length "QJT8x"]
          [0.25 :T-9-count-in-NT 3]]))
  (is (= (scorix/no-trump-points
          ["ATx"  "QJT9x" "JTxx"  "xx"]
          [:right :left   :right  :partner])
         [[8 :high-card-points]
          [0.5 :A-T-count 4]
          [1 :length "QJT9x"]
          [-0.5 :gap-in-left-suit "QJT"]
          [-0.5 :length-in-opponent-suit "QJT9x"]
          [0.5 :T-9-count-in-NT 4]
          [0.5 :QJT9-or-QJT9-in-opponent-suit-in-NT "QJT9"]
          [0.25 :JTxx-or-J9xx-in-right-opponent-suit-in-NT "JTxx"]]))
  (is (= (scorix/no-trump-points
          ["K"      "QT"     "QJ98xx"  "AQ8x"]
          [:partner :partner  nil       nil])
         [[14 :high-card-points]
          [-1 :blank "K"]
          [-0.5 :blank "QT"]
          [1 :length "QJ98xx"]
          [0.5 :length "AQ8x"]
          [0.5 :partner-suit-honors 1]
          [-1 :short-in-partner-suit "K"]
          [0.5 :partner-suit-honors 1]]))
  (is (= (scorix/no-trump-points
          ["AJxx"     "AKJx"  "Kx"     "xxx"]
          [ nil        nil    :partner  nil])
         [[16 :high-card-points]
          [0.5 :length "AJxx"]
          [0.5 :length "AKJx"]
          [0.5 :partner-suit-honors 1]
          [-0.5 :T-9-count-in-NT 0]])))

(deftest trump-points
  (is (= (scorix/trump-points
          ["ATxxxx" "xxx" "xx"   "xx"]
          [:trump    nil  :left :right]
          4)
         [[4 :high-card-points]
          [-1.5 :trump-basic-correction]
          [1 :trump-suit-length 6]
          [2 :more-or-less-trumps-than-promised 4 6]
          [2.25 :high-trump-cards]
          [1 :short-non-trump-suits]
          [0.5 :short-non-trump-opponent-suit 2 :left]
          [0 :short-non-trump-opponent-suit 2 :right]]))
  (is (= (scorix/trump-points
          ["AKQxxx" "KTx" "xxxx"  ""]
          [:trump    nil   nil   :left]
          4)
         [[12 :high-card-points]
          [-1.5 :trump-basic-correction]
          [1 :trump-suit-length 6]
          [2 :more-or-less-trumps-than-promised 4 6]
          [2.5 :high-trump-cards]
          [0 :honors-in-non-trump-suits]
          [3 :short-non-trump-suits]
          [0.5 :short-non-trump-opponent-suit 0 :left]])))

(deftest random-hand
  (let [hand (scorix/random-hand)]
    (is (vector? hand))
    (is (= (count hand)
           4))
    (is (= (count (apply str hand))
           13))))

(deftest pseudo-random-hand
  (let [[hand remaining] (scorix/pseudo-random-hand scorix/full-deck {:hcp-range [0 40]})]
    (is hand)
    (is (= 13 (count (apply str hand))))
    (is (= 39 (count remaining)))
    (is (<= 0 (scorix/calc-high-card-points hand) 40)))
  (let [[hand remaining] (scorix/pseudo-random-hand scorix/full-deck {:hcp-range [25 25]})]
    (is hand)
    (is (= 13 (count (apply str hand))))
    (is (= 39 (count remaining)))
    (is (<= 25 (scorix/calc-high-card-points hand) 25))))
