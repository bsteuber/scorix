(ns stubid.schachcafe-test
  (:require-macros [cljs.test :refer [deftest is]])
  (:require [stubid.core :as s]
            [stubid.schachcafe :as sc]))

;; (deftest pass-hands
;;   (is (= :pass (sc/make-bid ["82" "AJT98" "9" "KQT32"]))))

;; (deftest minor-5-opening
;;   (is (= (sc/make-bid ["86" "9" "AJ9865432" "5"])
;;          [5 diamonds]))
;;   (is (= (sc/make-bid ["86" "9" "5" "KT9865432"])
;;          [5 clubs])))

;; (deftest major-4-opening
;;   (is (= (sc/make-bid ["9" "AJ9865432" "86" "5"])
;;          [4 hearts]))
;;   (is (= (sc/make-bid ["KT986543" "863" "9" "5"])
;;          [4 spades])))

;; (deftest minor-4-opening
;;   (is (= (sc/make-bid ["86" "9" "AJ986543" "52"])
;;          [4 diamonds]))
;;   (is (= (sc/make-bid ["86" "Q9" "5" "KT965432"])
;;          [4 clubs])) )

;; (deftest weak-3-opening
;;   (is (= (sc/make-bid ["9" "AKJ9854" "8632" "5"])
;;          [3 hearts]))
;;   (is (= (sc/make-bid ["AKT9854" "863" "J9" "5"])
;;          [3 spades])))

;; (deftest weak-2-opening
;;   (is (= (sc/make-bid ["94" "AK9854" "8632" "5"])
;;          [2 hearts]))
;;   (is (= (sc/make-bid ["AKT985" "T863" "J9" "5"])
;;          [2 spades])))

;; (deftest suit-1-opening
;;   (is (= (sc/make-bid ["AJ943" "AK98" "862" "5"])
;;          [1 spades]))
;;   (is (= (sc/make-bid ["AJ94" "AK98" "863" "52"])
;;          [1 hearts]))
;;   (is (= (sc/make-bid ["AJ94" "AK98" "8632" "5"])
;;          [1 diamonds]))
;;   (is (= (sc/make-bid ["AJ94" "863" "AK98" "52"])
;;          [1 diamonds]))
;;   (is (= (sc/make-bid ["AK94" "863" "AJ98" "52"])
;;          [1 spades]))
;;   (is (= (sc/make-bid ["AK94" "863" "AK98" "52"])
;;          [1 diamonds])))

;; (deftest nt-2-opening
;;   (is (= (sc/make-bid ["AJ94" "AK98" "A86" "A1"])
;;          [2 no-trump])))

;; (deftest nt-1-opening
;;   (is (= (sc/make-bid ["AJ94" "AK98" "AK6" "A1"])
;;          [1 no-trump]))
;;   (is (= (sc/make-bid ["AJ94" "AK98" "AJ62" "A"])
;;          [1 no-trump])))

;; (deftest suit-1-responses
;;   (is (= (sc/make-bid ["AJ94" "J986" "876" "92"]
;;                   [{:player :partner
;;                     :bid [1 spades]}])
;;          [2 spades]))
;;   (is (= (sc/make-bid [ "876" "92" "AJ94" "J986"]
;;                   [{:player :partner
;;                     :bid [1 hearts]}])
;;          [1 no-trump]))
;;   (is (= (sc/make-bid [ "A76" "92" "AJ94" "J986"]
;;                   [{:player :partner
;;                     :bid [1 hearts]}])
;;          [2 clubs]
;;          [2 diamonds]))
;;   (is (= (sc/make-bid ["Jxxx" "ATxxx" "x" "K98"]
;;                   [{:player :partner
;;                     :bid [1 hearts]}])
;;          [3 hearts]))
;;   (is (= (sc/make-bid ["J9xx" "ATxx" "Ax" "Axx"]
;;                   [{:player :partner
;;                     :bid [1 hearts]}])
;;          [2 no-trump])))

;; (deftest nt-1-responses
;;   (is (= (sc/make-bid ["9654" "J986" "876" "92"]
;;                   [{:player :partner
;;                     :bid [1 no-trump]}])
;;          [2 clubs]))
;;   (is (= (sc/make-bid ["AJ654" "J986" "87" "92"]
;;                   [{:player :partner
;;                     :bid [1 no-trump]}])
;;          [2 spades]))
;;   (is (= (sc/make-bid ["J9863" "AJ654" "8" "92"]
;;                   [{:player :partner
;;                     :bid [1 no-trump]}])
;;          [2 hearts]))
;;   (is (= (sc/make-bid ["J9863" "J654" "A" "92"]
;;                   [{:player :partner
;;                     :bid [1 no-trump]}])
;;          [2 diamonds])))

;; (deftest after-1-nt-2-clubs
;;   (is (= (sc/make-bid ["AKQ863" "AKQ4" "A" "KQ"]
;;                   [{:player :me
;;                     :bid [1 no-trump]}
;;                    {:player :partner
;;                     :bid [2 clubs]}])
;;          [2 diamonds]))
;;   (is (= (sc/make-bid ["AKQ863" "AKQ4" "5" "63"]
;;                   [{:player :me
;;                     :bid [1 no-trump]}
;;                    {:player :partner
;;                     :bid [2 clubs]}])
;;          [2 spades]))
;;   (is (= (sc/make-bid ["AKQ8" "AKQ54" "53" "63"]
;;                   [{:player :me
;;                     :bid [1 no-trump]}
;;                    {:player :partner
;;                     :bid [2 clubs]}])
;;          [2 hearts]))
;;   (is (= (sc/make-bid ["AKQ8" "AKQ5" "Q63" "K3"]
;;                   [{:player :me
;;                     :bid [1 no-trump]}
;;                    {:player :partner
;;                     :bid [2 clubs]}])
;;          [2 no-trump])))
