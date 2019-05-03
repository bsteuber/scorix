(ns stubid.constraints)

(defrecord Unsolvable [])

(defn fail []
  (throw (Unsolvable.)))

(defn interval
  ([]
   (interval nil nil))
  ([min]
   (interval min nil))
  ([min max]
   (let [min (or min 0)
         max (or max 40)]
     (when (> min max)
       (fail))
     {:min min
      :max max})))

(defn union [& xs]
  (interval (->> xs
             (filter some?)
             (map :min)
             (reduce max))
        (->> xs
             (filter some?)
             (map :max)
             (reduce min))))

(defn plus [& xs]
  (interval (->> xs
             (map :min)
             (reduce +))
        (->> xs
             (map :max)
             (reduce +))))

(defn minus [x & xs]
  (interval (->> xs
             (map :max)
             (reduce - (:min x)))
        (->> xs
             (map :min)
             (reduce - (:max x)))))

(defn interval-values [{:keys [min max]}]
  (range min (inc max)))

(defn remove-item [at-index coll]
  (->> coll
       (map-indexed (fn [index item]
                      (when-not (= at-index index)
                        item)))
       (filter some?)))

(defmulti parse-rule (fn [key args]
                       key))

(defn constant? [x]
  (number? x))

(defn normalize-rule [rule]
  (if (vector? rule)
    rule
    [rule]))

(defn build-rule [rule]
  (let [[key & args] (normalize-rule rule)
        args (->> args
                  (mapcat (fn [arg]
                            (if (seq? arg)
                              arg
                              [arg]))))
        result (parse-rule key args)]
    (assoc result
           :vars (remove constant? (:vars result))
           :rule rule)))

(defn restrict-intervals [intervals restrictions]
  (try
    (->> restrictions
         (remove (fn [[k v]]
                   (constant? k)))
         (into {})
         (merge-with union intervals))
    (catch Unsolvable _
      nil)))

(defn restrict-rule [rule intervals cc]
  (try
    (if-let [restrict-fn (:restrict rule)]
      (restrict-fn
       intervals
       (fn [restrictions]
         (when restrictions
           (when-let [new-intervals (restrict-intervals intervals
                                                        restrictions)]
             (cc new-intervals)))))
      (cc intervals))
    (catch Unsolvable _
      nil)))

(defn check-rule [rule solution cc]
  (try
    (if-let [check-fn (:check rule)]
      (check-fn solution cc)
      (cc solution))
    (catch Unsolvable _
      nil)))

(defn get-interval [intervals x]
  (if (constant? x)
    (interval x x)
    (intervals x)))

(defn get-solution [solution x]
  (if (constant? x)
    x
    (solution x)))

(defn interval-args [intervals args]
  (map (partial get-interval intervals)
       args))

(defn solution-args [solution args]
  (map (partial get-solution solution)
       args))

(defmethod parse-rule :else [_ _]
  {})

(defmethod parse-rule :simple-not [_ [arg]]
  (let [rule (build-rule arg)]
    {:vars (:vars rule)
     :check (fn [solution cc]
              (when-not (check-rule rule solution (constantly true))
                (cc solution)))}))

(defmulti negate-rule first)

(defmethod negate-rule :default [rule]
  [:simple-not rule])

(defmethod negate-rule :<= [[_ x y]]
  [:> x y])

(defmethod parse-rule :not [_ [arg]]
  (build-rule (negate-rule (normalize-rule arg))))

(defmethod parse-rule :or [_ args]
  (let [rules (map build-rule args)]
    {:vars (mapcat :vars rules)
     :restrict (fn [intervals cc]
                 (->> rules
                      (some (fn [rule]
                              (restrict-rule rule intervals cc)))))
     :check (fn [solution cc]
              (->> rules
                   (some (fn [rule]
                           (check-rule rule solution cc)))))}))

(defmethod parse-rule :and [_ args]
  (let [rules (map build-rule args)]
    {:vars (mapcat :vars rules)
     :restrict (fn [intervals cc]
                 (let [build (fn build [intervals [rule & more-rules] cc]
                               (if-not rule
                                 (cc intervals)
                                 (restrict-rule rule intervals
                                                (fn [intervals]
                                                  (build intervals more-rules cc)))))]
                   (build intervals rules cc)))
     :check (fn [solution cc]
              (let [build (fn build [[rule & more-rules] cc]
                            (if-not rule
                              (cc solution)
                              (check-rule rule solution
                                          (fn []
                                            (build more-rules cc)))))]
                (build rules cc)))}))

(defmethod parse-rule :cond [_ conditionals]
  (let [rule [:or
              (->> conditionals
                   (map-indexed (fn [index [condition then-rule]]
                                  [:and
                                   (->> (take index conditionals)
                                        (map first)
                                        (map (fn [x]
                                               [:not x])))
                                   condition
                                   then-rule])))]]
    (build-rule rule)))

(defmethod parse-rule := [_ args]
  {:vars args
   :restrict (fn [intervals cc]
               (when-let [result (apply union (interval-args intervals args))]
                 (when-let [new-intervals (restrict-intervals
                                           intervals
                                           (zipmap args (repeat result)))]
                   (cc new-intervals))))
   :check (fn [solution cc]
            (when (->> (solution-args solution args)
                       (apply =))
              (cc solution)))})

(defmethod parse-rule :<= [_ [x-arg y-arg :as args]]
  {:vars args
   :restrict
   (fn [intervals cc]
     (let [[x y] (interval-args intervals args)
           restrictions {x-arg (interval nil (:max y))
                         y-arg (interval (:min x) nil)}
           new-intervals (restrict-intervals intervals restrictions)]
       (when new-intervals
         (cc new-intervals))))
   :check (fn [solution cc]
            (let [[x y] (solution-args solution args)]
              (when (<= x y)
                (cc solution))))})

(defmethod parse-rule :>= [_ [x-arg y-arg :as args]]
  {:vars args
   :restrict
   (fn [intervals cc]
     (let [[x y] (interval-args intervals args)
           restrictions {x-arg (interval (:min y) nil)
                         y-arg (interval nil (:max x))}
           new-intervals (restrict-intervals intervals restrictions)]
       (when new-intervals
         (cc new-intervals))))
   :check (fn [solution cc]
            (let [[x y] (solution-args solution args)]
              (when (>= x y)
                (cc solution))))})

(defmethod parse-rule :< [_ [x-arg y-arg :as args]]
  {:vars args
   :restrict
   (fn [intervals cc]
     (let [[x y] (interval-args intervals args)
           restrictions {x-arg (interval nil (dec (:max y)))
                         y-arg (interval (inc (:min x)) nil)}
           new-intervals (restrict-intervals intervals restrictions)]
       (when new-intervals
         (cc new-intervals))))
   :check (fn [solution cc]
            (let [[x y] (solution-args solution args)]
              (when (< x y)
                (cc solution))))})

(defmethod parse-rule :> [_ [x-arg y-arg :as args]]
  {:vars args
   :restrict
   (fn [intervals cc]
     (let [[x y] (interval-args intervals args)
           restrictions {x-arg (interval (inc (:min y)) nil)
                         y-arg (interval nil (dec (:max x)))}
           new-intervals (restrict-intervals intervals restrictions)]
       (when new-intervals
         (cc new-intervals))))
   :check (fn [solution cc]
            (let [[x y] (solution-args solution args)]
              (when (> x y)
                (cc solution))))})

(defmethod parse-rule :sum [_ args]
  {:vars args
   :restrict
   (fn [intervals cc]
     (let [vars (remove constant? args)
           [sum & xs] (interval-args intervals args)
           new-sum (apply plus xs)
           new-xs (->> xs
                       (map-indexed (fn [index _]
                                      (->> xs
                                           (remove-item index)
                                           (apply minus sum)))))
           new-intervals (restrict-intervals intervals (zipmap args (cons new-sum new-xs)))]
       (when new-intervals
         (cc new-intervals))))
   :check
   (fn [solution cc]
     (let [[sum & xs] (solution-args solution args)]
       (when (= sum (apply + xs))
         (cc solution))))})

(defmethod parse-rule :bid [_ [level suit]]
  (build-rule
   [:and
    [:= :bid-level level]
    [:= :bid-suit suit]]))

(defmethod parse-rule :pass [_ _]
  (build-rule
   [:and
    [:= :bid-level 0]
    [:= :bid-suit 0]]))

(defn run-restrictions [orig-intervals rule cc]
  (let [run-rec (fn run-rec [intervals cc]
                  (restrict-rule rule intervals
                                 (fn [new-intervals]
                                   (if (= intervals new-intervals)
                                     (cc intervals)
                                     (run-rec new-intervals cc)))))]
    (run-rec orig-intervals cc)))

(defn rule-vars [rule]
  (cond
    (vector? rule) (->> rule
                        rest
                        (mapcat rule-vars)
                        distinct)
    (keyword? rule) [rule]))

(defn run-restrictor [rule-vector]
  (let [rule (build-rule rule-vector)
        vars (->> rule
                  :vars
                  (remove constant?))
        intervals (zipmap vars (repeat (interval)))]
    (run-restrictions intervals rule identity)))

(def max-tries 20)

(def current-tries (atom 0))

(defn run-solver
  ([rule-vector]
   (run-solver rule-vector {}))
  ([rule-vector intervals]
   (reset! current-tries 0)
   (let [rule (build-rule rule-vector)
         vars (->> (:vars rule)
                   (remove constant?)
                   distinct)
         intervals (merge (zipmap vars (repeat (interval)))
                          intervals)
         rec-solve (fn rec-solve [[next-var & more-vars] intervals cc]
                     (if next-var
                       (run-restrictions
                        intervals
                        rule
                        (fn [intervals]
                          (let [possible-values (interval-values
                                                 (get-interval intervals next-var))]
                            (if (> (count possible-values) 1)
                              (some (fn [x]
                                      (when (> @current-tries max-tries)
                                        (throw "Max tries exceeded"))
                                      (swap! current-tries inc)
                                      (rec-solve more-vars
                                                 (restrict-intervals
                                                  intervals
                                                  {next-var (interval x x)})
                                                 cc))
                                    possible-values)
                              (rec-solve more-vars
                                         intervals
                                         cc)))))
                       (let [solution (->> intervals
                                           (map (fn [[k v]]
                                                  [k (:min v)]))
                                           (into {}))
                             check-res (check-rule rule solution cc)]
                         check-res)))]
     (rec-solve vars intervals identity))))
