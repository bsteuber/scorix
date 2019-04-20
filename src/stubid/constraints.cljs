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

(defn build-rule [rule]
  (let [[key & args] (if (vector? rule)
                       rule
                       [rule])
        args (->> args
                  (mapcat (fn [arg]
                            (if (seq? arg)
                              arg
                              [arg]))))]
    (parse-rule key args)))

(defn constant? [x]
  (number? x))

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
      (cc))
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

(defn restrict-intervals [intervals restrictions]
  (try
    (->> restrictions
         (remove (fn [[k v]]
                   (constant? k)))
         (into {})
         (merge-with union intervals))
    (catch Unsolvable _
      nil)))

(defmethod parse-rule :else [_ _]
  {})

(defmethod parse-rule :not [_ [arg]]
  (let [rule (build-rule arg)]
    {:vars (:vars rule)
     :check (fn [solution cc]
              (when-not (check-rule rule solution (constantly true))
                (cc solution)))}))

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

(defmethod parse-rule :cond [_ args]
  (println "cond" [:or
    (->> args
         (map-indexed (fn [index rules]
                        [:and
                         (->> (take index args)
                              (map first)
                              (map (fn [x]
                                     [:not x])))
                         (seq rules)])))])
  (build-rule
   [:or
    (->> args
         (map-indexed (fn [index rules]
                        [:and
                         (->> (take index args)
                              (map first)
                              (map (fn [x]
                                     [:not x])))
                         (seq rules)])))]))

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

(defmethod parse-rule :<= [_ args]
  {:vars args
   :restrict
   (fn [intervals cc]
     (let [xs (interval-args intervals args)
           restrictions (->> args
                             (map-indexed (fn [index arg]
                                            (let [smaller (->> xs
                                                               (take (inc index))
                                                               (map :min)
                                                               (apply max))
                                                  larger (->> xs
                                                              (drop index)
                                                              (map :max)
                                                              (apply min))]
                                              [arg (interval smaller larger)])))
                             (into {}))
           new-intervals (restrict-intervals intervals restrictions)]
       (when new-intervals
         (cc new-intervals))))
   :check (fn [solution cc]
            (let [xs (solution-args solution args)]
              (when (apply <= xs)
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

(defn run-restrictions [intervals rule cc]
  (let [run-rec (fn run-rec [intervals cc]
                  (restrict-rule rule intervals
                                 (fn [new-intervals]
                                   (if (= intervals new-intervals)
                                     (cc intervals)
                                     (run-rec new-intervals cc)))))]
    (run-rec intervals cc)))

(defn rule-vars [rule]
  (cond
    (vector? rule) (->> rule
                        rest
                        (mapcat rule-vars)
                        distinct)
    (keyword? rule) [rule]))

(defn run-restrictor [rule-vector]
  (let [rule (build-rule rule-vector)
        intervals (zipmap (:vars rule) (repeat (interval)))]
    (run-restrictions intervals rule identity)))

(defn run-solver
  ([rule-vector]
   (run-solver rule-vector {}))
  ([rule-vector intervals]
   (let [rule (build-rule rule-vector)
         vars (->> (:vars rule)
                   (remove constant?)
                   distinct)
         intervals (merge (zipmap vars (repeat (interval)))
                          intervals)
         rec-solve (fn rec-solve [[next-var & more-vars] intervals cc]
                     (if next-var
                       (run-restrictions intervals rule
                                         (fn [intervals]
                                           (->> (get-interval intervals next-var)
                                                interval-values
                                                (some (fn [x]
                                                        (rec-solve more-vars
                                                                   (restrict-intervals
                                                                    intervals
                                                                    {next-var (interval x x)})
                                                                   cc))))))
                       (let [solution (->> intervals
                                           (map (fn [[k v]]
                                                  [k (:min v)]))
                                           (into {}))]
                         (check-rule rule solution cc))))]
     (rec-solve vars intervals identity))))
