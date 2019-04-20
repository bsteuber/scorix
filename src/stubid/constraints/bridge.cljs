(ns stubid.constraints.bridge)

(def all-suits (range 4))
(def all-players (range 4))

(def length-constraints
  [:and
   (for [suit all-suits]
     [:sum 13 (for [player all-players]
                [:suit-length player suit])])
   (for [player all-players]
     [:sum 13 (for [suit all-suits]
                [:suit-length player suit])])])
