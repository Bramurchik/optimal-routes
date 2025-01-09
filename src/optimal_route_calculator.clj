(ns optimal-route-calculator)

; Data structures definition with new parameters
(defrecord Node [name was_here current min-capacity max-capacity])
(defrecord Edge [node1 node2 distance])



;list of cities as nodes
(def cities
  [(Node. "Budapest" false 250 100 250)
   (Node. "Vienna" false 50 70 100)
   (Node. "Prague" false 100 50 100)
   (Node. "Berlin" false 40 50 100)
   (Node. "Rome" false 150 100 250)])

;List of roads as edges
(def roads
  [(Edge. "Budapest" "Vienna" 200)
   (Edge. "Vienna" "Prague" 300)
   (Edge. "Prague" "Berlin" 400)
   (Edge. "Vienna" "Berlin" 250)
   (Edge. "Budapest" "Rome" 500)
   (Edge. "Rome" "Prague" 600)])


;Trucks data structure with needed parameters
(def trucks
  [{:id 1 :capacity 30 :location "Prague" :cargo 0}
   {:id 2 :capacity 30 :location "Rome" :cargo 0}
   {:id 3 :capacity 30 :location "Budapest" :cargo 0}])

;Global atom for storing info about cities, which was visited by truck already this day
(def delivered-to (atom #{}))


;; Auxiliary functions

;Function for finding the distance between two cities
(defn get-distance [node1 node2]
  (or (some #(when (and (= (:node1 %) node1) (= (:node2 %) node2)) (:distance %)) roads)
      (some #(when (and (= (:node1 %) node2) (= (:node2 %) node1)) (:distance %)) roads)))
;Function for finding a city by its name
(defn get-node-by-name [node-name nodes]
  (first (filter #(= (:name %) node-name) nodes)))
;Get all roads starting from a specific city.
(defn get-edges-for-node [node-name edges]
  (filter #(= (:node1 %) node-name) edges))
; Find all neighboring cities connected by roads.
(defn get-children-for-node [node-name nodes edges]
  (let [edges-for-node (get-edges-for-node node-name edges)]
    (map #(get-node-by-name (:node2 %) nodes) edges-for-node)))


;City marker as visited per truck ride
(defn was-visited [node nodes]
  (map #(if (= (:name %) (:name node))
          (Node. (:name %) true (:current %))
          %)
       nodes))
;Function for a check if a city needs more goods (below minimum supply and not yet delivered to).
(defn needs-delivery? [node min-amount]
  (and (< (:current node) min-amount) (not (@delivered-to (:name node)))))
; Check if a city can spare the required amount of goods.
(defn can-take-from-city? [node amount min-amount]
  (>= (- (:current node) amount) min-amount))
; Calculate how much goods are needed for a city.
(defn calculate-needed-amount [node capacity min-amount]
  (min (- min-amount (:current node)) capacity))


;; Core functions

;Function, that finds the optimal route between two cities(Nodes)
(defn traverse [start-node end-node structures roads path]
  (let [node (get-node-by-name start-node structures)]
    (if (= (:name node) end-node)
      (do
        (println "Path found:" (conj path end-node))
        (conj path end-node))
      (let [visited-structures (was-visited node structures)
            next-nodes (filter #(not (:was_here %))
                               (get-children-for-node start-node visited-structures roads))]
        (loop [paths (map #(traverse (:name %) end-node visited-structures roads (conj path start-node)) next-nodes)]
          (if (seq paths)
            (first (filter some? paths))
            "nil"))))))

;Finds the nearest city that can supply goods to another city.
(defn find-nearest-supplier [current-location target-node needed-amount nodes edges min-amount]
  (let [potential-suppliers (filter #(can-take-from-city? % needed-amount min-amount) nodes)]
    (when (seq potential-suppliers)
      (->> potential-suppliers
           (sort-by #(+ (or (get-distance (:name current-location) (:name %)) 999999)
                        (or (get-distance (:name %) (:name target-node)) 999999)))
           first))))

;Plans a single delivery route for a truck.
(defn plan-delivery-route [truck nodes edges min-amount]
  (let [cities-needing-delivery (filter #(needs-delivery? % min-amount) nodes)
        current-location (get-node-by-name (:location truck) nodes)]
    (when (seq cities-needing-delivery)
      (let [target-node (first cities-needing-delivery)
            needed-amount (calculate-needed-amount target-node (:capacity truck) min-amount)
            supplier (find-nearest-supplier current-location target-node needed-amount nodes edges min-amount)]
        (when supplier
          (swap! delivered-to conj (:name target-node))
          {:truck-id (:id truck)
           :from (:name supplier)
           :to (:name target-node)
           :amount needed-amount
           :total-distance (+ (or (get-distance (:name current-location) (:name supplier)) 0)
                              (or (get-distance (:name supplier) (:name target-node)) 0))})))))

;;Main functions

;Function, that generates paths for all trucks
(defn generate-all-routes [nodes edges min-amount]
  (reset! delivered-to #{})
  (keep #(plan-delivery-route % nodes edges min-amount) trucks))

(defn -main []
  (println "\nHappy Fruit Delivery Route Plan:")
  (let [routes (generate-all-routes cities roads 50)]
    (if (seq routes)
      (doseq [route routes]
        (println (str "\nTruck " (:truck-id route) " Route:"))
        (println (format "  Start from: %s" (get-in trucks [(dec (:truck-id route)) :location])))
        (println (format "  Pick up %d units from %s" (:amount route) (:from route)))
        (println (format "  Deliver to %s" (:to route)))
        (println (format "  Total distance: %d km" (:total-distance route))))
      (println "No deliveries needed at this time."))))

(println "Starting Happy Fruit Delivery System...")
(-main)
(println "Program completed.")
