(ns Shortest-route-function.shortest-route-calc

  (:require [clojure.set :as set]))

; Data structures definition with new parameters
(defrecord Node [name was_here current min-capacity max-capacity])
(defrecord Edge [node1 node2 distance])

; List of cities as nodes
(def cities
  [(Node. "Budapest" false 250 100 250)
   (Node. "Vienna" false 50 70 100)
   (Node. "Prague" false 100 50 100)
   (Node. "Berlin" false 40 50 100)
   (Node. "Rome" false 150 100 250)])

; List of roads as edges
(def roads
  [(Edge. "Budapest" "Vienna" 200)
   (Edge. "Vienna" "Prague" 300)
   (Edge. "Prague" "Berlin" 400)
   (Edge. "Vienna" "Berlin" 250)
   (Edge. "Budapest" "Rome" 500)
   (Edge. "Rome" "Prague" 600)])

; Trucks data structure with needed parameters
(def trucks
  [{:id 1 :capacity 30 :location "Prague" :cargo 0}
   {:id 2 :capacity 30 :location "Rome" :cargo 0}
   {:id 3 :capacity 30 :location "Budapest" :cargo 0}])

; Global atom for storing info about cities, which was visited by truck already this day
(def delivered-to (atom #{}))

;; Auxiliary functions

(defn get-distance [node1 node2]
  (or (some #(when (and (= (:node1 %) node1) (= (:node2 %) node2)) (:distance %)) roads)
      (some #(when (and (= (:node1 %) node2) (= (:node2 %) node1)) (:distance %)) roads)))

(defn get-node-by-name [node-name nodes]
  (first (filter #(= (:name %) node-name) nodes)))

(defn get-edges-for-node [node-name edges]
  (filter #(= (:node1 %) node-name) edges))

(defn get-children-for-node [node-name nodes edges]
  (let [edges-for-node (get-edges-for-node node-name edges)]
    (map #(get-node-by-name (:node2 %) nodes) edges-for-node)))

(defn was-visited [node nodes]
  (map #(if (= (:name %) (:name node))
          (Node. (:name %) true (:current %) (:min-capacity %) (:max-capacity %))
          %)
       nodes))

(defn needs-delivery? [node min-amount]
  (and (< (:current node) min-amount) (not (@delivered-to (:name node)))))

(defn can-take-from-city? [node amount min-amount]
  (>= (- (:current node) amount) min-amount))

(defn calculate-needed-amount [node capacity min-amount]
  (min (- min-amount (:current node)) capacity))

;; Core functions

(defn dijkstra-shortest-path [start-node end-node nodes edges]
  (let [unvisited (atom (set (map :name nodes)))
        distances (atom (zipmap (map :name nodes) (repeat ##Inf)))
        previous (atom {})]
    (swap! distances assoc start-node 0)
    (while (seq @unvisited)
      (let [current (apply min-key #(get @distances %) @unvisited)]
        (when (= current end-node)
          (loop [path [] node end-node]
            (if node
              (recur (conj path node) (get @previous node))
              (reverse path))))
        (swap! unvisited disj current)
        (doseq [neighbor (map :node2 (get-edges-for-node current edges))]
          (when (contains? @unvisited neighbor)
            (let [alt (+ (get @distances current)
                         (get-distance current neighbor))]
              (when (< alt (get @distances neighbor))
                (swap! distances assoc neighbor alt)
                (swap! previous assoc neighbor current)))))))))

(defn plan-delivery-route [truck nodes edges min-amount]
  (let [cities-needing-delivery (filter #(needs-delivery? % min-amount) nodes)
        current-location (get-node-by-name (:location truck) nodes)]
    (when (seq cities-needing-delivery)
      (let [target-node (first cities-needing-delivery)
            needed-amount (calculate-needed-amount target-node (:capacity truck) min-amount)
            path (dijkstra-shortest-path (:name current-location) (:name target-node) nodes edges)]
        (when path
          (swap! delivered-to conj (:name target-node))
          {:truck-id       (:id truck)
           :route          path
           :amount         needed-amount
           :total-distance (reduce + (map #(get-distance %1 %2) path (rest path)))})))))

(defn generate-all-routes [nodes edges min-amount days]
  (reset! delivered-to #{})
  (doseq [day (range 1 (inc days))]
    (println (str "\nTrading day " day ":"))
    (doseq [truck trucks]
      (if-let [route (plan-delivery-route truck nodes edges min-amount)]
        (do
          (println (str "Truck number " (:truck-id route) " for day " day ":"))
          (println "Route:" (clojure.string/join " -> " (:route route)))
          (println (str "Goods delivered: " (:amount route) " units"))
          (println (str "Total distance: " (:total-distance route) " km")))
        (println (str "Truck " (:id truck) " has no deliveries today."))))))

(defn -main []
  (println "\nHappy Fruit Delivery Route Plan:")
  (generate-all-routes cities roads 50 3)
  (println "\nProgram completed."))

(println "Starting Happy Fruit Delivery System...")
(-main)
(println "Program completed.")


