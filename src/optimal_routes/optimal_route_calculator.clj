(ns optimal_routes.optimal_route_calculator
  (:require [clojure.string :as str]))

;; -----------------------------
;; DATA STRUCTURES
;; -----------------------------
(defrecord Node [name was_here current min-capacity max-capacity])
(defrecord Edge [node1 node2 distance])

(def cities
  [(->Node "Budapest" false 250 100 250)
   (->Node "Vienna"   false  50  70 100)
   (->Node "Prague"   false 100  50 100)
   (->Node "Berlin"   false  40  50 100)
   (->Node "Rome"     false 150 100 250)])

(def roads
  [(->Edge "Budapest" "Vienna" 200)
   (->Edge "Vienna"   "Prague" 300)
   (->Edge "Prague"   "Berlin" 400)
   (->Edge "Vienna"   "Berlin" 250)
   (->Edge "Budapest" "Rome"   500)
   (->Edge "Rome"     "Prague" 600)])

;; Trucks data structure with needed parameters
(def trucks
  (atom
    {1 {:id 1 :capacity 30 :location "Prague"   :cargo 0}
     2 {:id 2 :capacity 30 :location "Rome"     :cargo 0}
     3 {:id 3 :capacity 30 :location "Budapest" :cargo 0}}))

;; Delivery requests data structure
(def daily-demands
  [{"Vienna" 10 "Berlin" 10}
   {"Prague" 15}
   {"Vienna" 35}])

;; Global atom for storing info about cities, which were visited by truck already this day
(def delivered-to (atom #{}))

;; -----------------------------
;; AUXILIARY FUNCTIONS
;; -----------------------------

(defn get-distance [node1 node2]
  (or (some #(when (and (= (:node1 %) node1)
                        (= (:node2 %) node2))
               (:distance %))
            roads)
      (some #(when (and (= (:node1 %) node2)
                        (= (:node2 %) node1))
               (:distance %))
            roads)))

;; Function, that list all neighboring nodes with current node
(defn get-neighbors [current-node edges]
  (->> edges
       (filter #(or (= (:node1 %) current-node)
                    (= (:node2 %) current-node)))
       (map #(if (= (:node1 %) current-node)
               (:node2 %)
               (:node1 %)))))

;; Dijkstra based shortest path finder algotithm
(defn dijkstra-shortest-path [start-node end-node nodes edges]
  (when (and (some #(= (:name %) start-node) nodes)
             (some #(= (:name %) end-node) nodes))
    (let [all-names   (map :name nodes)
          unvisited   (atom (set all-names))
          distances   (atom (zipmap all-names (repeat Double/POSITIVE_INFINITY)))
          previous    (atom {})]
      (swap! distances assoc start-node 0)
      (while (seq @unvisited)
        (let [current (apply min-key #(get @distances %) @unvisited)]
          (swap! unvisited disj current)
          (doseq [neighbor (get-neighbors current edges)]
            (when (contains? @unvisited neighbor)
              (let [alt (+ (get @distances current)
                           (get-distance current neighbor))]
                (when (< alt (get @distances neighbor))
                  (swap! distances assoc neighbor alt)
                  (swap! previous assoc neighbor current)))))))
      (if (Double/isInfinite (get @distances end-node))
        nil
        (loop [path []
               node end-node]
          (if node
            (recur (conj path node) (get @previous node))
            (reverse path)))))))

(defn path-distance [path]
  (if (nil? path)
    Double/POSITIVE_INFINITY
    (reduce + (map #(get-distance %1 %2) path (rest path)))))

;; Algorithm that choose the closest truck to the city
(defn best-truck-for-city [city-name trucks-map nodes edges]
  (->> trucks-map
       vals
       (map (fn [t]
              (let [route (dijkstra-shortest-path (:location t)
                                                  city-name
                                                  nodes
                                                  edges)
                    dist  (if route (path-distance route) Double/POSITIVE_INFINITY)]
                [t route dist])))
       (remove #(Double/isInfinite (nth % 2)))
       (apply (partial min-key #(nth % 2)))))

;; -----------------------------
;; BUSINESS LOGIC
;; -----------------------------
(defn deliver-for-all-days []
  (reset! delivered-to #{})
  (doseq [day-idx (range (count daily-demands))]
    (let [day-num (inc day-idx)
          demands (nth daily-demands day-idx)]
      (println (str "\n--- Trading Day " day-num " ---"))
      (if (empty? demands)
        (println "No deliveries for this day.")
        (doseq [[city need-amount] demands]
          (when (pos? need-amount)
            (loop [remaining need-amount]
              (when (pos? remaining)
                (let [[chosen-truck route total-dist]
                      (best-truck-for-city city @trucks cities roads)]
                  (if (nil? route)
                    (println "No route found for" city "!")
                    (let [truck-cap (:capacity chosen-truck)
                          deliver-now (min truck-cap remaining)]
                      (println (str "Truck " (:id chosen-truck)
                                    " delivers " deliver-now " units to " city))
                      (println (str "  Route: " (str/join " -> " route)))
                      (println (str "  Distance: " total-dist " km"))
                      (swap! trucks update (:id chosen-truck)
                             #(assoc % :location city))
                      (swap! delivered-to conj city)
                      (recur (- remaining deliver-now))))))))))))
  (println "\nDelivery plan completed for all days."))

;; -----------------------------
;; ENTRY POINT
;; -----------------------------
(defn -main []
  (println "Starting Happy Fruit Delivery System...\n")
  (deliver-for-all-days)
  (println "\nProgram completed."))

(-main)
