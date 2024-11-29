# Happy Fruit Delivery System
 The Clojure-based optimal routes calculation solution, made by the fourth team during the Symbolic Computation class.
 
This program helps delivery trucks find the best routes to take. It makes sure each city gets the supplies they need, while making sure trucks don't carry more than they can handle. The code for this is split into three main parts, which I'll explain one by one.

# Development Workflow Challenges
We would like to highlight a significant limitation in our development process: our version
control and collaborative coding approach. Due to our team's initial mistake, we developed the
entire project locally without utilizing GitHub for version tracking during the development
phase. This is a critical area for improvement in future projects

# GitHub Repository Management
Our GitHub repository was only populated after the project was:

- 100% completed
- Thoroughly tested by all team members
- Verified to meet all initial project requirements

We understand that this approach is different from typical software development practices,
which focus on continuous integration, frequent updates, and shared version control. Our team
is committed to improving our workflow in future projects by using branch-based development.

# Development Communication
While we lack traditional GitHub pull request histories. The reason is - our team maintained
communication through:
- Regular standup meetings
- Detailed code review sessions
- Collaborative problem-solving discussions
- Shared documentation of design decisions and challenges

# Core Functions and Logic
## Distance cheker funtion
```clojure
(defn get-distance [node1 node2]
(or (some #(when (and (= (:node1 %) node1) (= (:node2 %) node2))
(:distance %)) roads)
(some #(when (and (= (:node1 %) node2) (= (:node2 %) node1))
(:distance %)) roads)))
```
- Calculates the distance between two cities based on the roads data
- Checks both directions (from -> to and to -> from) to ensure flexibility
- Returns the distance if a road connection exists, or nil if no connection found

## Status fucntions

```clojure
(defn was-visited [node nodes]
(map #(if (= (:name %) (:name node))
(Node. (:name %) true (:current %))
%)
nodes))
(defn needs-delivery? [node min-amount]
(and (< (:current node) min-amount) (not (@delivered-to (:name
node)))))
(defn can-take-from-city? [node amount min-amount]
(>= (- (:current node) amount) min-amount))
(defn calculate-needed-amount [node capacity min-amount]
(min (- min-amount (:current node)) capacity))
```
### was-visited:
- Marks a specific node as visited by updating its was_here flag
- Returns a modified list of nodes with the specified node marked
  
### needs-delivery?:
- Determines if a city requires a delivery:
- Checks if current stock is less than minimum required amount
- Ensures the city hasn't been added to the delivered-to set

### can-take-from-city?:
- Checks if a city can supply a certain amount of stock
- Ensures the city maintains its minimum stock level after supplying

### calculate-needed-amount:
- Computes how much stock a city needs
- Ensures the amount doesn't exceed the truck's capacity

## Delivery Planning and Execution

```clojure
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

(defn find-nearest-supplier [current-location target-node needed-amount nodes edges min-amount]
  (let [potential-suppliers (filter #(can-take-from-city? % needed-amount min-amount) nodes)]
    (when (seq potential-suppliers)
      (->> potential-suppliers
           (sort-by #(+ (or (get-distance (:name current-location) (:name %)) 999999)
                        (or (get-distance (:name %) (:name target-node)) 999999)))
           first))))

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

```
### traverse function
- Gets the starting node
- Checks if the end node is reached
- Marks the node as visited and get its children
- Recursively explore paths from the current node to the end node

### find-nearest-supplier function
- Filters potential suppliers
- Sorts suppliers based on distance
- Returns the nearest supplier
  
### plan-delivery-route Function
- Find cities needing delivery
- Determine the truck's current location
- If there are cities needing delivery:
   - Select the first city needing delivery as the target node
   - Calculate the needed amount
   - Find the nearest supplier for the needed amount
- If a supplier is found
   - Add the target city to the delivered-to set
   - Return the route details, including the truck ID, supplier, target node, amount, and total distance.

## Execution functions
```clojure
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
```
### generate-all-routes
- Resets the delivered-to set to start fresh
- Plans delivery routes for all trucks in the system
### -main Function:
- Generates delivery routes for all trucks
- Prints a detailed route plan for each truck, including:
- Truck ID
- Starting location
- Pickup details
- Delivery destination
- Total travel distance
