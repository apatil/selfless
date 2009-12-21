(ns selfless
    (:use clojure.contrib.graph clojure.set))

;;(set! *warn-on-reflection* true)

;; TODO: Why is this slower using pvmap than pmap?
; (defn vpvmap [f c] (pvmap f (vec c)))
;; (def vpvmap pmap)
(def vpvmap map)

(defn force-state [state] 
    "Forces evaluation of all the keys in the state."
    (zipmap (keys state) (vpvmap force (vals state))))

(defn zipmapmap [f coll] (zipmap coll (map f coll)))

(defn flosures [flow]
    "Produces fns for operating on the state of the given flow."

    (let [  allkeys (set (keys flow))
            parents (zipmapmap #(intersection (-> % flow :args set) allkeys) allkeys)
            children (reduce 
                        (fn [cm [c p]] (assoc cm p (conj (cm p) c)))
                        (zipmap allkeys (repeat #{}))
                        ;; Reduce over all the parent-child pairs in the flow
                        (for [c allkeys p (parents c)] [c p]))]
    
    (letfn [
        
        (timing [key] (-> key flow :timing))
        (obliv? [key] (= (timing key) :oblivious))
        (eager? [key] (=  (timing key) :eager))
        
        ;; ===================================
        ;; = Destroying state after a change =
        ;; ===================================
        
        ;; A version that checks for obliviousness & eagerness
        (forget-- [state key] (cond 
            ;; If oblivious, do not dissociate the key.
            (obliv? key) state
            ;; If eager, re-evaluate the key if possible. If it has 
            ;; not changed, don't forget it or its children.
            (eager? key) (if (= (state key) (maybe-compute-key state key)) 
                            state 
                            (forget- state key))
            ;; If lazy, forget the key and its children.
            true (forget- state key)))

        ;; A version that does NOT check. Always calls the version that checks.
        (forget- [state key] 
            ;; If this key is stil present in the state, dissociate it and forget 
            ;; its children.
            (if (state key)
                (reduce forget-- (dissoc state key) (children key))
                state))

        ;; A user-callable version. Will not check for obliviousness.
        (forget [state & keys]
            "User-callable fn used to dissoc certain keys from a state."
            (reduce forget- state keys))
        
        ;; Will not check for obliviousness.
        (change [state new-substate]
            "User-callable fn used to change a state's value at certain keys."
            (merge (reduce forget- state (keys new-substate)) new-substate))
                
        ;; =======================
        ;; = Creating new states =
        ;; =======================
        
        ;; Computes the value at the key.
        (compute-key [key state] 
            (let [parent-state (select-keys state (parents key))]
                (apply (:fn (flow key))
                     (replace 
                         (zipmap (keys parent-state) (vpvmap force (vals parent-state)))
                         (-> key flow :args)))))

        ;; Whether the state has all the parents of the key.
        (has-all-parents [key state]
            (let [p (parents key)]
                (= (count p) (count select-keys state p))))

        ;; Only computes the value at the key if all its parents are present.
        (maybe-compute-key [key state]
            (if (has-all-parents key state) (compute-key key state) nil))

        ;; Adds a key's value to the state, wrapped in a delay.
        (add-delay [state key] 
            (if (state key) state
                (let [state- (reduce add-delay state (parents key))]
                    (assoc state- key 
                        (if (eager? key)
                            (compute-key key state-)
                            (delay (compute-key key state-)))))))   
        
        ;; Adds all keys' values to the state, wrapped in delays.        
        (complete [state] (reduce add-delay state allkeys))
    
        ] 
        {:flow flow
        :new-state #(complete {})
        :forget (comp complete forget)
        :change (comp complete change)
        :init (comp complete (partial change {}))
        :obliv? obliv?
        :eager? eager?
        :parents parents
        :children children
        :allkeys allkeys})))

(defn assoc-node- [timing flow key fun & [args]]
    "Adds fun as a node to the dataflow flow, with key 'key'. 
    Labels must be unique within dataflows."
    (if (flow key) 
        (throw (Exception. (.concat "Node key already taken: " (name key))))
        (let [args (if args args [])]
        (assoc flow key {:fn fun :timing timing :args args}))))

(def assoc-node (partial assoc-node- :lazy))
(def assoc-oblivious (partial assoc-node- :oblivious))
(def assoc-eager (partial assoc-node- :eager))
(defn root-fn [] 
    (throw (Exception. (.concat "Root node uninitialized: " (name key)))))
(defn assoc-root [flow key] (assoc-node- :lazy flow key root-fn []))
    
(defmacro def-flosures [flow]
    "Binds the flow's 'methods' to vars."
    (let [sym-dash (.concat (name flow) "-")
        name-fn #(.concat sym-dash (name %))
        flosures (-> flow eval flosures)]
    (cons 'do (map (fn [[key val]] `(def ~(symbol (name-fn key)) ~val)) flosures))))

(defmacro with-flosures [flosures & exprs] 
    "Like let-bindings, but provides update, forget and change
    functions in context of flow."
    (let [f (eval flosures)
            v (vec (mapcat (fn [[key val]] [(symbol (name key)) (list flosures key)]) f))]
        `(let ~v ~@exprs)))
        
(defmacro with-flow [flow & exprs]
    "Like let-bindings, but provides update, forget and change
    functions in context of flow."
    `(with-flosures (flosures ~flow) ~@exprs))
                
(defn flow-graph [dir flow]
    "Returns a clojure-contrib graph corresponding to the given flow."
    (struct directed-graph (keys flow) #(dir (flow %))))

(def forward-graph (partial flow-graph :children))
(def backward-graph (partial flow-graph :parents))

(defn profile-flow [flow]
    "TODO: Find the average execution time of each function in the flow somehow.
    This information can be used to optimize eager tags, as well as parallelization.
    Figure out wall-clock time, CPU time, number of threads used etc."
    nil)