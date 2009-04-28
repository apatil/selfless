(defmacro structmap-and-accessors [sym & fields]
    "Defunes a structmap with given symbol, and defines accessors 
    for all its fields."
    (let [code-lst `(defstruct ~sym ~@fields)
            sym-dash (.concat (name sym) "-")
            accessor-names (zipmap fields (map (comp #(.concat sym-dash %) name) fields))]
        (cons 'do (cons code-lst
            (for [field fields] (let [n (accessor-names field) s (symbol n)]
                `(def ~s (accessor ~sym ~field))))))))
                
(structmap-and-accessors node :fn :parents :children :block?)

(defn add-node [flow key fun block? & [args]]
    "Adds fun as a node to the dataflow flow, with key 'key'. 
    Labels must be unique within dataflows."
    (if (flow key) 
        (throw (Exception. (.concat "Node key already taken: " (name key))))
        (let [
            ; Parents are nodes in the flow
            parents (set (filter (fn [key] (flow key)) args))   
            ; A partially-applied version of the function that takes nodes only
            pfun #(apply fun (replace % args))                  
            ; The flow, with the new function added
            new-flow (assoc flow key (struct node pfun parents #{} block?))
            ; A function adding key to the children list of a parent.
            add-child (fn [nf p] (assoc nf p (assoc (nf p) :children (conj (:children (nf p)) key))))] 
            ; Notify parents of new child
            (if (and parents (not block?))
                (reduce add-child new-flow parents)
                new-flow))))
                
(defn add-root [flow key] (add-node key #() flow true []))

(defn forget-children [flow state key]
    "Notifies the children of a key that it has changed. 
    Sets their values to nil, and propagates the 'message' 
    to their children."
    (let [children (node-children (flow key))]
        (apply dissoc (reduce (partial forget-children flow) state children) children)))

(defn change [flow state new-substate]
    "Sets the flow's value at certain keys to new values.
    Forgets children's values."
    (merge (reduce (partial forget-children flow) state (keys new-substate)) new-substate))
    
(defn forget [flow state keys]
    "Forgets the flow's value at given keys."
    (apply dissoc (reduce (partial forget-children flow) state keys) keys))

(defn parent-vals [parents state]
    "Accumulates the node's parents' values. Returns a map.
    The map may have nil's."
    (let [vals (map state parents)]
        (zipmap parents vals)))

(defn compute [node parent-vals]
    "Utility function for computing flow's state at key.
    Should only be called when all parents are ready."
    ((node-fn node) parent-vals))

(defn eval-node [flow state key]
    "Updates the state with the value corresponding to key,
    and any ancestral values necessary to compute it."
    (let [cur-val (state key)]
        (if cur-val cur-val
            (let [node (flow key)
                parents (node-parents node)
                n-parents (count parents)
                pval-map (parent-vals parents state)
                ; Reduce-fn reduces over the parent value map, updating the curent state as it goes.
                reduce-fn (if (> n-parents 0) 
                    (fn [s kv]
                        (let [key (kv 0) val (kv 1)]
                            (if val s (eval-node flow key s)))))
                new-state (if (= n-parents 0) state (reduce reduce-fn state pval-map))
                new-val (compute node (parent-vals parents new-state))]
            (assoc new-state key new-val)))))
    
(defn eval-nodes [flow state keys]
    "Evaluates the state at given keys. Propagates message of 
    recomputation to parents. Lazy by default; if value of any 
    key is not nil, it is left alone. If eager, values are 
    recomputed."
    (let [new-state (reduce (fn [state key] (eval-node flow state key)) state keys)]
        new-state))
        
;(defn concurrent-eval-state [flow state keys]
;    "Like eval-state, but updates are done concurrently when
;    possible.")
        
(def fn1 (fn [] 3))       
(defn fn2 [a b c d e] [a b c d e])     
(defn fn3 [fn2] (apply + fn2))
(def flow (add-node {} :fn1 fn1 false []))
(def flow2 (add-node flow :fn2 fn2 false [:fn1 17 :fn1 2 5]))
(def flow3 (add-node flow2 :fn3 fn3 false [:fn2]))
(def new-state (eval-nodes flow3 {} [:fn1 :fn2 :fn3]))