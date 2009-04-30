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

(defn flosures [flow]
    "Produces fns for operating on the state of the given flow."

    (letfn [
        (forget-children [state key]
        "Notifies the children of a key that it has changed. 
        Sets their values to nil, and propagates the 'message' 
        to their children."
        (let [node (flow key)
            children (if node (node-children node) [])]
            (apply dissoc (reduce forget-children state children) children)))

        (change [state new-substate]
            "Sets the flow's value at certain keys to new values.
            Forgets children's values."
            (merge (reduce forget-children state (keys new-substate)) new-substate))

        (forget [state & keys]
            "Forgets the flow's value at given keys."
            (apply dissoc (reduce forget-children state keys) keys))
    
        (update-node [state key]
            "Updates the state with the value corresponding to key,
            and any ancestral values necessary to compute it."
            (if (state key) state
                (let [node (flow key)
                    parents (node-parents node)
                    new-state (reduce update-node state (node-parents node))]
                (assoc new-state key ((node-fn node) new-state)))))        
        
        (update-nodes [state & keys]
            "Evaluates the state at given keys. Propagates message of 
            recomputation to parents. Lazy by default; if value of any 
            key is not nil, it is left alone. If eager, values are 
            recomputed."
            (reduce update-node state keys))    
        ] {:update update-nodes :forget forget :change change}))

(defn update-flow-meta [flow]
    "Called automatically when flow is changed using add-node etc.
    Appends closure functions useful for altering states to flow."
    (with-meta flow (flosures flow)))

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
            (update-flow-meta
                (if (and parents (not block?))
                    (reduce add-child new-flow parents)
                    new-flow)))))
                
(defn add-root [flow key] 
    "Adds a root (parentless) node."
    (add-node flow key (fn []) true))
        
(defmacro def-flosures [flow]
    "Defines a structmap with given symbol, and defines accessors 
    for all its fields."
    (let [sym-dash (.concat (name flow) "-")
        name-fn #(.concat sym-dash (name %))]
    (map (fn [[key val]] `(def ~(symbol (name-fn key)) ~val)) (meta (eval flow)))))
    
(defmacro with-flow [flow bindings & exprs] 
    "Like let-bindings, but provides update, forget and change
    functions in context of flow."
    `(let [~'update ((meta ~flow) :update)
            ~'forget ((meta ~flow) :forget)
            ~'change ((meta ~flow) :change)]
            (let ~bindings ~@exprs)))

        
        
(defn fn2 [a b c d e] [a b c d e])     
(defn fn3 [fn2] (apply + fn2))

(def flow (add-root {} :fn1))
(def flow2 (add-node flow :fn2 fn2 false [:fn1 17 :fn1 2 5]))
(def flow3 (add-node flow2 :fn3 fn3 false [:fn2]))

(def-flosures flow3)

(def init-state (flow3-change {} {:fn1 3}))
(def new-state (flow3-update init-state :fn3 :fn1 :fn2))

(with-flow flow3
    [init-state (change {} {:fn1 3})
    new-state (update init-state :fn3 :fn1 :fn2)
    spotty-state (forget new-state :fn3)]
    [new-state spotty-state])