(ns selfless
    (:use clojure.contrib.graph))
(use 'clojure.contrib.pprint)


; TODO: Eagerly-updating nodes. If a parent is changed, should recompute immediately
; TODO: if possible. Propagate value message to children if value changed, otherwise do
; TODO: nothing. Useful for 'index' nodes whose value won't usually change even if parents
; TODO: change.

; TODO: Handle errors in concurrent update.

(defn pre [x] (do (print x "\n") x))

(defn zipmapmap [fn coll] (zipmap coll (map fn coll)))

(defn agent? [x] (instance? clojure.lang.Agent x))

(defn deref-or-val [x] (if (agent? x) @x x))

(defn map-now [fn coll] 
    (doseq [x coll] (fn x)))

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
        
        ; ===================================
        ; = Destroying state after a change =
        ; ===================================
        (forget-children [state key]
            "Notifies the children of a key that it has changed. 
            If they are not eager, sets their values to nil, and 
            propagates the 'message' to their children.
            If they are eager, attempts to recompute their values
            before taking the above action."
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
    
        ; =====================
        ; = Sequential update =
        ; =====================
        (update-node [state key]
            "Updates the state with the value corresponding to key,
            and any ancestral values necessary to compute it."
            (if (state key) state
                (let [node (flow key)
                    parents (node-parents node)
                    new-state (reduce update-node state parents)]
                (assoc new-state key ((node-fn node) new-state)))))        
        
        (update-nodes [state & keys]
            "Evaluates the state at given keys. Propagates message of 
            recomputation to parents."
            (reduce update-node state keys))
        
        ; ======================
        ; = Concurrent updates =
        ; ======================
        (m-update [parent-vals state key new-pv collating-agent]
            "A message that a parent can send to a child when it has 
            updated."
            (let [new-vals (merge parent-vals new-pv)
                    node (flow key)]
                (if (= (count new-vals) (count (node-parents node))) 
                    ; If the value can be computed, do it.
                    (let [new-val ((node-fn node) new-vals)
                            msg-map {key new-val}] 
                        (do
                            (map-now #(send (state %) m-update state % msg-map collating-agent) (node-children node))
                            (send collating-agent m-record msg-map)
                            new-val))
                    ; Otherwise just record the parent's value.
                    new-vals)))
        
        (m-record [[state keys-remaining] new-vals]
            "A message agents send to the latch agent when they update.
            Its value consists of a [state keys-remaining] couple. When
            keys-remaining is zero, the requested update has been made."
            (let [new-state (merge state new-vals)
                new-keys (apply (partial disj keys-remaining) (keys new-vals))]
                [new-state new-keys]))
            
        (create-agent [state-and-roots key]
            "Creates an agent at the given key, which is responsible for 
            computing the value at that key. Creates agents for parents
            if necessary."
            (let [state (state-and-roots 0)
                    roots (state-and-roots 1)]
                (if (state key) [state roots]
                    (let [node (flow key)
                            parents (node-parents node)
                            parent-vals (select-keys state (filter (comp not agent? state) parents))
                            [state roots] (reduce create-agent [state roots] parents)]
                        ; Update the state with new agents at this node and at the parent nodes
                        [(assoc state key (agent parent-vals)) 
                        ; If this is a root node, add it to the roots.
                        (if (= (count parent-vals) (count parents)) (conj roots key) roots)]))))
            
        (concurrent-update [state & keys-to-update]
            "Does a concurrent update of the given keys. Returns three things: an agent
            whose state will eventually change to [requested state []], a fn to start
            the update, and the agent-filled state."
            (let [[new-state roots] (reduce create-agent [state []] keys-to-update)
                la (print roots "\n")
                    collating-agent (agent [state (set (filter (comp not state) (keys new-state)))])
                    start (fn [] (map-now #(send (new-state %) m-update new-state % {} collating-agent) roots))]
                    [collating-agent start new-state]))
        
        ] 
        {:update update-nodes :forget forget :change change :c-update concurrent-update}))
        


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
    "Binds the flow's 'methods' to vars."
    (let [sym-dash (.concat (name flow) "-")
        name-fn #(.concat sym-dash (name %))]
    (map (fn [[key val]] `(def ~(symbol (name-fn key)) ~val)) (meta (eval flow)))))
    
(defmacro with-flow [flow bindings & exprs] 
    "Like let-bindings, but provides update, forget and change
    functions in context of flow."
    `(let [~'update ((meta ~flow) :update)
            ~'forget ((meta ~flow) :forget)
            ~'change ((meta ~flow) :change)
            ~'c-update ((meta ~flow) :c-update)]
            (let ~bindings ~@exprs)))

(defn- pair-to-node [fl [sym body]]
    "Helper function for flow."
    (let [key (keyword (name sym))]        
        (if (empty? body)
            (add-root fl key)
        (let [f (eval (first body))
            args (rest body)
            is-block #(= % :block)
            block? (some is-block args)
            args (filter (comp not is-block) args)
            keys (keys fl)
            symbs (map (comp symbol name) keys)
            args (replace (zipmap symbs keys) args)] 
            (add-node fl key f block? args)))))

(defmacro flow [init-flow bindings]
    "Creates a flow with syntax similar to let-bindings."
    (let [pairs (partition 2 bindings)]
        (reduce pair-to-node init-flow pairs)))

(defmacro def-flow [sym init-flow bindings]
    "Creates a flow and binds it to a symbol. See also 'flow'."
    `(def ~sym (flow ~(eval init-flow) ~bindings)))
    
(defn flow-graph [dir flow]
    "Returns a clojure-contrib graph corresponding to the given flow."
    (struct directed-graph (keys flow) #(dir (flow %))))

(def forward-graph (partial flow-graph :children))
(def backward-graph (partial flow-graph :parents))