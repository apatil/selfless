(ns selfless
    (:use clojure.contrib.graph clojure.contrib.monads))

(set! *warn-on-reflection* true)

; TODO: create-agent should add watchers to parents. That way you can remove some update code
; TODO: and you don't have to rely on knowing who the children are when updating.

; TODO: Don't use the state monad in with-flow. You want to make the state explicit at all times.
; TODO: You don't need to worry about preferred errors at this level. ZeroProbabilities 
; TODO: will always happen at leaf nodes, so the logp-accessor fn can deal with them.


(defn zipmapmap [fn coll] (zipmap coll (map fn coll)))
(defn map-now [fn coll] (doseq [x coll] (fn x)))
(defn has-keys? [m k] (every? identity (map (partial contains? m) k)))
(defn agent? [x] (instance? clojure.lang.Agent x))

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
            (if (state key)
                (let [node (flow key)
                    children (:children node)]
                        ; If the node is eagerly-updating, give it a chance right now.
                    (apply forget state children))
                state))

        (change [state new-substate]
            "Sets the flow's value at certain keys to new values.
            Forgets children's values."
            (merge (reduce forget-children state (keys new-substate)) new-substate))
            
        (eager-update [[state keys] key]
            "Intended to be reduced over [state keys] pair. At end of
            reduce, state will be updated with values of all eager nodes
            that were ready to compute, and keys will contain keys of all 
            nodes that did not get updated."
            (let [node (flow key)
                eager? (= (:timing node) :eager)]
                (if eager?
                    (if (has-keys? state (:parents node))
                        [(assoc state key ((:fn node) state)) keys]
                        [state (conj keys key)])
                    [state (conj keys key)])))

        (forget [state & keys]
            "Forgets the flow's value at given keys."
            (let [[new-state new-keys] (reduce eager-update [state []] keys)]
                (apply dissoc (reduce forget-children new-state new-keys) new-keys)))
    
        ; =====================
        ; = Sequential update =
        ; =====================
        (update-node [state key]
            "Updates the state with the value corresponding to key,
            and any ancestral values necessary to compute it."
            (if (state key) state
                (let [node (flow key)
                    parents (:parents node)
                    new-state (reduce update-node state parents)]
                (assoc new-state key ((:fn node) new-state)))))        
        
        (update-nodes [state & keys]
            "Evaluates the state at given keys. Propagates message of 
            recomputation to parents."
            (reduce update-node state keys))
        
        ; ======================
        ; = Concurrent updates =
        ; ======================
        
        (msg-record [[state keys-remaining] new-vals]
            "A message agents send to the collating agent when they update.
            Its value consists of a [state keys-remaining] couple. When
            keys-remaining is zero, the requested update has been made."
            (let [new-state (merge state new-vals)
                new-keys (apply (partial disj keys-remaining) (keys new-vals))]
                [new-state new-keys]))
        
        (watch-fn [parent-key child-node]
            "This function is sent to the child when the parent's value changes
            by a watcher."
            (let [parents (:parents child-node)
                    update-fn (:fn child-node)]
                (fn [child-val parent]
                    (let [parent-val (deref parent)
                            status (::status parent-val)
                            value (::value parent-val)]
                        (cond
                            (= ::up-to-date status)
                                (let [new-vals (assoc child-val parent-key value)]
                                    (if (= (count new-vals) (count parents))
                                        (try {::value (update-fn new-vals) ::status ::up-to-date}
                                            (catch Exception err {::value err ::status ::error}))
                                        new-vals))
                            (= ::error status)
                                {::value value ::status ::error}
                            true
                                child-val)))))
        
        (add-update-watcher [parent-key child-key child-agent state]
            "Allows children to watch parents for updates."
            (let [parent (state parent-key)]
                (if (agent? parent)
                    (add-watcher parent :send child-agent (watch-fn parent-key (flow child-key))))))
            
        (create-agent [orig-state [state roots] key]
            "Creates an agent at the given key, which is responsible for 
            computing the value at that key. Creates agents for parents
            if necessary."
            (if (state key) [state roots]
                (let [parents (:parents (flow key))
                        parent-vals (select-keys orig-state parents)
                        [state roots] (reduce (partial create-agent orig-state) [state roots] parents)
                        a (agent parent-vals)]
                    ; Update the state with new agents at this node and at the parent nodes
                    [(assoc state key a) 
                    ; If this is a root node, add it to the roots.
                    (if (= (count parent-vals) (count parents)) 
                        (conj roots key)
                        (do
                            (map-now #(add-update-watcher % key a state) parents)
                            roots))])))
        
        (create-agents [state & keys-to-update]
            "Returns a state with agents in the keys that need to be updated,
            and the set of 'root keys' corresponding to agents that can start 
            updating immediately."
            (reduce (partial create-agent state) [state []] keys-to-update))
        
        (start-c-update [state roots collating-agent] 
            "Used by the concurrent updates."
            (fn [] (map-now #(send (state %) (fn [cur-val] {::status ::up-to-date ::value ((-> % flow :fn) cur-val)})) roots)))
            
        (concurrent-update [state & keys-to-update]
            "Returns a fn that starts the update, and the new state with agents
            corresponding to all the keys whose values are requested but not
            known."
            (let [[new-state roots] (apply create-agents state keys-to-update)
                    start (start-c-update new-state roots nil)]
                [start new-state]))
            
        (agent-update [state & keys-to-update]
            "Does a concurrent update of the given keys. Returns two things: a fn 
            to start the update and an agent whose state will eventually change to 
            [requested state []]."
            (let [[new-state roots] (apply create-agents state keys-to-update)
                    ; An agent whose value will eventually be the up-to-date state
                    collating-agent (agent [state (set (filter (comp not state) (keys new-state)))])
                    start (start-c-update new-state roots collating-agent)]
                    [start collating-agent]))
                    
        (future-update [state & keys-to-update]
            "Does a concurrent update of the given keys. Returns a delay which, when
            forced, returns the updated state."
            (let [[s a] (apply agent-update state keys-to-update)
                    ; Create a countdown latch and a watcher that opens the latch when the state is ready.
                    latch (java.util.concurrent.CountDownLatch. 1)
                    w (add-watch a latch (fn [#^java.util.concurrent.CountDownLatch latch r old-v new-v] 
                                            (if (= (count (new-v 1)) 0) (.countDown latch))))
                    ; Start the update.
                    nothing (s)]
                    (delay (do (.await latch) (@a 0)))))
        
        ] 
        {:update update-nodes 
        :forget forget 
        :change change 
        :c-update concurrent-update 
        :a-update agent-update 
        :f-update future-update}))

(defn add-node [flow key fun {:keys [timing] :or {timing :lazy}} & [args]]
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
            new-flow (assoc flow key {:fn pfun :parents parents :children #{} :timing timing})
            ; A function adding key to the children list of a parent.
            add-child (fn [nf p] (assoc nf p (assoc (nf p) :children (conj (:children (nf p)) key))))] 
            ; Notify parents of new child
            (if (and parents (not= timing :oblivious))
                (reduce add-child new-flow parents)
                new-flow))))
                
(defn add-root [flow key] 
    "Adds a root (parentless) node."
    (add-node flow key (fn []) true))
    
(defmacro def-flosures [flow]
    "Binds the flow's 'methods' to vars."
    (let [sym-dash (.concat (name flow) "-")
        name-fn #(.concat sym-dash (name %))]
    (map (fn [[key val]] `(def ~(symbol (name-fn key)) ~val)) (meta (eval flow)))))
    
(defmacro with-flosures [flosures bindings & exprs] 
    "Like let-bindings, but provides update, forget and change
    functions in context of flow."
    `(let [~'update (~flosures :update)
            ~'forget (~flosures :forget)
            ~'change (~flosures :change)
            ~'c-update (~flosures :c-update)
            ~'a-update (~flosures :a-update)
            ~'f-update (~flosures :f-update)]
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
            (add-node fl key f :lazy args)))))

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