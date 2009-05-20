(ns selfless
    (:use clojure.contrib.graph clojure.contrib.monads))

(set! *warn-on-reflection* true)

; TODO: Don't use the state monad in with-flow. You want to make the state explicit at all times.
; TODO: You don't need to worry about preferred errors at this level. ZeroProbabilities 
; TODO: will always happen at leaf nodes, so the logp-accessor fn can deal with them.


(defn zipmapmap [fn coll] (zipmap coll (map fn coll)))
(defn map-now [fn coll] (doseq [x coll] (fn x)))
(defn has-keys? [m k] (every? identity (map (partial contains? m) k)))
(defn agent? [x] (instance? clojure.lang.Agent x))
(defn same-number? [x y] (= (count x) (count y)))

(defn flosures [flow]
    "Produces fns for operating on the state of the given flow."
    (letfn [
        
        ; ===================================
        ; = Destroying state after a change =
        ; ===================================
        
        (not-obliv? [key] (not= (-> key flow :timing) :oblivious))
        
        (forget-children [state key]
            "Notifies the children of a key that it has changed. 
            If they are not eager, sets their values to nil, and 
            propagates the 'message' to their children.
            If they are eager, attempts to recompute their values
            before taking the above action."
            (if (state key)
                (let [node (flow key)
                    children (:children node)]
                    (apply forget state (filter not-obliv? children)))
                state))

        (change [state new-substate]
            "Sets the flow's value at certain keys to new values.
            Forgets children's values."
            (merge (reduce forget-children state (keys new-substate)) new-substate))
        
        (forget [state & keys]
            "Forgets the flow's value at given keys."
            (let [[new-state new-keys] (reduce eager-update [state []] keys)]
                (apply dissoc (reduce forget-children new-state new-keys) new-keys)))
    
        ; ======================
        ; = Sequential updates =
        ; ======================
        
        (eager-update [[state keys] key]
            "Intended to be reduced over [state keys] pair. At end of
            reduce, state will be updated with values of all eager nodes
            that were ready to compute, and keys will contain keys of all 
            nodes that did not get updated."
            (let [node (flow key)
                eager? (= (:timing node) :eager)]
                (if eager?
                    ; If the node is eagerly-updating, give it a chance right now.
                    (if (has-keys? state (:parents node))
                        [(assoc state key ((:fn node) state)) keys]
                        [state (conj keys key)])
                    [state (conj keys key)])))
        
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
        
        (send-fn [state msg val collating-agent]
            "Creates a fn to be mapped over keys, which sends messages to 
            the keys' agents."
            (fn [key] (send (state key) msg state key val collating-agent)))
            
        (send-notify-return [state msg val key collating-agent children]
            "Wrapped by msg-err and update-and-notify"
            (do
                (if collating-agent (send collating-agent msg-record {key val}))
                (map-now (send-fn state msg val collating-agent) children)
                val))
        
        (msg-err [parent-vals state key err collating-agent children] 
            "Action taken by state-managing agents upon encountering an error.
             The error is stored in the agent and propagated to children. It is
             currently not possible to re-raise the error."
             (send-notify-return state msg-err {key err} key collating-agent children))
        
        (update-and-notify [state collating-agent node new-vals children]
            "Used by msg-update."
            (let [new-val ((:fn node) new-vals)]
                (send-notify-return state msg-update {key new-val} key collating-agent children)))
        
        (msg-update [parent-vals state key new-pv collating-agent]
            "A message that a parent can send to a child when it has 
            updated."
            (let [new-vals (merge parent-vals new-pv) 
                    node (flow key)
                    children (:children node)]
                (if (same-number? new-vals (:parents node)) 
                    ; If the value can be computed, do it.
                    (try (update-and-notify state collating-agent node new-vals children)
                        (catch Exception err (msg-err parent-vals state key err collating-agent children)))
                    ; Otherwise just record the parent's value.
                    new-vals)))
        
        (msg-record [[state keys-remaining] new-vals]
            "A message agents send to the collating agent when they update.
            Its value consists of a [state keys-remaining] couple. When
            keys-remaining is zero, the requested update has been made."
            (let [new-state (merge state new-vals)
                new-keys (apply (partial disj keys-remaining) (keys new-vals))]
                [new-state new-keys]))
            
        (create-agent [orig-state [state roots] key]
            "Creates an agent at the given key, which is responsible for 
            computing the value at that key. Creates agents for parents
            if necessary."
            (if (state key) [state roots]
                (let [parents (:parents (flow key))
                        parent-vals (select-keys orig-state parents)
                        [state roots] (reduce (partial create-agent orig-state) [state roots] parents)]
                    ; Update the state with new agents at this node and at the parent nodes
                    [(assoc state key (agent parent-vals)) 
                    ; If this is a root node, add it to the roots.
                    (if (same-number? parent-vals parents) (conj roots key) roots)])))
        
        (create-agents [state & keys-to-update]
            "Returns a state with agents in the keys that need to be updated,
            and the set of 'root keys' corresponding to agents that can start 
            updating immediately."
            (reduce (partial create-agent state) [state []] keys-to-update))
        
        (start-c-update [state roots collating-agent] 
            "Used by the concurrent updates."
            (fn [] (map-now (send-fn state key msg-update {} collating-agent) roots)))
            
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
            (reduce add-child new-flow parents))))
                
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
                
(defn flow-graph [dir flow]
    "Returns a clojure-contrib graph corresponding to the given flow."
    (struct directed-graph (keys flow) #(dir (flow %))))

(def forward-graph (partial flow-graph :children))
(def backward-graph (partial flow-graph :parents))