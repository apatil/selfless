(ns selfless
    (:use clojure.contrib.graph)
    (:use clojure.parallel))

;(set! *warn-on-reflection* true)
;TODO: Get fj-pmap to work. It's not handing off properly. 

(defn fj-pmap [f coll] (pvec (par coll :map f)))
(defn force-state [state] (zipmap (keys state) (pmap force (vals state))))

(defn flosures [flow]
    "Produces fns for operating on the state of the given flow."
    (letfn [
        
        (obliv? [key] (= (-> key flow :timing) :oblivious))
        (parents [key] (-> key flow :parents))
        (children [key] (-> key flow :children))
        
        ; ===================================
        ; = Destroying state after a change =
        ; ===================================

        ; A version that checks for obliviousness
        (forget-- [state key] (if (obliv? key) state (forget- state key)))
        
        ; A version that does NOT check. Always calls the version that checks.
        (forget- [state key] 
            (if (state key)
                (reduce forget-- (dissoc state key) (children key))
                state))
        
        ; A user-callable version. Will not check for obliviousness.
        (forget [state & keys]
            "User-callable fn used to dissoc certain keys from a state."
            (reduce forget- state keys))
        
        ; Will not check for obliviousness.
        (change [state new-substate]
            "User-callable fn used to change a state's value at certain keys."
            (merge (reduce forget- state (keys new-substate)) new-substate))
                
        ; =======================
        ; = Creating new states =
        ; =======================
        
        (add-delay [state key] 
            (if (state key) state
                (let [state- (reduce add-delay state (parents key))]
                    (assoc state- key (delay ((:fn (flow key)) state-))))))   
                
        (complete [state] (reduce add-delay state (keys flow)))
    
        ] 
        {:flow flow
        :new-state #(complete {})
        :forget (comp complete forget)
        :change (comp complete change)
        :init (comp complete (partial change {}))
        :obliv? obliv?
        :parents parents
        :children children}))

(defn assoc-node- [timing flow key fun & [args]]
    "Adds fun as a node to the dataflow flow, with key 'key'. 
    Labels must be unique within dataflows."
    (if (flow key) 
        (throw (Exception. (.concat "Node key already taken: " (name key))))
        (let [
            args (if args args [])
            ; Parents are nodes in the flow
            parents (set (filter (fn [key] (flow key)) args))   
            ; A version of the function that evaluates arguments in parallel.
            pfun #(apply fun (pmap force (replace % args)))
            ;pfun identity
            ; The flow, with the new function added
            new-flow (assoc flow key {:fn pfun :parents parents :children #{} :timing timing})
            ; A function adding key to the children list of a parent.
            add-child (fn [nf p] (assoc nf p (assoc (nf p) :children (conj (:children (nf p)) key))))] 
            ; Notify parents of new child
            (reduce add-child new-flow parents))))

(def assoc-node (partial assoc-node- :lazy))
(def assoc-oblivious (partial assoc-node- :oblivious))
(defn root-fn [] 
    (throw (Exception. (.concat "Root node uninitialized: " (name key)))))
(defn assoc-root [flow key] (assoc-node- :lazy flow key root-fn []))
    
(defmacro def-flosures [flow]
    "Binds the flow's 'methods' to vars."
    (let [sym-dash (.concat (name flow) "-")
        name-fn #(.concat sym-dash (name %))]
    (map (fn [[key val]] `(def ~(symbol (name-fn key)) ~val)) (meta (eval flow)))))
    
(defmacro with-flosures [flosures bindings & exprs] 
    "Like let-bindings, but provides update, forget and change
    functions in context of flow."
    `(let [~'new-state (~flosures :new-state)
            ~'forget (~flosures :forget)
            ~'change (~flosures :change)
            ~'init (~flosures :init)
            ~'obliv? (~flosures :obliv?)
            ~'parents (~flosures :parents)
            ~'children (~flosures :children)]
            (let ~bindings ~@exprs)))
                
(defn flow-graph [dir flow]
    "Returns a clojure-contrib graph corresponding to the given flow."
    (struct directed-graph (keys flow) #(dir (flow %))))

(def forward-graph (partial flow-graph :children))
(def backward-graph (partial flow-graph :parents))