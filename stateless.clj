(defmacro structmap-and-accessors [sym & fields]
    "Defunes a structmap with given symbol, and defines accessors for all its fields."
    (let [code-lst `(defstruct ~sym ~@fields)
            sym-dash (.concat (name sym) "-")
            accessor-names (zipmap fields (map (comp #(.concat sym-dash %) name) fields))]
        (cons 'do (cons code-lst
            (for [field fields] (let [n (accessor-names field) s (symbol n)]
                `(def ~s (accessor ~sym ~field))))))))
                
(structmap-and-accessors node :fn :parents :children :block?)

(defn add-node [label fun flow block? & [args]]
    "Adds fun to the dataflow flow, with label 'label', which must be unique."
    (if (flow label) 
        (throw (Exception. (.concat "Node label already taken: " (name label))))
        (let [
            ; Parents are nodes in the flow
            parents (set (filter #(if (flow %) 1 0) args))   
            ; A partially-applied version of the function that takes nodes only
            pfun #(apply fun (replace % args))                  
            ; The flow, with the new function added
            new-flow (assoc flow label (struct node pfun parents #{} block?))
            ; A function adding label to the children list of a parent.
            add-child (fn [nf p] (assoc nf p (assoc (nf p) :children (conj (:children (nf p)) label))))] 
            ; Notify parents of new child
            (if parents
                (reduce add-child new-flow parents)
                new-flow))))

(defn notify [children state label]
    "Notifies the children of a label that it has changed. 
    Sets their values to nil, and propagates the 'message' 
    to their children."
    (reduce #(  ) state children))

(defn change-state [flow state new-substate]
    "Sets the flow's value at certain labels to new values."
    (reduce #(  ) state new-substate))
        
(def fn1 #())       
(defn fn2 [a b c d e] [a b c d e])     
(defn fn3 [fn2] (apply + fn2))
(def flow (add-node :fn1 fn1 {} false []))
(def flow2 (add-node :fn2 fn2 flow false [:fn1 3 :fn1 2 5]))
(def flow3 (add-node :fn3 fn3 flow2 false [:fn2]))
; Error here
(def flow3 (add-node :fn3 fn3 flow3 [:fn2]))