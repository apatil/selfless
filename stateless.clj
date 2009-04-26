;TODO: Force unique labels.
(defn add-fn [label fun flow & [args]]
    "Adds fun to the dataflow flow, with label 'label', which must be unique."
    (let [
        ; Parents are nodes in the flow
        parents (set (filter #(if (flow %) 1 0) args))   
        ; A partially-applied version of the function that takes nodes only
        pfun (comp fun #(replace % args))                  
        ; The flow, with the new function added
        new-flow (assoc flow label {:fn pfun :val nil :parents parents :children #{}})
        ; A function adding label to the children list of a parent.
        reduce-fn (fn [nf p] (assoc nf p (assoc (nf p) :children (conj (:children (nf p)) label))))] 
        ; Notify parents of new child
        (if parents
            (reduce reduce-fn new-flow parents)
            new-flow)))
        
(def fn1 #())       
(defn fn2 [a b c d e] [a b c d e])     
(def flow (add-fn :fn1 fn1 {} []))
(def flow2 (add-fn :fn2 fn2 flow [:fn1 3 :fn1 2 5]))