(defn add-fn [fun flow & [args]]
    "Adds fun to the dataflow flow, updating the children of ancestors appropriately."
    (loop [new-flow (assoc flow fun {:val nil :parents args :children #{}})
            parent (first args)
            parents (rest args)]
        (if parent
            (recur 
                (if (new-flow parent)
                    (assoc new-flow parent (assoc (new-flow parent) :children (conj (:children (new-flow parent)) fun))) 
                new-flow)
                (first parents) 
                (rest parents))
            new-flow)))

(def fn1 #())       
(defn fn2 [a b] [a b])     
(def flow (add-fn fn1 {} []))
(def flow2 (add-fn fn2 flow [fn1 3]))