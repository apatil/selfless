(defn where [bool-vec]
    "Like numpy's where."
    (for [i (range (count bool-vec)) :when (bool-vec i)] i))

(defn build-args [wf args]
    "Like take-where. Returns a closure that takes a list of arguments, inserts input values."
    (fn [f-args] (replace args f-args)))

(defn multi-partial [test fun args]
    "Extension of partial to multiple positional arguments."
    (let [where-f (map test args)
            where-nf (apply + where-f)]
        (cond 
            ;If all arguments are functions, return inputs.
            (= where-nf (count args))
                fun
            ; If no arguments are functions, close on the answer.
            (= where-nf 0)
                (fn [] (apply fun args))
            ; Otherwise, partially apply the function.
            true
                (comp fun (build-args where-f (filter (comp not test) args))))))

;TODO: Force unique labels.
(defn add-fn [label fun flow & [args]]
    "Adds fun to the dataflow flow, with label 'label', which must be unique."
    (let [test #(if (flow %) 1 0)
        parents (set (filter test args))
        pfun (multi-partial test fun args)]
        (loop [new-flow (assoc flow label {:fn pfun :val nil :parents parents :children #{}})
            parent (first parents)
            parents (rest parents)]
            (if parent
                (recur
                    (if (new-flow parent)
                        (assoc new-flow parent (assoc (new-flow parent) :children (conj (:children (new-flow parent)) label)))
                    new-flow)
                    (first parents)
                    (rest parents))
                new-flow))))

(def fn1 #())       
(defn fn2 [a b c d e] [a b c d e])     
(def flow (add-fn :fn1 fn1 {} []))
(def flow2 (add-fn :fn2 fn2 flow [:fn1 3 :fn1 2 5]))