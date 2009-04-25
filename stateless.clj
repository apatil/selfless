(defn build-args [wf args]
    "Like take-where. Returns a closure that takes a list of arguments, inserts "
    (fn [f-args] 
        (loop [f-arg (first f-args)
            f-args (rest f-args)
            wff (first wf)
            wf (rest wf)
            arg (first args)
            args (rest args)
            c-args []]
            (if (or arg f-arg)
                (if wff
                    ; If this argument was a function, take the next input value
                    (recur (first f-args) (rest f-args) (first wf) (rest wf) arg args (conj c-args f-arg))
                    ; If this argument was a constant, take the next remembered value
                    (recur f-arg f-args (first wf) (rest wf) (first args) (rest args) (conj c-args arg)))
                c-args))))

(defn multi-partial [fun args]
    "Extension of partial to multiple positional arguments."
    (let [where-f (map fn? args)
            where-nf (apply + where-f)]
        (cond 
            ;If all arguments are functions, return inputs.
            (= where-nf (count args))
                [fun args]
            ; If no arguments are functions, close on the answer.
            (= where-nf 0)
                [(fn [] (apply fun args)) []]
            ; Otherwise, partially apply the function.
            true
                [(comp fun (build-args where-f (filter (comp not fn?) args))) (filter fn? args)])))

;TODO: Force unique labels.
(defn add-fn [label fun flow & [arg-labels]]
    "Adds fun to the dataflow flow, with label 'label', which must be unique."
    (let [args (map flow arg-labels)
        [pfun pargs] (multi-partial fun args)]
        (loop [new-flow (assoc flow label {:fn pfun :val nil :parents arg-labels :children #{}})
            parent (first arg-labels)
            parents (rest arg-labels)]
            (if parent
                (recur 
                    (if (new-flow parent)
                        (assoc new-flow parent (assoc (new-flow parent) :children (conj (:children (new-flow parent)) label))) 
                    new-flow)
                    (first parents) 
                    (rest parents))
                new-flow))))

(def fn1 #())       
(defn fn2 [a b] [a b])     
(def flow (add-fn :fn1 fn1 {} []))
(def flow2 (add-fn :fn2 fn2 flow [fn1 3]))