(load-file "selfless.clj")
(refer 'selfless)
(use 'clojure.stacktrace)
(use 'clojure.inspector)

; ========
; = Test =
; ========

(defn sleeping [res] (do (. Thread sleep 100) res))
(defn fn2 [a b c d e] (sleeping [a b c d e]))
(defn fn3 [y] (sleeping (apply + y)))
(defn fn4 [x y] (sleeping (+ x y)))

; Create a flow
(def flow1 (assoc-root {} :x))
(def flow2 (assoc-node flow1 :y fn2 [:x 17 :x 2 5]))
(def flow3 (assoc-node flow2 :z fn3 [:y]))
(def flow4 (assoc-node flow3 :w fn4 [:x :z]))
(def flow5 (assoc-oblivious flow4 :v fn4 [:x :z]))
(def flow6 (assoc-node flow5 :q identity [:x]))


(with-flosures (flosures flow6)
    (inspect children)
    (def states
        (let
            [init-state (init {:x 3})
            spotty-state (forget init-state :z)
            newer-state (change {} {:x 11})
            blocked-change-state (change init-state {:x 11})]    
        [init-state spotty-state newer-state blocked-change-state]))
    
    ; Takes the same time as three 100-ms sleeps, not four!
    (time (print (force-state (states 2)))))
    
