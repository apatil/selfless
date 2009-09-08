(load-file "selfless.clj")
(refer 'selfless)
(use 'clojure.contrib.stacktrace)

; ========
; = Test =
; ========

(defn fn2 [a b c d e] [a b c d e])
(defn fn3 [y] (apply + y))
(defn fn4 [x y] (+ x y))

; Create a flow
(def flow1 (assoc-node {} :x []))
(def flow2 (assoc-node flow1 :y fn2 [:x 17 :x 2 5]))
(def flow3 (assoc-node flow2 :z fn3 [:y]))
(def flow4 (assoc-node flow3 :w fn4 [:x :z]))
(def flow5 (assoc-oblivious flow4 :v fn4 [:x :w]))
(def flow6 (assoc-node flow5 :q identity [:x]))
    
(def flosures6 (flosures flow6))



; Create some states
(def states
    (with-flosures flosures6
    [init-state (change (new-state) {:x 3})
    spotty-state (forget init-state :z)
    newer-state (change {} {:x 11})
    blocked-change-state (change init-state {:x 11})]

    [init-state spotty-state newer-state blocked-change-state]))