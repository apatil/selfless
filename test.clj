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
(def flow1 (assoc-lazy {} :x []))
(def flow2 (assoc-lazy flow1 :y fn2 [:x 17 :x 2 5]))
(def flow3 (assoc-lazy flow2 :z fn3 [:y]))
(def flow4 (assoc-lazy flow3 :w fn4 [:x :z]))
(def flow5 (assoc-oblivious flow4 :v fn4 [:x :w]))
(def flow6 (assoc-eager flow5 :q identity [:x]))
    
(def flosures6 (flosures flow6))



; Create some states
(def states
    (with-flosures flosures6
    [init-state (change {} {:x 3})
    new-state (update init-state :w :v :q)
    
    spotty-state (forget new-state :z)
    
    newer-state (change {} {:x 11})
    newerer-state (update newer-state :y :w)
    
    blocked-change-state (change new-state {:x 11})]

    [new-state spotty-state newer-state blocked-change-state]))
    
;(with-flosures flosures6
;    [[fa a] (a-update {:x 3} :z :x :y :w :v :q)
;    [fc c] (c-update {:x 3} :z :x :y :w :v :q)
;    d (f-update {:x 3} :z :x :y :w :v :q)]
;    (def fa fa)
;    (def fc fc)
;    (def a a)
;    (def c c)  
;    (def d d))