(load-file "selfless.clj")
(refer 'selfless)
(use 'clojure.contrib.stacktrace)

; ========
; = Test =
; ========

(defn fn2 [a b c d e] [a b c d e  "a"])
(defn fn3 [y] (apply + y))
(defn fn4 [x y] (+ x y))

; Create a flow
(def-flow flow3 {}
    [x ()
    y (fn2 x 17 x 2 5)
    z (fn3 y)
    w (fn4 x z)])
    
(def flosures3 (flosures flow3))

;; Create some states
;(with-flosures flosures3
;    [init-state (change {} {:x 3})
;    new-state (update init-state :z :x :y :w)
;    spotty-state (forget new-state :z)
;
;    newer-state (change {} {:x 11})
;    newerer-state (update newer-state :y :w)
;
;    blocked-change-state (change new-state {:x 11})]
;
;    [new-state spotty-state newer-state newerer-state blocked-change-state])
    
(with-flosures flosures3
    [[fa a] (a-update {:x 3} :z :x :y :w)
    [fc c] (c-update {:x 3} :z :x :y :w)
    d (f-update {:x 3} :z :x :y :w)]
    (def fa fa)
    (def fc fc)
    (def a a)
    (def c c)
    (def d d))