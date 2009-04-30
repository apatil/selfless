selfless : Stateless, lazy dataflow for Clojure
===============================================

Separates the dataflow model and the state, so many states can be updated concurrently.

Usage
-----

First, build a dataflow::

    (defn fn2 [a b c d e] [a b c d e])     
    (defn fn3 [fn2] (apply + fn2))
 
    (def flow (add-root {} :fn1))
    (def flow2 (add-node flow :fn2 fn2 false [:fn1 17 :fn1 2 5]))
    (def flow3 (add-node flow2 :fn3 fn3 false [:fn2]))

The arguments of ``add-node`` are ``[flow key fun block? & [args]]``. The resulting dataflow is a ``{:key node}`` map. Nodes hold functions, parents, children, and ``block?`` slots. Blocking nodes do not 'listen' to their parents.

``add-root`` is a version of ``add-node`` that assumes the node has no parents & is blocking.

Then, seed & evaluate as many states as you want::

    (def init-state (change flow3 {} {:fn1 3}))
    (def new-state (eval-nodes flow3 init-state [:fn3 :fn1 :fn2]))
    
    (def init-state2 (change flow3 {} {:fn1 17}))
    (def new-state2 (eval-nodes flow3 init-state [:fn3 :fn1 :fn2]))
    
Since the 'library' is purely functional, you can work with multiple states in different threads without causing problems.
    
License
-------

Copyright (c) Anand Patil, 2009. Licensed under Creative Commons BY-SA, see LICENSE.