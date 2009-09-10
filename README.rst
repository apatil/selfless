:Date: 3 June 2009
:Author: Anand Patil
:Contact: anand.prabhakar.patil@gmail.com
:Copyright: Anand Patil, 2009.
:License: See LICENSE

Selfless provides stateless, lazy dataflow for Clojure. It separates the dataflow model and the state, so many states can be updated concurrently.

**NOTE**: The 'delay' branch (which you are using) requires the par branch of clojure, available from http://github.com/richhickey/clojure/tree/par. To build it, you'll need Rich Hickey's build of the jsr166 from http://cloud.github.com/downloads/richhickey/clojure/jsr166y.jar on your classpath.

Usage
-----

First, build a dataflow::

    (defn fn2 [a b c d e] [a b c d e])     
    (defn fn3 [fn2] (apply + fn2))
 
    (def flow (add-root {} :fn1))
    (def flow2 (add-node flow :fn2 fn2 [:fn1 17 :fn1 2 5]))
    (def flow3 (add-oblivious flow2 :fn3 fn3 [:fn2]))

The arguments of ``add-node`` and ``add-oblivious`` are ``[flow key fun & [args]]``. The resulting dataflow is a ``{:key node}`` map. Nodes hold functions, parents, children, and timing slots. Timing can be lazy (the default) and oblivious, meaning a node does not do anything in response to parent updates. ``add-root`` is a version of ``add-node`` that assumes the node has no parents & that a value for it must be provided.

Having created the dataflow, you can create functions ``flow3-init``, ``flow3-forget`` and ``flow3-change`` with::
    
    (def-flosures flow3)
    
or if you don't want to bind the functions to vars you can create them in a map keyed by ``:init``, ``:forget`` and ``:change`` with::

    (flosures flow3)
    
Then you can use the ``with-flosures`` macro, which creates a context within which the functions ``init``, ``forget`` and ``change`` are understood to pertain to the given flow::

    (with-flosures (flosures flow3)
        (let
            [init-state (init {:fn1 3})
            init-vals (force-state init-state)
            partial-state (forget init-state :fn2)
            altered-state (change new-state {:fn1 3})]
            [init-state init-vals altered-state]))

Concurrency
-----------
    
Nodes' values are wrapped in delays. When you force a value, it forces its parents' values, and so on. Evaluations are done concurrently (using ``pmap``) where possible. The ``force-state`` function evaluates the entire state and strips away the delays.