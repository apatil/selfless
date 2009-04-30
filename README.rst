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

The arguments of ``add-node`` are ``[flow key fun block? & [args]]``. The resulting dataflow is a ``{:key node}`` map. Nodes hold functions, parents, children, and ``block?`` slots. Blocking nodes do not 'listen' to their parents. ``add-root`` is a version of ``add-node`` that assumes the node has no parents & is blocking.

Having created the dataflow, you can create functions ``flow3-update``, ``flow3-forget`` and ``flow3-change`` with::
    
    (def-flosures flow3)
    
or if you don't want to bind the functions to vars you can create them in a map keyed by ``:update``, ``:forget`` and ``:change`` with::

    (flosures flow3)
    
Or you can use the with-flow macro, within which the functions ``update``, ``forget`` and ``change`` are understood to pertain to the given flow::

    (with-flow flow3
        exprs...)

Then, seed & updateuate as many states as you want::

    (def init-state (flow3-change {} {:fn1 3}))
    (def new-state (flow3-update init-state :fn3 :fn1 :fn2))    

    (def init-state (flow3-change {} {:fn1 17}))
    (def new-state (flow3-update init-state :fn3 :fn1 :fn2))    

You can also start a new state by altering an existing one::

    (def newer-state (flow3-change new-state {:fn1 1}))
    
or by just pruning an existing one::
    
    (def other-state (flow3-forget new-state :fn2 :fn3))
    
Since the 'library' is purely functional, you can work with multiple states in different threads without causing problems.
    
License
-------

Copyright (c) Anand Patil, 2009. Licensed under Creative Commons BY-SA, see LICENSE.