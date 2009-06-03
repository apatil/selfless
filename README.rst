:Date: 3 June 2009
:Author: Anand Patil
:Contact: anand.prabhakar.patil@gmail.com
:Copyright: Anand Patil, 2009.
:License: See LICENSE

Selfless provides stateless, lazy dataflow for Clojure. It separates the dataflow model and the state, so many states can be updated concurrently.

Usage
-----

First, build a dataflow::

    (defn fn2 [a b c d e] [a b c d e])     
    (defn fn3 [fn2] (apply + fn2))
 
    (def flow (add-root {} :fn1))
    (def flow2 (add-node flow :fn2 fn2 {:timing :eager} [:fn1 17 :fn1 2 5]))
    (def flow3 (add-node flow2 :fn3 fn3 [:fn2]))

The arguments of ``add-node`` are ``[flow key fun & [args]]``, with an optional keyword argument ``:timing``. The resulting dataflow is a ``{:key node}`` map. Nodes hold functions, parents, children, and timing slots. There are three timings available: eager, lazy (the default) and oblivious, meaning a node not do anything in response to parent updates. ``add-root`` is a version of ``add-node`` that assumes the node has no parents.

Having created the dataflow, you can create functions ``flow3-update``, ``flow3-forget`` and ``flow3-change`` with::
    
    (def-flosures flow3)
    
or if you don't want to bind the functions to vars you can create them in a map keyed by ``:update``, ``:forget`` and ``:change`` with::

    (flosures flow3)
    
Then you can use the ``with-flosures`` macro, within which the functions ``update``, ``forget`` and ``change`` are understood to pertain to the given flow::

    (with-flosures (flosures flow3)
        [init-state {:fn1 3}
        new-state (update init-state :fn3 :fn1 :fn2)
        partial-state (forget init-state :fn2)
        altered-state (change new-state {:fn1 3})])

Concurrency
-----------
    
If your dataflow contains heavy functions, you can perform three types of concurrent updates.

  1. Receive a state map populated with agents whose values will eventually update, and a fn that starts the update.
  2. Receive a single agent whose state will eventually change to the updated state, and a fn that starts the update
  3. Receive a future which, when forced, returns the updated state.
  
In the first two cases, the update does not start until you call the fn. This gives you the chance to add watchers before the update begins.
    
Since the 'library' is purely functional, you can update multiple states (either serially or concurrently) in different threads without causing problems.
    
License
-------

Copyright (c) Anand Patil, 2009. Licensed under Creative Commons BY-SA, see LICENSE.