# check.statem

[![Clojars Project](https://img.shields.io/clojars/v/net.jeffhui/check.statem.svg)](https://clojars.org/net.jeffhui/check.statem)
[![cljdoc badge](https://cljdoc.org/badge/net.jeffhui/check.statem)](https://cljdoc.org/d/net.jeffhui/check.statem/CURRENT)

Generate stateful tests with state machines.

## What is this?

This library allows you to define a state machine (aka - a model) that can be
used to compare against an API/SUT. This allows test.check to explore possible
usages of your API that is stateful (as opposed to traditional, purely
functional test subjects).

In test.check terms, this library provides a generator that produces valid
programs that conform to state machine specification. Shrunken values produce
smaller variants of the program that still conform to the same state machine.


For details about why state machine testing can be useful, check out the [talk
by John Hughes](https://www.youtube.com/watch?v=zi0rHwfiX1Q). Unlike what John
Hughes' demos, this library only supports serialized state machine testing (no
parallel testing). Maybe someday in the future.


### Installation

To install via lein:

```clojure
[net.jeffhui/check.statem "1.0.0-SNAPSHOT"]
```

Or clojure deps:

```clojure
{:deps {net.jeffhui/check.statem {:mvn/version "1.0.0-SNAPSHOT"}}}
```

### The Problem with the Naive Approach

While you can currently do something like:

```clojure
(def put-generator ...)
(def get-generator ...)
(def kv-store-ops-generator (gen/vector (gen/one-of [get-generator put-generator])))
```

It assumes a naive form of command generation, it's much more difficult to
encode related behaviors across commands:

- What if you always want `get` to fetch a key a previous `put` has placed?
- What if you always want `put` to write to keys used yet?


That's not even considering the shrinking behaviors:

- How do you honor shrinking with the above constraints?
  - eg - smaller sequences of commands should still have `get` to a key after `put`?
- How do you ensure shrinking of related data?
  - If you're using test.check's `bind`, then full shrinking isn't honored ([TCHECK-112](http://dev.clojure.org/jira/browse/TCHECK-112))
  
check.statem provides a more structured approach to defining state machine
models that then can be used to generate a sequence of commands to execute. The
command sequence shrinks according to the state machine.

## Example

```clojure
(require '[net.jeffhui.check.statem :refer [defstatem cmd-seq run-cmds]])

(definterface IQueue
  (^void init [^int size])
  (^void enqueue [item])
  (dequeue []))

; our code we're testing
(deftype TestQueue [^:volatile-mutable items ^:volatile-mutable capacity]
  IQueue
  (enqueue [this item]
    (set! (.items this) (conj items item)))
  (dequeue [this]
    (let [result (first items)]
      (set! (.items this) (subvec items 1))
      result)))

(defn queue-interpreter [cmd {:keys [varsym var-table]}]
  (case (first cmd)
    :new     (TestQueue. [] (second cmd))
    :enqueue (.enqueue ^IQueue (var-table (second cmd)) (nth cmd 2))
    :deque   (.dequeue ^IQueue (var-table (second cmd)))))

(defstatem queue-statem
  [mstate]
  (:new (assume [] (nil? mstate))
        (given [] gen/pos-int)
        (advance [v [_ n]] {:items    []
                            :capacity n
                            :ref      v}))
  (:enqueue (assume [] (and (not (nil? mstate))
                            (< (count (mstate :items)) (mstate :capacity))))
            (given [] [(gen/return (:ref mstate)) gen/int])
            (advance [v [_ _ n]] (update mstate :items conj n)))
  (:deque (assume [] (and (not (nil? mstate))
                          (pos? (count (mstate :items)))))
          (advance [_ _] (update mstate :items subvec 1))
          (given [] (gen/return (:ref mstate)))
          (verify [_ _ r] (= r (first (:items mstate))))))

(for-all [cmds (cmd-seq queue-statem)]
          ;; run-cmds-debug is useful to debug commands executed
          (run-cmds queue-statem cmds queue-interpreter))
```

## License

Copyright © 2020 Jeff Hui

Distributed under the Eclipse Public License version 1.0.


## Wish / Consider / Todo List

Interesting ideas to consider for the future of this library. This doesn't
doesn't mean it'll be implemented, just for consideration:

- [ ] Build generators from json schema
- [ ] State Machine QOL
  - [ ] Track & handle [[only-when]] implementation for commands that have dependent references
  - [ ] Make it faster!
  - [ ] Provide better debugging information when [[verify]] fails
- [ ] Async Features
  - [ ] Being able to encode "sleep" command with varable sleep interval (and shrink other commands to this)
  - [ ] Be able to model / track "potential" commands for un-observable behaviors
- [ ] Parallel State Machine Generation
   - [ ] Prelude + 2 - 5 threads
   - [ ] Shrinking with at-least (inverse of always)
   - [ ] Cooperative scheduling
     - [ ] Can control concurrent execution order via a value
       - [ ] Which (obviously) can then get generated & shrunk
       - [ ] Investigate some way to use compiler-like magic to automatically instrument concurrent execution
- [ ] Documentation
   - [ ] A getting start guide / tutorial
   - [ ] More state machine examples in the test suite
   - [ ] Internals - how shrinking works
- [ ] Fancy things
   - [ ] Logo?
   - [ ] A proper website?
   - [ ] A talk?
      
