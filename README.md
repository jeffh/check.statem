# check.statem

[Docs](https://jeffh.github.io/check.statem/)

Facilities for generating test programs using state machines.

## What is this?

This library allows you to define a state machine (aka - a model) that can be used to compare against an API/subject. This allows test.check to explore possible usages of your API that is stateful (as opposed to traditional, purely functional test subjects).

In test.check terms, this library provides a generator that produces valid programs that conform to state machine specification. Shrunken values produce smaller variants of the program that still conform to the same state machine.

For details about why this can be useful, check out the [talk by John Hughes](https://www.youtube.com/watch?v=zi0rHwfiX1Q). Unlike what John Hughes' demos, this library only supports serialized state machine testing (no parallel testing).

## Usage

To install via lein:

```clojure
[net.jeffhui/check.statem "1.0.0-SNAPSHOT"]
```

Or clojure deps:

```clojure
{:deps {net.jeffhui/check.statem {:mvn/version "1.0.0-SNAPSHOT"}}}
```

To import:

```clojure
(require '[net.jeffhui.check.statem :refer [defstatem cmd-seq run-cmds check!]])
```

Then you can define your state machine:

```clojure
(require '[clojure.test.check.generators :as gen])

(defstatem key-value-statem ;; name of the state machine
  [mstate] ;; the internal model state -- starts as nil
  ;; define transitions
  (:put ;; name of the transition
    (args [] [gen/keyword gen/any-printable-equatable]) ;; associated data generators for this transition
    (advance [_ [_ k value]] ;; next-mstate given the generated data and the current mstate
      (assoc mstate k value)))
  (:get
     ;; precondition for this transition to be utilized (here: we must have stored something)
     (assume [] (pos? (count mstate)))
     (args [] [(gen/elements (keys mstate))]) ;; generate values from model state
     ;; postcondition assertion: check model state against the return value of the subject-under-test
     ;; implementation.
     (verify [_ [_ k] return-value]
        (= (mstate k) return-value))))
```

From there, we can check if we have something that works reasonably well:

```clojure
(check! key-value-statem) ;; => nil
```

This only goes through and generates + shrinks some values to make sure there isn't obviously wrong. You can also use test.check's gen/sample for some example programs, but you'll probably want to look at only one at a time for clarity:

```clojure
(rand-nth (gen/sample (cmd-seq key-value-statem)))
;; =>
[[:set [:var 1] [:put :W4 {}]]
 [:set [:var 2] [:get :W4]]]

```

The returned data is an symbolic program representation this is a sequential, imperiative form similar to:

```clojure
(do
  (def v1 (put :W4 {}))
  (def v2 (put :W4)))
```

Then, to compare the model against an implementation in tests, we need interpreter function that can translate the transitions from the state machine to use against an actual implementation:

```clojure
;; based on the example data above, the inner function will be called twice with cmd being:
;;   cmd = [:put :W4 {}], ctx = {:var-sym [:var 1]}
;;   cmd = [:get :W4], ctx = {:var-table {[:var 1] nil}, :var-sym [:var 2]}
(defn kv-interpreter [state]
  (fn [cmd ctx]
    (case (first cmd)
      :put (swap! state conj (vec (rest cmd)))
      :get (second (first  (filter (comp (partial = (second cmd)) first) @state))))))
```

Finally, run it:

```clojure
(require '[clojure.test.check.clojure-test :refer [defspec]])
(require '[clojure.test.check.properties :refer [for-all]])

(defspec kv-spec 100
  (for-all
   [cmds (cmd-seq key-value-statem)]
   (:ok? (run-cmds key-value-statem cmds (kv-interpreter (atom []))))))
```

If you run it, you'll get a test failure similar to this value:

```clojure
[[:set [:var 16] [:put :A -1]]
 [:set [:var 17] [:put :A 1]]
 [:set [:var 18] [:get :A]]]
```

This is a minimal test case that produces the failure.

Obviously, the large the system to test against, the greater difference between the model implementation and the production implemention will typically be. Small examples typically produce the similar sizes of implementations.

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
          (:ok? (run-cmds queue-statem cmds queue-interpreter)))
```

## License

Copyright © 2019 Jeff Hui

Distributed under the Eclipse Public License version 1.0.
