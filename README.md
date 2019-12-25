# check.statem

Facilities for generating test programs using state machines.

For details about why this can be useful, check out the [talk by John Hughes](https://www.youtube.com/watch?v=zi0rHwfiX1Q). Unlike what John Hughes' demos, this library only supports serialized state machine testing (no parallel testing).

## Usage

To install via lein:

```
[net.jeffhui/check.statem "1.0.0-SNAPSHOT"]
```

Or clojure deps:

```
{:deps {net.jeffhui/check.statem {:mvn/version "1.0.0-SNAPSHOT"}}}
```

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
