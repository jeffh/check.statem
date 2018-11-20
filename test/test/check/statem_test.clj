(ns test.check.statem-test
  (:require [clojure.test :refer :all]
            [test.check.statem :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]))

(definterface IQueue
  (^void init [^int size])
  (^void enqueue [item])
  (dequeue []))

(comment
  (def ^IQueue q (TestQueue. [] 3))

  (.enqueue q 0)
  (.enqueue q 1)
  (.dequeue q))

(defstatem queue-statem
  "The contract for a queue with a max size"
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
          (verify [_ r] (= r (first (:items mstate))))))

(deftype TestQueue [^:volatile-mutable items ^:volatile-mutable capacity]
  IQueue
  (enqueue [this item]
    (set! (.items this) (conj items item)))
  (dequeue [this]
    (let [result (first items)]
      (set! (.items this) (subvec items 1))
      result)))

(defn queue-runner [cmd varsym var-table]
  (case (first cmd)
    :new     (TestQueue. [] (second cmd))
    :enqueue (.enqueue ^IQueue (var-table (second cmd)) (nth cmd 2))
    :deque   (.dequeue ^IQueue (var-table (second cmd)))))

(defspec behaves-like-a-queue 100
  (for-all [cmds (cmd-seq queue-statem)]
           (:ok? (run-cmds queue-statem cmds queue-runner))))

(defn spec-timings [v]
  (let [t 10]
    (/
     (reduce + 0
             (map (fn [_] (:time-elapsed-ms (clojure.edn/read-string (with-out-str (clojure.test/test-var v)))))
                  (range t)))
     t)))

(comment
  ;; ~2400ms
  (println (long (spec-timings #'test.check.statem-test/behaves-like-a-queue)) " ms")
  (clojure.test/test-var #'test.check.statem-test/behaves-like-a-queue)
  )

