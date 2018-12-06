(ns test.check.statem-test
  (:require [clojure.test :refer :all]
            [test.check.statem :refer :all]
            [clojure.test.check :as tc]
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
          (verify [_ _ r] (= r (first (:items mstate))))))

(deftype TestQueue [^:volatile-mutable items ^:volatile-mutable capacity]
  IQueue
  (enqueue [this item]
    (set! (.items this) (conj items item)))
  (dequeue [this]
    (let [result (first items)]
      (set! (.items this) (subvec items 1))
      result)))

(defn queue-runner [cmd {:keys [varsym var-table]}]
  (case (first cmd)
    :new     (TestQueue. [] (second cmd))
    :enqueue (.enqueue ^IQueue (var-table (second cmd)) (nth cmd 2))
    :deque   (.dequeue ^IQueue (var-table (second cmd)))))

(defn- deque-skewed-cmds [kw->cmds]
  (let [freq {:new     100
              :enqueue 1
              :deque   1000}]
    (gen/frequency (mapv (fn [[k c]]
                           [(freq k) c])
                         kw->cmds))))

(defspec queue-program-generation-using-fair-distribution 100
  (for-all [cmds (cmd-seq queue-statem)]
           (:ok? (run-cmds queue-statem cmds queue-runner))))

(defspec queue-program-generation-using-custom-distribution 100
  (for-all [cmds (cmd-seq queue-statem {:select-generator (select-cmds deque-skewed-cmds)})]
           (let [f (frequencies (mapv (comp first last) cmds))]
             (and
              ;; statem restricts new to always be 1
              (= 1 (:new f))
              ;; we should almost always deque as many times as we enque based
              ;; on our bias generator
              (< (- (:enqueue f 0)
                    (:deque f 0))
                 3)
              ;; also, this should still pass against our queue
              (:ok? (run-cmds queue-statem cmds queue-runner))))))

(defn spec-timings [v]
  (let [t 10]
    (/
     (reduce + 0
             (map (fn [_] (:time-elapsed-ms (clojure.edn/read-string (with-out-str (clojure.test/test-var v)))))
                  (range t)))
     t)))

(comment
  (test.check.statem/statem-check! queue-statem)
  (map (comp first last) (gen/sample (cmd-seq queue-statem {:size 3})))

  ;; --- no tracing
  ;; ~2400ms ;; first test
  ;; ~3500ms ;; fix shrinking, preserving var indicies
  ;; ~2700ms ;; vectorize varsyms, naive walk of generated form
  ;; ~2500ms ;; convert statemachine from map to defrecord with field access
  ;; ~1800ms ;; inline drop-seq-permutations into shrink-commands* transducer
  (println (long (spec-timings #'test.check.statem-test/queue-program-generation-using-fair-distribution)) " ms")
  (clojure.test/test-var #'test.check.statem-test/queue-program-generation-using-fair-distribution)
  (test.check.statem/dump-trace)
  )
