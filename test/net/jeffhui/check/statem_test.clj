(ns net.jeffhui.check.statem-test
  (:require [clojure.test :refer [testing deftest is]]
            [net.jeffhui.check.statem :refer [defstatem cmd-seq run-cmds run-cmds-debug select-by-frequency check!]]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.walk :as walk]))

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
        (args [] [gen/pos-int])
        (advance [v [_ n]] {:items    []
                            :capacity n
                            :ref      v}))
  (:enqueue (assume [] (and (not (nil? mstate))
                            (< (count (mstate :items)) (mstate :capacity))))
            (args [] [(gen/return (:ref mstate)) gen/int])
            (advance [v [_ _ n]] (update mstate :items conj n)))
  (:deque (assume [] (and (not (nil? mstate))
                          (pos? (count (mstate :items)))))
          (advance [_ _] (update mstate :items subvec 1))
          (args [] [(gen/return (:ref mstate))])
          (verify [prev-mstate _ r] (= r (first (:items prev-mstate))))))

(deftype TestQueue [^:volatile-mutable items ^:volatile-mutable capacity]
  IQueue
  (enqueue [this item]
    (set! (.items this) (conj items item)))
  (dequeue [this]
    (let [result (first items)]
      (set! (.items this) (subvec items 1))
      result)))

(defn queue-runner [cmd {:keys [var-sym var-table]}]
  (case (first cmd)
    :new     (TestQueue. [] (second cmd))
    :enqueue (.enqueue ^IQueue (var-table (second cmd)) (nth cmd 2))
    :deque   (.dequeue ^IQueue (var-table (second cmd)))))

(defspec queue-program-generation-using-fair-distribution 100
  (for-all [cmds (cmd-seq queue-statem)]
           (:ok? (run-cmds queue-statem cmds queue-runner))))

(defspec queue-program-generation-using-custom-distribution 100
  (for-all [cmds (cmd-seq queue-statem {:select-generator (select-by-frequency {:new     100
                                                                                :enqueue 1
                                                                                :deque   1000})})]
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

(defn- macroexpand-thrown? [form]
  (try
    (walk/macroexpand-all form)
    false
    (catch Throwable t
      (instance? AssertionError (.getCause t)))))

(deftest validate-defstatem
  (is (macroexpand-thrown? `(defstatem bad-statem [mstate] (:add (~'assume [~'x] true)))))
  (is (macroexpand-thrown? `(defstatem bad-statem [mstate] (:add (~'args [~'x] gen/int)))))
  (is (macroexpand-thrown? `(defstatem bad-statem [mstate] (:add (~'advance [~'_])))))
  (is (macroexpand-thrown? `(defstatem bad-statem [mstate] (:add (~'advance [~'_ ~'_ ~'_])))))
  (is (macroexpand-thrown? `(defstatem bad-statem [mstate] (:add (~'verify [~'_ ~'_])))))
  (is (macroexpand-thrown? `(defstatem bad-statem [mstate] (:add (~'verify [~'_ ~'_ ~'_ ~'_])))))
  (is (macroexpand-thrown? `(defstatem bad-statem [mstate] (:add (~'blah [])))))
  (is (macroexpand-thrown? `(defstatem bad-statem [mstate this extra]))))

(deftest run-cmds-asserts-against-poor-copy-from-test-check-test-results
  (is (thrown? IllegalArgumentException
               (run-cmds queue-statem [[[:set [:var 1] [:new 2]]]]) queue-runner))
  (is (run-cmds queue-statem [[:set [:var 1] [:new 2]]] queue-runner)))

(deftest statem-check-rules
  (testing "1-less arg in generator destructuring"
    (is (thrown? AssertionError
                 (do (defstatem bad-statem [mstate]
                                (:add (args [] gen/int)
                                      (verify [_ [a] _] true)))
                     (check! bad-statem))))
    (is (thrown? AssertionError
                 (do (defstatem bad-statem [mstate]
                                (:add (args [] gen/int)
                                      (advance [_ [a]] true)))
                     (check! bad-statem))))

    (is (thrown? AssertionError
                 (do (defstatem bad-statem [mstate]
                                (:add (args [] gen/int)
                                      (verify [_ [a] _] true)))
                     (check! bad-statem)))))

  (testing "1-more arg in generator destructuring"
    (is (thrown? AssertionError
                 (do (defstatem bad-statem [mstate]
                                (:add (args [] gen/int)
                                      (verify [_ [a b c] _] true)))
                     (check! bad-statem))))

    (is (thrown? AssertionError
                 (do (defstatem bad-statem [mstate]
                                (:add (args [] gen/int)
                                      (advance [_ [a b c]] true)))
                     (check! bad-statem))))

    (is (thrown? AssertionError
                 (do (defstatem bad-statem [mstate]
                                (:add (args [] gen/int)
                                      (verify [_ [a b c] _] true)))
                     (check! bad-statem))))))

(defn spec-timings [v]
  (let [t 10]
    (/
     (reduce + 0
             (map (fn [_] (:time-elapsed-ms (clojure.edn/read-string (with-out-str (clojure.test/test-var v)))))
                  (range t)))
     t)))

(comment
  (check! queue-statem)
  (clojure.pprint/pprint
   (last
    (gen/sample (cmd-seq queue-statem {:size 3}))))

  ;; --- no tracing, rounded to nearest 100ms, informal test runs
  ;; ~2400ms ;; first test
  ;; ~3500ms ;; fix shrinking, preserving var indicies
  ;; ~2700ms ;; vectorize varsyms, naive walk of generated form
  ;; ~2500ms ;; convert statemachine from map to defrecord with field access
  ;; ~1800ms ;; inline drop-seq-permutations into shrink-commands* transducer
  ;; ~1700ms ;; (rest (rest impl)) for statem, vector-only args generator
  (println (long (spec-timings #'net.jeffhui.check.statem-test/queue-program-generation-using-fair-distribution)) " ms")
  (clojure.test/test-var #'net.jeffhui.check.statem-test/queue-program-generation-using-fair-distribution)
  (net.jeffhui.check.statem/dump-trace)
  )
