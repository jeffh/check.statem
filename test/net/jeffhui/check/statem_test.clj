(ns net.jeffhui.check.statem-test
  (:require [clojure.test :as clojure-test :refer [testing deftest is]]
            [net.jeffhui.check.statem :as statem :refer [defstatem cmd-seq run-cmds run-cmds-debug select-by-frequency check!]]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose-tree]
            [clojure.walk :as walk]
            [clojure.test.check.results :as results]))

(defn- generate-rt
  ([generator]
   (generate-rt generator 30))
  ([generator size]
   (let [rng (random/make-random)]
     (gen/call-gen generator rng size)))
  ([generator size seed]
   (let [rng (random/make-random seed)]
     (gen/call-gen generator rng size))))

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
          (verify [prev-mstate _ r] (= r (first (:items prev-mstate))))
          (verify-debug [prev-mstate _ r] {:expected (first (:items prev-mstate))})))

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

(deftest reading-commands
  (is (statem/lookup-command queue-statem :new))
  (is (statem/lookup-command queue-statem :enqueue))
  (is (statem/lookup-command queue-statem :deque))
  (is (= #{:new :enqueue :deque} (set (statem/list-commands queue-statem)))))

(deftest valid-cmd-seq-rejects-invalid-specific-sequences
  (is (false? (statem/valid-cmd-seq? queue-statem [[:set [:var 1] [:new]]
                                                   [:set [:var 2] [:new]]])))

  (is (false? (statem/valid-cmd-seq? queue-statem [[:set [:var 1] [:new]]
                                                   [:set [:var 2] [:enqueue 1]]
                                                   [:set [:var 3] [:new]]]))))


(defn- random-commands
  "A bad implementation of cmd-seq, do not try at home."
  []
  (let [statem queue-statem]
    (gen/such-that
     ;; single commands can potentially be valid queue-statem
     #(not= :new (first (last (first %))))
     (gen/not-empty
      (gen/vector
       (gen/one-of
        (into
         []
         (map-indexed
          (fn [i cmd-name]
            (let [c (statem/lookup-command statem cmd-name)]
              (gen/fmap
               (fn [cmd-data]
                 [:set [:var i] cmd-data])
               (statem/args c nil)))))
         (statem/list-commands statem))))))))

(defspec valid-cmd-seq-rejects-invalid-sequences 100
  (for-all [cmds (random-commands)]
           (not (statem/valid-cmd-seq? queue-statem cmds))))

(defspec generation-always-conforms-to-statem 100
  (for-all [cmds (cmd-seq queue-statem)]
           (statem/valid-cmd-seq? queue-statem cmds)))

(deftest always-fails-if-at-least-one-failure-occurs
  (let [state (atom 0)]
    (is (every? false?
                (map :result
                     (rose-tree/seq
                      (generate-rt
                       (for-all [i gen/int]
                                (statem/always
                                 (pos? (mod (swap! state inc) 5)))))))))))


(comment
  (clojure-test/test-var #'shrinking-always-conforms-to-statem)

  (send net.jeffhui.check.statem.internal/stats (constantly nil))
  @net.jeffhui.check.statem.internal/stats
  (net.jeffhui.check.statem.internal/dump-trace)

  (double (/ 2884093872 1000000000))

  )


(deftest sometimes-fails-if-all-runs-fails
  (let [state (atom 0)]
    (is (every? false?
                (map :result
                     (rose-tree/seq
                      (generate-rt
                       (for-all [i gen/int]
                                (statem/sometimes
                                 false))))))))
  (let [state (atom 0)]
    (is (every? true?
                (map :result
                     (rose-tree/seq
                      (generate-rt
                       (for-all [i gen/int]
                                (statem/sometimes
                                 (zero? (mod (swap! state inc) 5)))))))))))

#_
(doseq [r (rose-tree/seq (generate-rt (cmd-seq queue-statem) 5))]
  (clojure.pprint/pprint r))



(deftest shrinking-always-conforms-to-statem
  (dotimes [i 100]
    (let [cmds-gen (cmd-seq queue-statem)]
      ;; fully walking a tree never happens for large generations, and is
      ;; impractical to fully traverse (without using lots of memory).
      (let [rt              (generate-rt cmds-gen (mod i 5))
            original-size   (count (rose-tree/root rt))
            shrinking-sizes (set (map count (rose-tree/seq rt)))]
        (is (= shrinking-sizes (set (range 0 (inc original-size)))))
        (is (every? (partial statem/valid-cmd-seq? queue-statem)
                    (rose-tree/seq rt)))))))

(defspec queue-program-generation-using-fair-distribution 100
  (for-all [cmds (cmd-seq queue-statem)]
           (statem/print-failed-runs!
            (run-cmds queue-statem cmds queue-runner))))

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
              (run-cmds queue-statem cmds queue-runner)))))

(defn- macroexpand-thrown? [form]
  (try
    (walk/macroexpand-all form)
    false
    (catch Throwable t
      (instance? IllegalArgumentException (.getCause t)))))

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
   (gen/generate (cmd-seq queue-statem {:size 3})))

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

(defstatem key-value-statem ;; name of the state machine
  [mstate] ;; the internal model state -- starts as nil
  ;; define transitions
  (:put ;; name of the transition
   (args [] [statem/fast-keyword #_gen/keyword (gen/scale #(int (/ % 10)) gen/int)]) ;; associated data generators for this transition
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

;; bad version
#_
(defn kv-interpreter []
  (let [state (atom [])]
    (fn interpreter [cmd ctx]
      (case (first cmd)
        :put (swap! state conj (vec (rest cmd)))
        :get (second (first (filter (comp (partial = (second cmd)) first) @state)))))))

;; good version
(defn kv-interpreter []
  (let [state (atom [])]
    (fn interpreter [cmd ctx]
      (case (first cmd)
        :put (swap! state conj (vec (rest cmd)))
        :get (second (last (filter (comp (partial = (second cmd)) first) @state)))))))

(defspec kv-spec 400
  (for-all
   [cmds (cmd-seq key-value-statem)]
   (statem/print-failed-runs!
    (run-cmds key-value-statem cmds (kv-interpreter)))))

#_
(take 10
      (rose-tree/seq
       (let [rng (clojure.test.check.random/make-random)]
         (rose-tree/zip vector [(gen/call-gen gen/keyword rng 30) (gen/call-gen gen/keyword rng 30)]))))

#_
(def rt
  (let [rng (clojure.test.check.random/make-random)]
    (gen/call-gen (cmd-seq key-value-statem) rng 30)))

#_(rose-tree/root rt)
#_(take 10 (rose-tree/seq rt))

(deftest empty-cmds
  (is (empty? (first (:smallest (:shrunk (tc/quick-check 5 (for-all [cmds (cmd-seq key-value-statem)]
                                                                    false))))))))

#_(net.jeffhui.check.statem.internal/dump-trace)

(def hashmap-statem
  (let [test-keys ["" "a" "house" "tree" "λ"]]
    (statem/->statem
     {:name     "HashmapStateMachine"
      :commands {:put {:args       (fn [state] [(gen/elements test-keys) gen/int])
                       :next-state (fn [state [_ k v] _] (assoc state k v))}
                 :get {:requires      (fn [state] (seq state))
                       :args          (fn [state] [(gen/elements test-keys)])
                       :postcondition (fn [prev-state _ [_ k] val]
                                        (= (get prev-state k) val))}}})))

(defn adder-interpreter []
  (fn [cmd {:keys [var-table]}]
    (case (first cmd)
      :add (apply + (map (fn [x] (if (statem/varsym? x)
                                   (var-table x)
                                   x))
                         (rest cmd))))))

(defstatem adder-statem
  [mstate]
  (:add (args []
          (let [g (if-let [vars (not-empty (keys (:vars mstate)))]
                    (gen/one-of [gen/int (gen/elements (vec vars))])
                    gen/int)]
            [g g g]))
        (advance [v [_ & xs]]
          (let [out (apply + (map (fn [x]
                                    (if (statem/varsym? x)
                                      (get-in mstate [:vars x])
                                      x))
                                  xs))]
            (-> mstate
                (assoc :latest out)
                (assoc-in [:vars v] out))))
        (verify [prev-mstate _ r]
          (= r (:latest mstate)))))

(defspec sequential-adding
  (for-all [cmds (cmd-seq adder-statem)]
           (statem/print-failed-runs!
            (run-cmds adder-statem cmds (adder-interpreter)))))

