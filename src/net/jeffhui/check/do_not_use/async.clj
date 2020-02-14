(ns ^:no-doc net.jeffhui.check.do-not-use.async
  "100% experimental. Expect nothing will be here tomorrow.

  Provides asynchronous test running."
  (:require [net.jeffhui.check.statem :as statem]
            [net.jeffhui.check.statem.internal :as internal :refer [trace]]
            [clojure.test.check.results :as results]))

(defn- error? [e] ;; TODO: move to internal and remove duplicates
  (boolean (::fail-fast (ex-data e))))

(defn- possible-conjectured-commands [statem state excluded-commands]
  (into []
        (filter #(let [c (second %)]
                   (and (= :conjectured (statem/kind c))
                        (statem/assume c state))))
        (apply dissoc (:commands statem) excluded-commands)))

(defn- into! [transient-to from]
  (reduce conj! transient-to from))

(defn run-cmds
  "Identical to `statem/run-cmds`, but speculatively traverses :conjectured
  commands to evaluated value states.


  In cmds, only the observed commands will be called, all conjectured commands
  are ignored.
  "
  ([^StateMachine statem cmds interpreter] (run-cmds statem cmds interpreter nil))
  ([^StateMachine statem cmds interpreter {:keys [initial-state catch? excluded-commands]
                                           :or   {catch?            true
                                                  excluded-commands #{}}}]
   (trace 'run-cmds
     (assert (and (seqable? cmds)
                  (or (zero? (count cmds))
                      (and (vector? (first cmds))
                           (keyword (ffirst cmds)))))
             "Invalid commands. Did you mean to remove one level of nesting from test results?")
     (let [interpreter (if catch?
                         (statem/catch-print-interpreter interpreter)
                         interpreter)
           result      (loop [rem-cmds  cmds
                              mstate    initial-state
                              var-table {}
                              history   (transient [(statem/->HistoryEntry true initial-state [:initial-state]
                                                                           nil {} ::no-debug)])]
                         (if (pos? (count rem-cmds))
                           (let [[_ v [kind :as cmd]] (rem-cmds 0)
                                 c                    (statem/lookup-command statem kind)]
                             (case (statem/kind c)
                               :observable ;; SUT can be interacted with this command
                               (let [next-mstate   (statem/advance c mstate v cmd)
                                     return-value  (statem/interpreter cmd {:var-table var-table})
                                     verify-result (and (not (error? return-value))
                                                        (statem/verify c next-mstate mstate cmd return-value))]
                                 (if (results/pass? verify-result)
                                   (recur (subvec rem-cmds 1)
                                          next-mstate
                                          (if (nil? return-value)
                                            var-table
                                            (assoc var-table v return-value))
                                          (conj! history (statem/->HistoryEntry verify-result next-mstate cmd return-value var-table
                                                                                (cond (statem/error? return-value)            ::exception
                                                                                      (satisfies? statem/DebuggableCommand c) (statem/verify-debug c next-mstate mstate cmd return-value)
                                                                                      :else                                   ::no-debug))))
                                   ;; we may need to walk through conjectured states
                                   ;; TODO: should we recursively walk, or is one depth good enough?
                                   (let [extra-history
                                         (loop [possible-cmds (possible-conjectured-commands statem mstate excluded-commands)]
                                           (let [[p-cmd-name p-cmd] (possible-cmds 0)
                                                 interim-v          [p-cmd-name]
                                                 interim-statem     (statem/advance p-cmd mstate nil interim-v)

                                                 next-mstate'  (statem/advance c next-mstate v cmd)
                                                 verify-result (and (not (error? return-value))
                                                                    (statem/verify c next-mstate mstate cmd return-value))]
                                             (cond
                                               (results/pass? verify-result) ;; found a valid async path
                                               [(statem/->HistoryEntry verify-result next-mstate cmd
                                                                       return-value var-table
                                                                       (cond (error? return-value)                   ::exception
                                                                             (satisfies? statem/DebuggableCommand c) (statem/verify-debug c next-mstate mstate cmd return-value)
                                                                             :else                                   ::no-debug))
                                                (statem/->HistoryEntry verify-result next-mstate' cmd return-value var-table ::no-debug)]

                                               (empty? possible-cmds) ;; expired all possibilities
                                               nil

                                               :else
                                               (recur (subvec possible-cmds 1)))))]
                                     (if (nil? extra-history)
                                       (statem/->ExecutionResult false cmds
                                                                 (persistent! (conj! history
                                                                                     (statem/->HistoryEntry verify-result next-mstate cmd
                                                                                                            return-value var-table
                                                                                                            (cond (error? return-value)                   ::exception
                                                                                                                  (satisfies? statem/DebuggableCommand c) (statem/verify-debug c next-mstate mstate cmd return-value)
                                                                                                                  :else                                   ::no-debug)))))
                                       (let [last-entry   (extra-history (dec (count extra-history)))
                                             return-value (:return-value last-entry)]
                                         (recur (subvec rem-cmds 1)
                                                (:mstate last-entry)
                                                (if (nil? return-value)
                                                  var-table
                                                  (assoc var-table v return-value))
                                                (into! history extra-history)))))))

                               :conjectured ;; never executed against SUT, just useful for state modeling
                               (recur (subvec rem-cmds 1) mstate var-table history)))
                           (statem/->ExecutionResult true cmds (persistent! history))))
           cleanup (:check.statem/cleanup (meta interpreter))]
       (when cleanup
         (cleanup result))
       result))))
