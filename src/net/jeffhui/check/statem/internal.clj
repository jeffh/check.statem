(ns ^:no-doc net.jeffhui.check.statem.internal
  (:require [clojure.pprint :as pprint]
            [clojure.inspector :as inspector]))

;; private use only
(if #_trace= false
  (do
    (def stats (agent {}))
    (defmacro ^:no-doc ^{:style/indent 1} trace [name & body]
      `(let [n# ~name
             s# (System/nanoTime)
             r# (do ~@body)
             e# (System/nanoTime)]
         (send stats update n# (fnil conj []) (- e# s#))
         r#))

    (defn dump-trace
      "Internal use, do not use."
      []
      (let [st    @stats
            s     (into []
                        (map (fn [[k vs]]
                               (let [times vs
                                     s     (reduce + 0 times)]
                                 {:name  k
                                  :min   (apply min times)
                                  :max   (apply max times)
                                  :sum   s
                                  :count (count times)
                                  :avg   (int (double (/ s (count vs))))}))
                             st))
            total (reduce (fn [acc [_ vs]]
                            (let [s (reduce + 0 vs)]
                              (merge-with +
                                          acc
                                          {:sum   s
                                           :count (count vs)})))
                          {:name  :total
                           :min   nil
                           :max   nil
                           :sum   0
                           :count 0
                           :avg   nil}
                          st)
            s     (conj s total)]
        (pprint/print-table (reverse (sort-by :sum s))))))

  (defmacro ^{:style/indent 1} trace [name & body]
    `(do ~@body)))


(defprotocol CmdRunTracer
  (run-start [_ cmds initial-mstate])
  (run-step [_ stmt mstate next-mstate var-table])
  (run-return [_ stmt prev-mstate mstate var-table return-value valid?])
  (run-end [_ result]))

(defrecord CmdRunPrinter [print-statem? print-return-values?]
  CmdRunTracer
  (run-start [_ cmds initial-mstate]
    (println (format "┌ commands (%d)" (count cmds)))
    (when print-statem?
      (println "│   └ mstate:" (pr-str initial-mstate))))
  (run-step [_ stmt mstate next-mstate var-table]
    (println "│" (pr-str stmt))
    (when print-statem?
      (println "│  "
               (if print-return-values?
                 "│"
                 "└")
               "mstate:" (pr-str next-mstate))))
  (run-return [_ stmt prev-mstate mstate var-table return-value valid?]
    (when print-return-values?
      (println "│   └ returned:" (pr-str return-value))))
  (run-end [_ result]
    (println "└" (if (:pass? result)
                   "OK"
                   "FAILED"))))

(defrecord CmdRunAbridgedPrinter [print-return-values?]
  CmdRunTracer
  (run-start [_ cmds initial-mstate]
    (println (format "┌ commands (%d)" (count cmds))))
  (run-step [_ stmt mstate next-mstate var-table]
    (print "│" (pr-str stmt))
    (flush))
  (run-return [_ stmt prev-mstate mstate var-table return-value valid?]
    (if print-return-values?
      (println " ->" (if valid? "ok" "failed") (pr-str return-value))
      (println " ->" (if valid? "ok" "failed"))))
  (run-end [_ result]
    (println "└" (if (:pass? result)
                   "OK"
                   "FAILED"))))

(defrecord CmdRunInspector [display-statem? display-return-values? inspect-type accum]
  CmdRunTracer
  (run-start [_ cmds initial-mstate]
    (reset! accum [(merge
                    {:cmd       nil
                     :var-table nil}
                    (when display-statem?
                      {:mstate initial-mstate}))]))
  (run-step [_ stmt mstate next-mstate var-table])
  (run-return [_ stmt prev-mstate mstate var-table return-value valid?]
    (swap! accum conj (merge
                       {:cmd       stmt
                        :valid?    valid?
                        :var-table var-table}
                       (when display-statem?
                         {:mstate mstate})
                       (when display-return-values?
                         {:return-value return-value}))))
  (run-end [_ result]
    (case inspect-type
      :inspect-table (inspector/inspect-table @accum)
      :inspect-tree  (inspector/inspect-tree @accum)
      :inspect       (inspector/inspect @accum))))

