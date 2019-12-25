(ns ^:no-doc net.jeffhui.check.statem.internal
  (:require [clojure.pprint :as pprint]))

;; private use only
(if #_trace= false
  (do
    (def ^:private stats (agent {}))
    (defmacro ^:no-doc ^{:style/indent 1} trace [name & body]
      `(let [n# ~name
             s# (System/nanoTime)
             r# (do ~@body)
             e# (System/nanoTime)]
         (send stats update n# (fnil conj []) (- e# s#))
         r#))

    (defn dump-trace
      "Internal use, do not use"
      []
      (let [s (into []
                    (map (fn [[k vs]]
                           (let [times vs
                                 s     (reduce + 0 times)]
                             {:name  k
                              :min   (apply min times)
                              :max   (apply max times)
                              :sum   s
                              :count (count times)
                              :avg   (int (double (/ s (count vs))))}))
                         @stats))]
        (pprint/print-table (reverse (sort-by :sum s))))))

  (defmacro ^{:style/indent 1} trace [name & body]
    `(do ~@body)))
