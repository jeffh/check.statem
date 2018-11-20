(ns test.check.statem
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.random :refer [make-random]]
            [clojure.test.check.rose-tree :as rose]))

;; Implementation detail:
;; It is strongly recommended to use the defstatem macro instead to generate
;; conformance to this interface
(defprotocol Command
  ;; listed in order of first-invocation in a test run
  (assume [_ model-state]
    "Used to assert if this command can be generated for the given state. Return
    true if this command can be executed.

    Implementing this provides optimization before generating command data.
    Should be free of side effects.")
  (given [_ model-state]
    "Generates command for the current system state. Should be free of side
    effects.")
  (only-when [_ model-state cmd-data]
    "Used to assert if this command can be executed for the given state. Return
    true if this command can be executed. Should be free of side effects.")
  (advance [_ model-state var-sym cmd-data]
    "Executes the command and returns the next state. Should be free of side
    effects")
  (verify [_ model-state cmd-data return-value] ;; verify
    "Asserts if the command is valid after state application. May cause side
    effects on subject under test (sut). The value of model-state is prior to
    calling advance."))

(declare defcommand)
(defmacro
  ^{:style/indent [1 :form [0 [1]]]
    :arglists '([name doc-string? commands*]
                [name doc-string? [model-state this?] commands*])}
  defstatem
  "Declares a state machine for building test programs.

  This macro provides syntactic sugar to creating a map that represents a state
  machine with Commands.

   - all methods imply model-state and this via the 4th argument
   - generate's body will wrap (gen/tuple (gen/return command-name-kw) ...)
   - all methods have default implementations if not specified:
     - `assume` returns true
     - `given` returns nil. AKA: (gen/tuple (gen/return command-name-kw))
     - `only-when` calls through to `assume`
     - `advance` returns model-state it was given
     - `verify` returns true

  Example:

    (defstatem Queue
      \"A basic queue state machine\"
      [mstate]
      (:enqueue (given [] gen/any-printable)
                (then [[_ item]] ((fnil conj []) mstate item))
                (verify [sut [_ item]] (.add sut item)))
      (:dequeue (assume [] (pos? (count mstate)))
                (advance [_] (subvec mstate 1))
                (verify [sut _] (= (.dequeue sut) (first mstate)))))

  "
  [table-name & commands]
  (let [[docstring & commands] (if (string? (first commands))
                                 commands
                                 (into [nil] commands))
        [global-bindings & commands :as cmd-list] commands]
    (when (seq? cmd-list)
      (assert (vector? global-bindings)))
    (assert (or (nil? docstring) (string? docstring)))
    `(do
       (def ~table-name
         ~@(when docstring [docstring])
         {:name     ~(name table-name)
          :commands {}})
       ~@(map (fn [c] `(defcommand ~table-name ~(first c) ~global-bindings ~@(rest c))) commands)
       (var ~table-name))))

(defn statem-command [statem command-name]
  (or (get (:commands statem) command-name)
      (throw (IllegalArgumentException. (format "Failed to find command (%s) for state machine (%s)"
                                                (pr-str command-name)
                                                (pr-str (:name statem)))))))

(defmacro
  ^{:style/indent [3 :form :form [1]]}
  defcommand
  "Provides an simplified way to define commands for the statem.

  You probably want to use defstatem instead of this macro directly.

  Simply is sugar for (alter-var-root state-machine assoc-in ... (reify Command
  ...)) to save typing and some boilerplate in the following ways:

   - all methods imply model-state and this via the 4th argument
   - given's body will wrap (gen/tuple (gen/return command-name-kw) ...)
   - all methods have default implementations if not specified:
     - `assume` returns true
     - `only-when` calls through to `assume`
     - `advance` returns model-state it was given
     - `given` returns nil. AKA: (gen/tuple (gen/return command-name-kw))
     - `verify` returns true

  Example:

    (defstatem set-statem)
    (defcommand set-statem :add [mstate this]
      (given [] gen/any-printable)
      (advance [[_ value]] (conj (set mstate) value)))
  "
  [table-name cmd-name [model-state this :as shared-bindings] & methods]
  (assert (vector? shared-bindings))
  (assert (every? seq? methods))
  (let [allowed-methods      '#{assume only-when advance given verify}
        unrecognized-methods (set/difference (set (map first methods))
                                             allowed-methods)]
    (assert (empty? unrecognized-methods)
            (format "Expected to only have the following methods defined (%s), but have (%s)"
                    (string/join ", " (map pr-str allowed-methods))
                    (string/join ", " (map pr-str unrecognized-methods)))))
  (assert (set (map first methods)))
  (let [this          (or this (gensym "this__"))
        mstate        (gensym "mstate__")
        cmd           (gensym "cmd__")
        value         (gensym "value__")
        default-impls {'assume    `(assume [] true)
                       'only-when `(only-when [cmd-name#] (assume ~this ~mstate))
                       'advance   `(advance [v# cmd-name#] ~model-state)
                       'given     `(given [] nil)
                       'verify    `(verify [cmd-name# return-value#] true)}
        impls         (merge default-impls
                             (into {} (map (juxt first identity) methods)))
        fill-impl     (fn [bindings sym]
                        (let [body   (impls sym)
                              inputs (second body)
                              b      (gensym "b__")]
                          `(let [~b           ~bindings
                                 ~model-state (first ~b)
                                 ~inputs      (vec (rest ~b))]
                             ~@(rest body))))]
    `(let [cn# ~cmd-name]
       (alter-var-root (var ~table-name)
                       assoc-in [:commands cn#]
                       (reify
                         Command
                         (assume [~this ~mstate]
                           ~(fill-impl [mstate] 'assume))
                         (only-when [~this ~mstate ~cmd]
                           ~(fill-impl [mstate cmd] 'only-when))
                         (given [~this ~mstate]
                           (let [~model-state ~mstate
                                 generators#  (do ~@(rest (impls 'given)))
                                 generators#  (if (or (nil? generators#) (seq? generators#) (vector? generators#))
                                                generators#
                                                [generators#])]
                             (apply gen/tuple (gen/return cn#) generators#)))
                         (advance [~this ~mstate ~value ~cmd]
                           ~(fill-impl [mstate value cmd] 'advance))
                         (verify [~this ~mstate ~cmd ~value]
                           ~(fill-impl [mstate cmd value] 'verify)))))))

(defn- vec-drop-at [coll n]
  ;; coll must be a vector
  (let [before (subvec coll 0 n)
        after (subvec coll (inc n) (count coll))]
    (into before after)))

(defn- drop-seq-permutations [s]
  (map (partial vec-drop-at s) (range 0 (count s))))

(defn- varsym [i]
  (symbol (str "var-" i)))

(defn- shrink-valid-cmd-sequence? [initial-state statem cmds allowed-indicies]
  ;; cmds must be a vector
  (loop [state            initial-state
         allowed-indicies allowed-indicies
         varindex         1]
    (if (pos? (count allowed-indicies))
      (let [[kind :as cmd] (last (cmds (first allowed-indicies)))
            c              (statem-command statem kind)]
        (if (only-when c state cmd)
          (recur (advance c state (varsym varindex) cmd)
                 (rest allowed-indicies)
                 (inc varindex))
          false))
      true)))

(defn- shrink-commands* [indicies-to-keep initial-state statem cmds]
  ;; indicies-to-keep must be vector
  ;; cmds must be a vector
  (when (pos? (count indicies-to-keep))
    (rose/make-rose
     (mapv cmds indicies-to-keep)
     (sequence ;; can reach OOM to realize this value non-lazily
      (comp
       (filter (partial shrink-valid-cmd-sequence? initial-state statem cmds))
       (map #(shrink-commands* % initial-state statem cmds))
       (remove nil?))
      (drop-seq-permutations indicies-to-keep)))))

(defn shrink-commands
  "Creates a rose tree from an initial value of commands generated.

  Chances are, you're looking for usihg cmd-seq"
  [initial-state statem cmds]
  (let [cmds (vec cmds)
        rt (shrink-commands* (vec (range 0 (count cmds)))
                             initial-state
                             statem
                             cmds)]
    (rose/make-rose cmds
                    (rose/children rt)
                    ;; QUESTION: should we include root (which is always empty)
                    ;; incase a bug occurs during initialization of the test
                    ;; case?
                    #_(cons (rose/pure (rose/root rt))
                            (rose/children rt)))))

(defn- assignment-statement [varindex cmd]
  (list 'def (varsym varindex) cmd))

(defn- cmd-state-seq [select-generator state statem size excluded-commands varindex]
  (let [possible-commands (into {}
                                (comp
                                 (filter #(assume (second %) state))
                                 (map #(vector (first %) (given (second %) state))))
                                (apply dissoc (:commands statem) excluded-commands))]
    (if (pos? (count possible-commands))
      (gen/bind (select-generator possible-commands)
                (fn [[kind & data :as cmd]]
                  (if (only-when (statem-command statem kind) state cmd)
                    (if (pos? size)
                      (gen/fmap
                       (partial into [(assignment-statement varindex cmd)])
                       (cmd-state-seq select-generator
                                      (advance (statem-command statem kind) state (varsym varindex) cmd)
                                      statem
                                      (dec size)
                                      #{}
                                      (inc varindex)))
                      (gen/return [(assignment-statement varindex cmd)]))
                    (cmd-state-seq select-generator state statem size (conj excluded-commands kind) (inc varindex)))))
      (gen/return []))))

(defn cmd-seq
  "A generator that produces a random sequence of commands that conform to a
  givens state machine.

  Example:

  (defn queue-interpreter [cmd v var-table])

  (for-all [cmds (cmd-seq queue-statem)]
    (run-cmds queue-statem cmds queue-interpreter))

  For a more thorough example, check out run-cmds.

  "
  ([statem]
   (cmd-seq statem nil))
  ([statem {:keys [select-generator size initial-state]
            :or   {select-generator (comp gen/one-of vals)}
            :as   options}]
   (if size
     (gen/shrink-2
      (gen/bind
       (cmd-state-seq select-generator initial-state statem size #{} 1)
       (fn [cmds]
         (gen/gen-pure (shrink-commands initial-state statem cmds)))))
     (gen/bind (gen/sized #(gen/choose 0 %))
               #(cmd-seq statem (assoc options :size %))))))

(defn valid-cmd-seq?
  "Returns true if a sequence of commands conforms to a state machine's requirements.

  Example:

    ;; for all of correct definitions of `queue-statem`, this should always pass
    (for-all [cmds (cmd-seq queue-statem)]
             (valid-cmd-seq? queue-statem cmd))
  "
  [statem cmds]
  (loop [rem-cmds  cmds
         mstate    nil]
    (if (pos? (count rem-cmds))
      (let [[_ v [kind :as cmd]] (first rem-cmds)
            c                    (statem-command statem kind)
            next-mstate          (advance c mstate v cmd)]
        (if (assume c mstate)
          (recur (rest rem-cmds)
                 next-mstate)
          false))
      true)))

(defn run-cmds
  "Executes the symbolic representation of a sequence of commands using an interpreter."
  [statem cmds interpreter]
  (loop [rem-cmds  cmds
         mstate    nil
         var-table {}]
    (if (pos? (count rem-cmds))
      (let [[_ v [kind :as cmd]] (first rem-cmds)
            c                    (statem-command statem kind)
            next-mstate          (advance c mstate v cmd)
            return-value         (interpreter cmd v var-table)]
        (if (verify c mstate cmd return-value)
          (recur (rest rem-cmds)
                 next-mstate
                 (if (nil? return-value)
                   var-table
                   (assoc var-table v return-value)))
          {:ok?          false
           :cmds         cmds
           :statem       statem
           :vars         var-table
           :model-state  mstate
           :return-value return-value
           :cmd          cmd}))
      {:ok? true})))

;; TODO: report this bug. This fails to compile in a weird unreadable way in
;; cider because: mstate isn't defined
;;
;; Notes:
;;  - this looks like cider middleware fault
;;  - clojure repl reports a sane error message
;;
;; (defn test-example []
;;   (loop [a nil]
;;     (when a
;;       (if false
;;         (recur nil)
;;         {:key mstate}))))

