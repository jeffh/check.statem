(ns test.check.statem
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]
            [clojure.test.check.random :as random :refer [make-random]]
            [clojure.test.check.rose-tree :as rose]
            [clojure.walk :as walk]))

;; private use only
(if #_trace= false
  (do
    (def ^:private stats (atom {}))
    (def ^:private ^:dynamic *trace-stack* [])

    (defmacro ^{:style/indent 1} trace [name & body]
      `(let [n# ~name
             s# (System/nanoTime)
             r# (do ~@body)
             e# (System/nanoTime)]
         (swap! stats update n# (fnil conj []) (- e# s#))
         r#))

    (defn dump-trace []
      (let [s (into []
                    (map (fn [[k vs]]
                           (let [times vs
                                 s     (reduce + 0 times)]
                             {:name k
                              :min  (apply min times)
                              :max  (apply max times)
                              :sum  s
                              :avg  (double (/ s (count vs)))}))
                         @stats))]
        (pprint/print-table (reverse (sort-by :sum s))))))

  (defmacro ^{:style/indent 1} trace [name & body]
    `(do ~@body)))

;; TODO:
;; [x] validate defcommand arg length
;; [ ] validate defcommand generator arg length
;; [~] shrinking - how to handle dropped vars?
;; [ ] help - test for generator errors?

;; Implementation detail:
;; It is strongly recommended to use the defstatem macro instead to generate
;; conformance to this interface
(defprotocol Command
  "Command interface. It's recommended to use `defstatem` and/or `defcommand`instead.

  Command represents an 'action' or transition that can be taken by a state
  machine. These transitions requires several bits of metadata:

   1. When is calling this command valid?
   2. What data does this command require besides the current state?
   3. How does the state machine's state change invoking this command?
   4. How do we validate this command's behavior to another implementation?

  The following functions map to the following questions:

   - `assume` and `only-when` answers #1, with the first being an optimization
     of the latter.
   - `given` answers #2
   - `advance` answers #3
   - `verify` answers #4

  A useful hint if you're implementing these functions from `defstatem` is that
  the first two arguments (`_` and `model-state`) are dropped for brevity.
  "
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
  (verify [_ model-state next-model-state cmd-data return-value] ;; verify
    "Asserts if the command is valid after state application. May cause side
    effects on subject under test (sut). The value of model-state is prior to
    calling advance."))

(defrecord StateMachine [name commands cmd-metadatas])

(declare defcommand)
(defmacro
  ^{:style/indent [1 :form [0 [1]]]
    :arglists     '([name doc-string? commands*]
                    [name doc-string? [model-state this?] commands*])}
  defstatem
  "Declares a state machine for building test verification programs.

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

  If you have a large state machine, you can break the state machine definition
  into several forms by using `defcommand` directly. Although, it may be more
  indicative of a bigger problem if your state machine has a lot of commands
  (since test.check may not be able to generate a large part of the state
  machine's program space on any given run.)

  Example:

    (defstatem Queue
      \"A basic queue state machine\"
      [mstate]
      (:enqueue (given [] gen/any-printable)
                (advance [[_ item]] ((fnil conj []) mstate item))
                (verify [_ next-mstate [_ item] ret] (.add sut item)))
      (:dequeue (assume [] (pos? (count mstate)))
                (advance [_] (subvec mstate 1))
                (verify [_ _ _ ret] (= (.dequeue sut) (first mstate)))))

  Notes:

    State machine definitions are entirely abstract - meaning there is no
    direct dependency on how any particular implementation that a production
    implementation may have. To perform that comparison, use a function like
    `run-cmds` with some integration code. This allows state machine definitions
    to be shared or reused against other production implementations.

  Large State Machines:

    If you have a large state machine, it may be better to break it up into
    multiple smaller ones to test. Smaller state machines allow test.check to
    generate more of the possible program space within a typical test generation
    (eg - 100).

    Alternatively, you can choose to generate commands with a skewed probability
    of generating specific events. It's probably not as good of a solution to
    breaking up the state machine, but can provide a more focused exploration of
    specific kind of program generations.

  "
  [table-name & commands]
  (let [[docstring & commands]                    (if (string? (first commands))
                                                    commands
                                                    (into [nil] commands))
        [global-bindings & commands :as cmd-list] commands]
    (when (seq? cmd-list)
      (assert (vector? global-bindings)))
    (assert (or (nil? docstring) (string? docstring)))
    `(do
       (def ~table-name
         ~@(when docstring [docstring])
         (map->StateMachine
          {:name          ~(name table-name)
           :commands      {}
           :cmd-metadatas {}}))
       ~@(map (fn [c] `(defcommand ~table-name ~(first c) ~global-bindings ~@(rest c))) commands)
       (var ~table-name))))

(defn statem-command [^StateMachine statem command-name]
  (trace 'statem-command
    (or (get (.commands statem) command-name)
        (throw (IllegalArgumentException. (format "Failed to find command (%s) for state machine (%s)"
                                                  (pr-str command-name)
                                                  (pr-str (.name statem))))))))

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
        next-mstate   (gensym "next-mstate__")
        cmd           (gensym "cmd__")
        value         (gensym "value__")
        default-impls {'assume    `(assume [] true)
                       'only-when `(only-when [cmd-name#] (assume ~this ~mstate))
                       'advance   `(advance [v# cmd-name#] ~model-state)
                       'given     `(given [] nil)
                       'verify    `(verify [next-mstate# cmd-name# return-value#] true)}
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
    (doseq [[n impl] default-impls
            :let [expected-size (count (second impl))
                  actual-size (count (second (impls n)))]]
      (assert (= expected-size actual-size)
              (format "Expected command [%s %s %s] implementation to have %d input parameters, got %d."
                      table-name
                      cmd-name
                      (name n)
                      expected-size
                      actual-size)))
    `(let [cn# ~cmd-name]
       (alter-var-root (var ~table-name)
                       assoc-in [:cmd-metadatas cn#]
                       (quote ~impls))
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
                         (verify [~this ~mstate ~next-mstate ~cmd ~value]
                           ~(fill-impl [mstate next-mstate cmd value] 'verify)))))))

(defn- vec-drop-at [coll n]
  (trace 'vec-drop-at
    ;; coll must be a vector
    (let [before (subvec coll 0 n)
          after (subvec coll (inc n) (count coll))]
      (into before after))))

;; varsym used to be (symbol (str "var-" 1))
;; but using a tuple (aka vector) is much faster to operate with instead of having to parse
(defn- varsym? [v]
  (and (vector? v)
       (= :statem/var (first v))))
(defn- varsym [i]
  (trace 'varsym
    [:statem/var i]
    #_
    (symbol (str "var-" i))))
(defn- varsym-offset [var]
  (trace 'varsym-offset
    (when (varsym? var)
      (second var))
    #_
    (try
      (Integer/parseInt
       (.substring (name var) (count "var-")))
      (catch NumberFormatException e nil))))


(defn- shrink-valid-cmd-sequence? [initial-state statem cmds allowed-indicies]
  (trace 'shrink-valid-cmd-sequence?
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
        true))))

(defn- realize-cmds
  [statem cmds indicies-to-keep]
  (trace 'realize-cmds
    ;; NOTE: this code is on the hot-path. Small inefficiencies have outsized impact here
    (let [vars (into #{}
                     (map (comp varsym inc))
                     indicies-to-keep)]
      (persistent!
       (reduce
        (fn [stmts i]
          (let [stmt (cmds i)]
            (conj! stmts [(stmt 0)
                          (stmt 1)
                          (mapv (fn [f]
                                  (trace 'realize-cmds--reducer--postwalker
                                    (if-let [idx (varsym-offset f)]
                                      (loop [f   f
                                             idx idx]
                                        (cond (contains? vars f) f
                                              (= 1 idx) nil
                                              :else (recur (varsym (dec idx)) (dec idx))))
                                      f)))
                                (stmt 2))
                          #_(walk/postwalk
                             (fn [f]
                               (trace 'realize-cmds--reducer--postwalker
                                 (if-let [idx (varsym-offset f)]
                                   (loop [f   f
                                          idx idx]
                                     (cond (contains? vars f) f
                                           (= 1 idx) nil
                                           :else (recur (varsym (dec idx)) (dec idx))))
                                   f)))
                             c)])))
        (transient [])
        indicies-to-keep)))))

(defn- shrink-commands* [indicies-to-keep initial-state statem cmds]
  ;; NOTE: on the hot path. Small inefficiencies compound dramatically here
  (trace 'shrink-commands*
    ;; indicies-to-keep must be vector
    ;; cmds must be a vector
    (when (pos? (count indicies-to-keep))
      (rose/make-rose
       (realize-cmds statem cmds indicies-to-keep)
       (sequence ;; can reach OOM to realize this value non-lazily
        (comp
         (map (partial vec-drop-at indicies-to-keep))
         (filter (partial shrink-valid-cmd-sequence? initial-state statem cmds))
         (keep #(shrink-commands* % initial-state statem cmds)))
        (range (count indicies-to-keep)))))))

(defn shrink-commands
  "Creates a rose tree from an initial value of commands generated.

  Chances are, you're looking for usihg cmd-seq"
  [initial-state statem cmds]
  (trace 'shrink-commands
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
                              (rose/children rt))))))

(defn- assignment-statement [varindex cmd]
  (trace 'assignment-statement
    [:set (varsym varindex) cmd]))

(defn- cmd-state-seq [select-generator state statem size excluded-commands varindex]
  (trace 'cmd-state-seq
    (let [possible-commands (into {}
                                  (comp
                                   (filter #(assume (second %) state))
                                   (map #(vector (first %) (given (second %) state))))
                                  (apply dissoc (:commands statem) excluded-commands))]
      (if (pos? (count possible-commands))
        (gen/bind (select-generator statem state possible-commands)
                  (fn [[kind & data :as cmd]]
                    (if (nil? cmd)
                      (gen/return [])
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
                        (cmd-state-seq select-generator state statem size (conj excluded-commands kind) (inc varindex))))))
        (gen/return [])))))

(defn select-cmds
  "A helper that simply returns a function that conforms to select-generator param.

  f must accept a map of all valid commands to take. Keys are keywords of the
  commands and values are the command generator for that given command.

  The returned generator must choose to return a command generator or nil to
  indicate the end of the cmd seq. Note that cmd-seq may choose to end a
  sequence if it has reached the size needed to generate.
  "
  [f]
  (trace 'select-cmds
    (fn select-generator [state statem possible-kw->cmd-gen]
      (trace 'select-cmds-select-generator
        (f possible-kw->cmd-gen)))))

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
            :or   {select-generator (select-cmds (comp gen/one-of vals))}
            :as   options}]
   (trace 'cmd-seq
     (if size
       (gen/shrink-2
        (gen/bind
         (cmd-state-seq select-generator initial-state statem size #{} 1)
         (fn [cmds]
           (gen/gen-pure (shrink-commands initial-state statem cmds)))))
       (gen/bind (gen/sized #(gen/choose 0 %))
                 #(cmd-seq statem (assoc options :size %)))))))

(defn valid-cmd-seq?
  "Returns true if a sequence of commands conforms to a state machine's requirements.

  Example:

    ;; for all of correct definitions of `queue-statem`, this should always pass
    (for-all [cmds (cmd-seq queue-statem)]
             (valid-cmd-seq? queue-statem cmd))
  "
  [statem cmds]
  (trace 'valid-cmd-seq?
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
        true))))

(defn run-cmds
  "Executes the symbolic representation of a sequence of commands using an interpreter."
  [statem cmds interpreter]
  (trace 'run-cmds
    (loop [rem-cmds  cmds
           mstate    nil
           var-table {}]
      (if (pos? (count rem-cmds))
        (let [[_ v [kind :as cmd]] (first rem-cmds)
              c                    (statem-command statem kind)
              next-mstate          (advance c mstate v cmd)
              return-value         (interpreter cmd {:var-sym   v
                                                     :var-table var-table})]
          (if (verify c mstate next-mstate cmd return-value)
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
        {:ok? true}))))

(def ^:private ^:dynamic *debug-statem* false)
(def ^:private ^:dynamic *debug-return-values* false)

(defn- debug-start [cmds initial-mstate]
  (println (format "┌ commands (%d)" (count cmds)))
  (when *debug-statem*
    (println "│   └ mstate:" (pr-str initial-mstate))))
(defn- debug-step [stmt mstate next-mstate var-table]
  (println "│" (pr-str stmt))
  (when *debug-statem*
    (println "│  "
             (if *debug-return-values*
               "│"
               "└")
             "mstate:" (pr-str next-mstate))))
(defn- debug-return [return-value]
  (when *debug-return-values*
    (println "│   └ SUT-return:" (pr-str return-value))))
(defn- debug-end [ret]
  (println "└" (if (:ok? ret)
                 "OK"
                 "FAILED"))
  ret)

(defn run-cmds-debug
  "Identical to run-cmds, but prints out each command executed"
  ([statem cmds interpreter]
   (run-cmds-debug nil statem cmds interpreter))
  ([{:as   debug-options
     :keys [mstate?
            return-value?]}
    statem cmds interpreter]
   (binding [*debug-statem* mstate?
             *debug-return-values* return-value?]
     (debug-start cmds nil)
     (debug-end
      (loop [rem-cmds  cmds
             mstate    nil
             var-table {}]
        (if (pos? (count rem-cmds))
          (let [[_ v [kind :as cmd] :as stmt] (first rem-cmds)
                c                             (statem-command statem kind)
                next-mstate                   (advance c mstate v cmd)
                _                             (debug-step stmt mstate next-mstate var-table)
                return-value                  (interpreter cmd {:var-sym   v
                                                                :var-table var-table})
                _                             (debug-return return-value)]
            (if (verify c mstate next-mstate cmd return-value)
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
          {:ok? true}))))))

(defn- denamespace-syms [form]
  (walk/postwalk
   (fn [f]
     (if (symbol? f)
       (symbol (name f))
       f))
   form))

(defn statem-check!
  "Performs validations against state machine. Useful for sanity-checking state
  machine definitions before attempting to use them.

  This will never run verify of state machine commands
  "
  [^StateMachine statem]
  ;; basic checks
  (assert (.name statem) "State Machine is missing a name")

  ;; checks by generating programs
  (let [num-cmds (count (.commands statem))
        programs (gen/sample (cmd-seq statem {:size num-cmds}) (* num-cmds 2))]
    ;; generator destructuring check
    (doseq [stmts programs
            [_ _ [cmd-name :as cmd]] stmts
            :let [cmd-meta (get (.cmd_metadatas statem) cmd-name)
                  generated-cmd-count (count cmd)]]
      (letfn [(check-gen-args [method gen-arg-index]
                (let [cmd-arg (nth (second (cmd-meta method)) gen-arg-index)]
                  (when (vector? cmd-arg)
                    (let [greater-than-expected? (> (count cmd-arg) generated-cmd-count)]
                      (assert (= (count cmd-arg) generated-cmd-count)
                              (format (str "Command [%s %s %s] destructures cmd"
                                           " parameter, but drops some values which is probably"
                                           " not what you intended.\n\n"
                                           "Here's the part of statem definition I'm using (eliding irrelevant parts):\n\n  %s\n\n"
                                           "But when I run this:\n\n  %s\n\n"
                                           (if greater-than-expected?
                                             (str "Then the following input parameters of %s are always nil:\n\n  %s\n\n"
                                                  "You probably just want to delete these input parameters.")
                                             (str "Then %s's input parameter bindings become:\n\n  %s\n\n"
                                                  "You probably need to define more input parameters."))
                                           "\n")
                                      (name (.name statem))
                                      cmd-name
                                      (name method)
                                      (pr-str (denamespace-syms
                                               `(defstatem ~(symbol (.name statem))
                                                  (~(first cmd)
                                                   (~(first (cmd-meta method))
                                                    ~(assoc (vec (repeat (count (second (cmd-meta method))) '_))
                                                            gen-arg-index
                                                            cmd-arg)
                                                    ...))
                                                  ...)))
                                      (pr-str (denamespace-syms `(~method
                                                                  ~(symbol (.name statem))
                                                                  ~@(assoc (vec (repeat (count (second (cmd-meta method))) '_))
                                                                           gen-arg-index
                                                                           cmd))))
                                      (name method)
                                      (if greater-than-expected?
                                        (string/join ", " (map pr-str (drop (count cmd) cmd-arg)))
                                        (with-out-str
                                          (pprint/pprint (zipmap cmd-arg (concat cmd (repeat nil))))))))))))]
        (check-gen-args 'only-when 0)
        (check-gen-args 'advance 1)
        (check-gen-args 'verify 1)))

    ;; check command coverage
    (let [exercised-cmds (->> programs
                              (map (fn [stmts] (set (map (comp first last) stmts))))
                              (apply concat [])
                              set)
          all-cmds (set (keys (.commands statem)))
          diff (set/difference all-cmds exercised-cmds)]
      (assert (empty? diff)
              (str "Expected to generate all commands, but didn't. Commands that didn't get generated: "
                   (pr-str diff))))))

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

