(ns net.jeffhui.check.statem
  "Facilities for testing stateful systems using state machines.

  Allows defining specially annotated state machines that can be used as the
  basis for generating test verification programs to validate stateful
  specifications.

  Test verification programs are defined as a sequence of transitions that
  conform to a state machine.

  API OVERVIEW

    The goal for state machine testing is to define the following:

     - A 'model' state machine (using [[defstatem]]). The model state machine
       defines the expected state machine behavior.
     - A interpreter function (adapter between [[defstatem]] and an
       implementation). This function reads model state transitions and performs the
       appropriate behavior in an implementation.

    The most important functions you need to understand are:

     - [[defstatem]] for defining state machines.
     - [[cmd-seq]] for generating symbolic test programs from a state machine.
     - [[run-cmds]] for executing a symbolic test program against code that
                    needs to be verified. This is to compare the state machine
                    behavior to a real implementation.

    There are some useful helper functions that aid in building & debugging
    state machines:

     - [[check!]] run some sanity checks against the state machine definition.
     - [[run-cmds-debug]] is a verbose printout version of [[run-cmds]].
     - [[select-by-frequency]] allows you to skew how [[cmd-seq]] generates commands.
     - [[print-failed-runs!]] prints commands if a particular [[run-cmds]] call fails.
  "
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pprint]
            [clojure.test.check.results :as results]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]
            [clojure.walk :as walk]
            [clojure.math.combinatorics :as combo]
            [net.jeffhui.check.statem.internal :as internal :refer [trace]]))

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                 (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

(defprotocol DebuggableCommand
  "An extension to Command protocol that allows a command to provide debug information."
  (verify-debug [_ model-state previous-model-state args return-value]
    "Called when verify of Command returns false. Allows returning a data structure for debugging.

    Useful to return model-state information related to verify.

    Return `:net.jeffhui.check.statem/no-debug` to indicate no debugging
    information. (Default for [[defstatem]]/[[defcommand]] macros).

    Note: remember that debugging information already includes return-value. So
    it's typically not useful to return it as well.
    "))

;; Implementation detail:
;; It is strongly recommended to use the defstatem macro instead to generate
;; conformance to this interface
(defprotocol Command
  "Command interface. NOTE: It's recommended to use [[defstatem]] and/or
  [[defcommand]] instead of implementing this protocol directly.
  Advanced-usage only.

  Command (aka - State Transition) represents an 'action' or transition that can
  be taken by a state machine. These transitions requires several bits of
  metadata:

   1. When is calling this command valid?
   2. What data does this command require besides the current state?
   3. How does the state machine's state change invoking this command?
   4. How do we validate this command's behavior to another implementation?

  The following functions map to the following questions:

   - `assume` and `only-when` answers #1, with the first being an optimization
     of the latter.
   - `args` answers #2
   - `advance` answers #3
   - `verify` answers #4

  If you're implementing these functions from `defstatem` is that the first two
  arguments (`_` and `model-state`) are assumed for brevity.
  "
  :extend-via-metadata true
  ;; listed in order of first-invocation in a test run
  (kind [_]
    "Returns a constant value of representing the type of transition this.
     Currently can be :observed or :conjectured.

     - :observed commands are traditional transitions that are invocable /
                 observable actions taken
     - :conjectured commands are transitions that are implied when another
                 transition occurrs. Conjectured commands are useful when
                 modeling asynchronous background operations.")
  (assume [_ model-state]
    "Return true to indicate that this command can be executed based on the
    current model state. Should be free of side effects.")
  (args [_ model-state]
    "Returns a vector generator to use as input arguments for the command and
    subsequent protocol functions below. The first argument of the vector must
    be the command's keyword (eg - (gen/return :add)).

    Should be free of side effects.")
  (only-when [_ model-state args]
    "Return true to indicate that this command can be executed based on the
     current model and generated arg data. Should be free of side effects.")
  (advance [_ model-state return-sym args]
    "Returns next model state from the given model state and its arguments.
    Should be free of side effects.")
  (verify [_ model-state previous-model-state args return-value]
    "Asserts if the command is valid after state application. Should be free of
    side effects."))

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

  If you have a large state machine, you can break the state machine definition
  into several forms by using `defcommand` directly. Although, it may be more
  indicative of a bigger problem if your state machine has a lot of commands
  (since test.check may not be able to generate a large part of the state
  machine's program space on any given run.)

  ### Example:

    ```clojure
    (defstatem queue-statem
      \"A basic queue state machine\"
      [mstate this]  ; -> state machine's model state is available in all commands

      ;; define commands for the state machine
      (:enqueue (args [] gen/any-printable)
                (advance [return-sym [cmd-name item]] ((fnil conj []) mstate item))
                (verify [prev-mstate [cmd-name item] ret] ret))

      (:dequeue (assume [] (pos? (count mstate)))
                (advance [_ _] (subvec mstate 1))
                (verify [_ _ return-value] (= return-value (first mstate)))))
    ```

  ### Implied parameters

    Implied parameters are like defrecord fields - parameters that exist in
    every method body. For conciseness, this is defined once instead of having to
    repeat it for every command. Otherwise state machine definitions would look like:

    ```clojure
    ;; NOTE: invalid code, do not use
    (defstatem queue-statem
      (:enqueue (assume [this model-state])
                (args [this model-state])
                (only-when [this model-state cmd-data])
                (advance [this model-state ret-sym cmd-data])
                (verify [this model-state prev-mstate cmd-data return-value])))
    ```

    Here's the following implied parameters:

    - `model-state` is the model state that is used for all commands.
    - `this` represents the command itself. Can be optionally elided.


  ### Command Methods

  #### `(assume [] ...)`

    Return true if this command can be used for a given the model state. If
    you depend on generated data, use `only-when` instead. Although using this
    method aids in faster program generation.

    Default implementation returns true. Implementation must be free of side
    effects.

  #### `(args [] ...)`

    Return a vector of generators of data needed to execute this command.
    Subsequent functions will receive the generated data as cmd-data. The
    generated data is prefixed with the keyword of the command name.

    Default implementation returns nil. Implementation must be free of side
    effects.

    ##### Example

    ```clojure
    (args [] [gen/int]) ;; => [:command-name 1]
    ```

  #### `(only-when [cmd-data] ...)`

  Return true if this command can be used for a given model state or
  generated command data.

  Default implementation calls through to `assume`. Implementation must be
  free of side effects.

  ##### Parameters

  | argument      | description                                        |
  | --------------|--------------------------------------------------- |
  | `model-state` | The model state that is used for all commands. See 'Implied parameters' section above.
  | `cmd-data`    | refers to the generated command data from `args`.


  #### `(advance [ret-sym cmd-data] ...)`

  Return the next model state from executing this command. ret-sym
  represents the symbolic value of the return value of from calling
  subject-under-test (but not yet realized).

  Default implementation returns `mstate`. Implemetation must be free of
  side effects.

  ##### Parameters

  | argument      | description                                        |
  | --------------|--------------------------------------------------- |
  | `model-state` | The model state that is used for all commands. See 'Implied parameters' section above.
  | `ret-sym`     | A symbolic representation of the subject under test's return value. This may be useful to reference usage of a return value for testing / interpreter usage.
  | `cmd-data`    | The generated command data from `args`.

  #### `(verify [prev-mstate cmd-data return-value] ...)`

  Verifies the state machine against the subject under test. Returns true
  if the subject under test returned the correct value (aka - passed an
  assertion).

  Default implementation returns true. Implementation must be free of side
  effects.

  ##### Parameters

  | argument       | description                                        |
  | ---------------|--------------------------------------------------- |
  | `model-state`  | The model state that is used for all commands. See 'Implied parameters' section above.
  | `prev-mstate`  | The model state prior to `advance`.
  | `cmd-data`     | The generated command data frmo `args`.
  | `return-value` | The actual value the subject under test returned when running.

  ### Notes

  State machine definitions are entirely abstract - meaning there is no
  external side effects that a production implementation may have. To perform
  that comparison, use a function like `run-cmds` with some integration code.
  This allows state machine definitions to be shared or reused against other
  production implementations.

  Other details of this macro:

  - args's body will wrap `(gen/tuple (gen/return command-name-kw) ...)`
  - all methods have default implementations if not specified
      - `assume` returns `true`
      - `args` returns `nil`. AKA: `(gen/tuple (gen/return command-name-kw))`
      - `only-when` returns `true`
      - `advance` returns `model-state` it was given
      - `verify` returns `true`

  ### Large State Machines

  If you have a large state machine, it may be better to break it up into
  multiple smaller ones to test. Smaller state machines allow test.check to
  generate more of the possible program space within a typical test generation
  (100 commands is test.checks' default size maximum).

  Alternatively, you can choose to generate commands with a skewed probability
  of generating specific events. It's probably not as good of a solution to
  breaking up the state machine, but can provide a more focused exploration of
  specific kinds of program generations.

  "
  [table-name & commands]
  (let [[docstring & commands]                    (if (string? (first commands))
                                                    commands
                                                    (into [nil] commands))
        [global-bindings & commands :as cmd-list] commands]
    (when (seq? cmd-list)
      (assert-args
       (vector? global-bindings)
       "missing or incorrect type of global bindings"

       (<= (count global-bindings) 2)
       "only first two args of global-bindings vector has any meaning."))
    `(do
       (def ~table-name
         ~@(when docstring [docstring])
         (->StateMachine ~(name table-name) {} {}))
       ~@(map (fn [c] `(defcommand ~table-name ~(first c) ~global-bindings ~@(rest c))) commands)
       (var ~table-name))))

(defn list-commands
  "Returns a sequence of keywords indicating available command names for the state machine.

  ### Example

    ```clojure
    (list-commands queue-statem)
    ;; => [:new :enqueue :dequeue]
    ```
  "
  [^StateMachine statem]
  (keys (.commands statem)))

(defn list-commands-by
  "Returns a sequence of keywords indicates available command names for the
  given state machine that match a given kind.

  ### Example

  ```clojure
    (list-commands-by queue-statem :observable)
    ;; => [:new :enqueue :dequeue]
    (list-commands-by queue-statem :conjectured)
    ;; => []
  ```
  "
  [^StateMachine statem k]
  (sequence
   (comp
    (remove (fn [[_ cmd]] (not= (kind cmd) k)))
    (map key))
   (.commands statem)))

(defn lookup-command
  "Returns a command that that matches a given interface. Throws if the command
  does not exist unless a default value is given.

  ### Example

    ```clojure
    (lookup-command queue-statem :new)
    ;; => <instance conforming to Command>
    ```

  "
  ([^StateMachine statem command-name]
   (trace 'lookup-command
          (assert statem "State machine cannot be nil")
          (or (get (.commands statem) command-name)
              (throw (IllegalArgumentException. (format "Failed to find command (%s) for state machine (%s)"
                                                        (pr-str command-name)
                                                        (pr-str (.name statem))))))))
  ([^StateMachine statem command-name default-value]
   (trace 'lookup-command
          (assert statem "State machine cannot be nil")
          (get (.commands statem) command-name default-value))))

(defmacro
  ^{:style/indent [3 :form :form [1]]}
  defcommand
  "Provides an simplified way to define commands for the statem.

  You probably want to use [[defstatem]] instead of this macro directly.
  [[defcommand]] allows you to structure your state machine more like
  multimethods.

  Simply is sugar for `(alter-var-root state-machine assoc-in ... (reify Command ...))`
  to save typing and some boilerplate in the following ways:

   - all methods imply model-state and this via the 3rd argument
   - args's body will wrap `(gen/tuple (gen/return command-name-kw) ...)`
   - all methods have default implementations if not specified:
       - `kind` returns `:observed`
       - `assume` returns `true`
       - `only-when` returns `true`
       - `advance` returns `model-state` it was given
       - `args` returns `nil`. AKA: `(gen/tuple (gen/return command-name-kw))`
       - `verify` returns `true`

  ### Example

    ```clojure
    (defstatem set-statem)

    (defcommand set-statem :add [mstate]
      (args [] [gen/integer])
      (advance [_ [_ value]] (conj (set mstate) value)))

    (defcommand set-statem :has [mstate]
      (args [] [gen/integer])
      (verify [_ [_ value]] (contains? (mstate) value)))
    ```

  "
  [table-name cmd-name [model-state this :as shared-bindings] & methods]
  (assert-args
   (vector? shared-bindings) "shared bindings must be a vector of [mstate this]"
   (<= (count shared-bindings) 2) "shared bindings only has at most 2 arguments [mstate this]"
   (every? seq? methods) "all methods are sequences")
  (let [allowed-methods      '#{kind assume only-when advance args verify verify-debug}
        unrecognized-methods (set/difference (set (map first methods))
                                             allowed-methods)]
    (assert-args (empty? unrecognized-methods)
                 (format "Expected to only have the following methods defined (%s), but have (%s)"
                         (string/join ", " (map pr-str allowed-methods))
                         (string/join ", " (map pr-str unrecognized-methods)))))
  (assert-args (set (map first methods)) "a method must be defined")
  (let [this          (or this (gensym "this__"))
        mstate        (gensym "mstate__")
        prev-mstate   (gensym "prev-mstate__")
        cmd           (gensym "cmd__")
        value         (gensym "value__")
        default-impls {'kind         `(kind [] :observed)
                       'assume       `(assume [] true)
                       'only-when    `(only-when [cmd-name#] true)
                       'advance      `(advance [v# cmd-name#] ~model-state)
                       'args         `(args [] nil)
                       'verify       `(verify [prev-mstate# cmd-name# return-value#] true)
                       'verify-debug `(verify-debug [prev-mstate# cmd-name# return-value#] ::no-debug)}
        impls         (merge default-impls
                             (into {} (map (juxt first identity)) methods))
        fill-impl     (fn [bindings sym]
                        (let [body   (impls sym)
                              inputs (second body)
                              b      (gensym "b__")]
                          `(let [~b           ~bindings
                                 ~model-state (first ~b)
                                 ~inputs      (vec (rest ~b))]
                             ~@(rest (rest body)))))]
    (doseq [[n impl] default-impls
            :let     [expected-size (count (second impl))
                      actual-size (count (second (impls n)))]]
      (assert-args
       (= expected-size actual-size)
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
                         DebuggableCommand
                         (verify-debug [~this ~mstate ~prev-mstate ~cmd ~value]
                           ~(fill-impl [mstate prev-mstate cmd value] 'verify-debug))
                         Command
                         (kind [~this]
                           ~(fill-impl [] 'kind))
                         (assume [~this ~mstate]
                           ~(fill-impl [mstate] 'assume))
                         (only-when [~this ~mstate ~cmd]
                           ~(fill-impl [mstate cmd] 'only-when))
                         (args [~this ~mstate]
                           (let [~model-state ~mstate
                                 generators#  (do ~@(rest (rest (impls 'args))))]
                             (apply gen/tuple (gen/return cn#) generators#)))
                         (advance [~this ~mstate ~value ~cmd]
                           ~(fill-impl [mstate value cmd] 'advance))
                         (verify [~this ~mstate ~prev-mstate ~cmd ~value]
                           ~(fill-impl [mstate prev-mstate cmd value] 'verify)))))))

(defn ->command
  "Converts a map into a command instance for use in a state machine."
  [{:keys [cmd type requires precondition args next-state postcondition postcondition-debug]}]
  (let [a args]
    (reify
      DebuggableCommand
      (verify-debug [this mstate prev-mstate c value]
        (if postcondition-debug
          (postcondition-debug mstate prev-mstate c value)
          ::no-debug))
      Command
      (kind [_]
        (or type :observed))
      (assume [_ mstate]
        (if requires
          (requires mstate)
          true))
      (only-when [_ mstate c]
        (if precondition
          (precondition mstate c)
          true))
      (args [_ mstate]
        (apply gen/tuple (gen/return cmd) (when a (a mstate))))
      (advance [_ mstate value c]
        (if next-state
          (next-state mstate value c)
          mstate))
      (verify [_ mstate prev-mstate c value]
        (if postcondition
          (postcondition mstate prev-mstate c value)
          true)))))

(defn ->statem
  "Converts a map into a state machine instance.

  Includes conversion of commands.

  ```clojure
  (->statem {:name \"MyStateMachine\"
             :commands {:start {:requires (fn [mstate] ...)
                                :precondition (fn [mstate cmd] ...)
                                :args (fn [mstate] ...)
                                :next-state (fn [mstate value cmd] ...)
                                :postcondition (fn [mstate prev-mstate cmd value] ...)
                                :postcondition-debug (fn [mstate prev-mstate cmd value] ...)}
                         :add   {...}}})
  ```
  "
  [{:keys [name commands]}]
  (->StateMachine (str name)
                  (into {}
                        (map (fn [[k v]]
                               [k (->command (assoc v :cmd k))]))
                        commands)
                  {}))

(defn- vec-drop-at [coll n]
  (trace 'vec-drop-at
    ;; coll must be a vector
    (let [before (subvec coll 0 n)
          after (subvec coll (inc n) (count coll))]
      (into before after))))

;; varsym used to be (symbol (str "var-" 1))
;; but using a tuple (aka vector) is much faster to operate with instead of having to parse
(defn varsym?
  "A predicate that returns true if v is a varsym.

  varsym are generated by [[cmd-seq]] to refer to return values produced by each
  command that is executed.

  This predicate is useful for interpreter functions to know if a given value is
  a variable to look up or a literal value.
  "
  [v]
  (and (vector? v)
       (= 2 (count v))
       (= :var (v 0))))
(defn- varsym [i]
  (trace 'varsym
    [:var i]))
(defn- varsym-offset [v]
  (trace 'varsym-offset
    (when (varsym? v)
      (v 1))))

(defn- shrink-valid-cmd-sequence?
  ([initial-state statem cmds]
   (trace 'shrink-valid-cmd-sequence?
     ;; cmds must be a vector
     ;; this is in hot path, outsize affect on cmd generation
     (let [cnt (count cmds)]
       (loop [i     0
              state initial-state]
         (if (< i cnt)
           (let [stmt (cmds i)
                 v    (stmt 1)
                 cmd  (stmt 2)
                 kind (cmd 0)
                 c    (lookup-command statem kind)]
             (if (and (assume c state) (only-when c state cmd))
               (recur (inc i)
                      (advance c state v cmd))
               false))
           true)))))
  ([initial-state statem cmds allowed-indicies]
   (trace 'shrink-valid-cmd-sequence?-with-allowed-indicies
     ;; cmds must be a vector
     ;; allowed-indicies must be a vector
     ;; this is in hot path, outsize affect on cmd generation
     (let [cnt (count allowed-indicies)]
       (loop [state    initial-state
              varindex 1]
         (if (<= varindex cnt)
           (let [[kind :as cmd] (last (cmds (allowed-indicies (dec varindex))))
                 c              (lookup-command statem kind)]
             (if (and (assume c state) (only-when c state cmd))
               (recur (advance c state (varsym varindex) cmd)
                      (inc varindex))
               false))
           true))))))


(defn valid-cmd-seq?
  "Returns true if a sequence of commands conforms to a state machine's requirements.

  ### Example

  ```clojure
  ;; for all of correct definitions of `queue-statem`, this should always pass
  (for-all [cmds (cmd-seq queue-statem)]
            (valid-cmd-seq? queue-statem cmd))
  ```
  "
  ([statem cmds] (valid-cmd-seq? statem cmds nil))
  ([statem cmds {:keys [initial-state]}]
   (trace 'valid-cmd-seq?
     (loop [rem-cmds cmds
            mstate   initial-state]
       (if (pos? (count rem-cmds))
         (let [[_ v [kind :as cmd]] (first rem-cmds)
               c                    (lookup-command statem kind)
               next-mstate          (try (advance c mstate v cmd)
                                         (catch Exception e
                                           ::error))]
           (cond
             (= ::error next-mstate) false

             (try (assume c mstate)
                  (catch Exception e
                    false))
             (recur (rest rem-cmds) next-mstate)

             :else false))
         true)))))

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
                                              (= 1 idx)          nil
                                              true               (recur (varsym (dec idx)) (dec idx))))
                                      f)))
                                (stmt 2))])))
        (transient [])
        indicies-to-keep)))))

(def ^:private empty-rose (rose/pure []))

(defn- shrink-combinations-size
  [indicies-to-keep]
  (trace 'shrink-combinations-size
    ;; indicies-to-keep must be vector
    (if (pos? (count indicies-to-keep))
      (rose/make-rose
       indicies-to-keep
       (sequence ;; can reach OOM to realize this value non-lazily
        (comp
         (mapcat (partial combo/combinations indicies-to-keep))
         (map vec)
         (map shrink-combinations-size))
        (range (count indicies-to-keep))))
      empty-rose)))

(def ^:private char-numeric
  (gen/choose 48 57))

(def fast-keyword
  "An alternative to test.check's keyword generation that reduces the data
  generation space in exchange for faster shrink times."
  (->> (gen/vector (gen/frequency [[10 gen/char-alpha]
                                   [5 char-numeric]
                                   [1 (gen/elements [\* \! \_ \? \.])]]))
       (gen/such-that (comp pos? count))
       (gen/scale #(long (Math/pow % 0.60)))
       (gen/fmap (comp keyword clojure.string/join))))

#_
(defn ^:private shrink-seq [gen]
  (let [rng (clojure.test.check.random/make-random)]
    (count (rose/children (gen/call-gen gen rng 8)))))

#_(time (shrink-seq fast-keyword))
#_(time (shrink-seq gen/keyword))


(defn- shrink-commands-size
  "Shrinks number of commands in a sequence while conforming to the state machine"
  [indicies-to-keep initial-state statem cmds]
  ;; NOTE: on the hot path. Small inefficiencies compound dramatically here
  (trace 'shrink-commands-size
    ;; indicies-to-keep must be vector
    ;; cmds must be a vector
    (if (pos? (count indicies-to-keep))
      (rose/make-rose
       (realize-cmds statem cmds indicies-to-keep)
       (sequence ;; can reach OOM to realize this value non-lazily
        (comp
         (mapcat (partial combo/combinations indicies-to-keep))
         (map vec)
         ;; (filter (partial shrink-valid-cmd-sequence? initial-state statem cmds))
         (map #(shrink-commands-size % initial-state statem cmds)))
        (range (count indicies-to-keep))))
      empty-rose)))

(defn- shrink-commands-data
  "Generates associate command data shrink tree."
  [initial-state statem cmds]
  (trace 'shrink-commands-data
    (let [rt-cmds (mapv (comp ::rose-tree meta) cmds)]
      (rose/filter
       (partial shrink-valid-cmd-sequence? initial-state statem)
       (rose/join
        (rose/fmap (fn [cmds] (shrink-commands-size (vec (range 0 (count cmds))) initial-state statem cmds))
                   (rose/zip vector rt-cmds)))))))

(defn- shrink-commands
  "Creates a rose tree from an initial value of commands generated."
  [initial-state statem cmds]
  (trace 'shrink-commands
    (let [cmds (vec cmds)
          rt   (shrink-commands-data initial-state statem cmds)]
      (rose/make-rose cmds
                      ;; QUESTION: should we include root (which is always empty)
                      ;; incase a bug occurs during initialization of the test
                      ;; case? Or is this a waste of time?
                      (rose/children rt)
                      #_(cons (rose/pure [])
                              (rose/children rt))))))

(defn- assignment-statement
  "Produces the assignment statement form: [:set [:var varindex] cmd]"
  [varindex cmd]
  (trace 'assignment-statement
    [:set (varsym varindex) cmd]))

(def ^:private make-gen #'gen/make-gen)

(defn- bind-helper
  [f rose]
  (gen/gen-fmap rose/join
                (make-gen
                 (fn [rnd size]
                   (rose/fmap #(gen/call-gen (f rose %) rnd size)
                              rose)))))

(defn- bind-with-rose
  "Like gen/bind, but passes along the generated rose-tree to f.

  This allows f to track the original rose tree for custom shrinking behaviors.
  "
  [generator f]
  (gen/gen-bind generator (partial bind-helper f)))

(defn- cmd-state-seq [select-generator state statem size excluded-commands varindex]
  (trace 'cmd-state-seq
    (let [possible-commands (into {}
                                  (comp
                                   (filter #(assume (second %) state))
                                   (map #(vector (first %) (args (second %) state))))
                                  (apply dissoc (:commands statem) excluded-commands))]
      (if (pos? (count possible-commands))
        (bind-with-rose (gen/fmap (partial assignment-statement varindex) (select-generator statem state possible-commands))
                        (fn [rt [_ _ [kind & data :as cmd] :as stmt]]
                          (if (nil? cmd)
                            (gen/return [])
                            (if (only-when (lookup-command statem kind) state cmd)
                              (if (pos? size)
                                (gen/fmap
                                 (partial into [(with-meta stmt {::rose-tree rt})])
                                 (cmd-state-seq select-generator
                                                (advance (lookup-command statem kind) state (varsym varindex) cmd)
                                                statem
                                                (dec size)
                                                #{}
                                                (inc varindex)))
                                (gen/return [(with-meta stmt {::rose-tree rt})]))
                              (cmd-state-seq select-generator state statem size (conj excluded-commands kind) (inc varindex))))))
        (gen/return [])))))

(defn- select-cmds
  "A helper that simply returns a function that conforms to select-generator param.

  f must accept a map of all valid commands to take. Keys are keywords of the
  commands and values are the command generator for that given command.

  The returned generator must choose to return a command generator or nil to
  indicate the end of the cmd seq. Note that cmd-seq may choose to end a
  sequence if it has reached the size needed to generate.
  "
  [f]
  (fn select-generator [state statem possible-kw->cmd-gen]
    (trace 'select-cmds-select-generator
      (f possible-kw->cmd-gen))))

(def select-by-any
  "A helper that simply returns a generator that picks commands randomly.

  ### Note

    Based on how cmd-seq works, this frequency is affected by the constraints
    of which command can be valid, as defined by the state machine.

  ### Example

    ```clojure
    (cmd-seq statem {:select-generator select-by-any})
    ```
  "
  (select-cmds (comp gen/one-of vals)))

(defn select-by-frequency
  "A helper that simply returns a generator that picks commands based on a
  probability map of the command keyword name to its likelihood.

  The likelihood is determined by taking the value divided by the sum of all
  likelihoods.

  ### Note

    Based on how cmd-seq works, this frequency is affected by the constraints
    of which command can be valid, as defined by the state machine.

  ### Example

    ```clojure
    (cmd-seq statem {:select-generator (select-by-frequency {:new    1000
                                                             :add    100
                                                             :remove 10})})
    ```
  "
  [cmd-kw->count]
  (select-cmds #(gen/frequency (mapv (fn [[k c]]
                                       [(cmd-kw->count k) c])
                                     %))))

(defn cmd-seq
  "A generator that produces a random sequence of commands that conform to a
  given state machine. Shrinking removes commands from the sequence while still
  conforming to the state machine.

  ### Parameters

  | key                        | required? | expected type                                | description     |
  | -------------------------- | --------- | -------------------------------------------- | --------------- |
  | `statem`                   | required  | StateMachine                                 | The state machine that the sequence of commands must conform to.
  | `select-generator`         | optional  | `(fn [state statem m] ...)`                  | A function that accepts a map of `{:command-kw command-impl}` where only commands valid for the state machine is in `m`. `state` is the current stored state of the state machine and `statem` is the given state machine. The function is expected to return a generator that picks one of the command-impls. (Default [[select-by-any]]).
  | `size`                     | optional  | non-negative integer                         | The number of commands to generate for any particular program. The default relies on test.check's natural sizing behavior.
  | `initial-state`            | optional  | anything StateMachine accepts as model state | The initial state when the state machine starts. Should be the same as the one given to [[run-cmds]].

  ### Example

  ```clojure
  (defn queue-interpreter [cmd run-cmd-ctx] ...)

  (for-all [cmds (cmd-seq queue-statem)]
    (:pass? (run-cmds queue-statem cmds queue-interpreter)))
  ```

  For a more thorough example, check out [[run-cmds]].

  ### Generated Values

  *An opaque value to pass to [[run-cmds]]. The structure may change in the
  SNAPSHOT versions.*

  Technically, returns a sequence of commands are in single-assignment
  statement form:

  ```clojure
  [
    [:set [:var 1] ...]
    [:set [:var 2] ...]
    [:set [:var 3] ...]
    ...
  ]
  ```

  Where `[:var N]` is a return value from running a given statement. This is
  abstract representation akin to this in Clojure:

  ```clojure
  (do
    (def var1 ...)
    (def var2 ...)
    (def var3 ...)
    ...)
  ```

  The data structure returned is a format that is intended:

  - To be somewhat human readable (since test.check prints this)
  - To have enough information in each statement for an interpreter function to operate
  - To be pure data to allow them to be stored as regression test cases
  - To be fast at generating. State machine generation can be slow for large state machines.


  SNAPSHOT limitations:

  The last reason is why the format should be considered opaque. It may be
  useful for inspecting, but it should be considered 'no warranty' behavior for
  depending on this result, say in a library.
  "
  ([statem]
   (cmd-seq statem nil))
  ([statem {:keys [select-generator size initial-state]
            :or   {select-generator select-by-any}
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

  ### Example

  ```clojure
  ;; for all of correct definitions of `queue-statem`, this should always pass
  (for-all [cmds (cmd-seq queue-statem)]
          (valid-cmd-seq? queue-statem cmd))
  ```
  "
  ([statem cmds] (valid-cmd-seq? statem cmds nil))
  ([statem cmds {:keys [initial-state]}]
   (trace 'valid-cmd-seq?
     (loop [rem-cmds cmds
            mstate   initial-state]
       (if (pos? (count rem-cmds))
         (let [[_ v [kind :as cmd]] (first rem-cmds)
               c                    (lookup-command statem kind)
               next-mstate          (try (advance c mstate v cmd)
                                         (catch Exception e
                                           ::error))]
           (cond
             (= ::error next-mstate) false

             (try (assume c mstate)
                  (catch Exception e
                    false))
             (recur (rest rem-cmds) next-mstate)

             :else false))
         true)))))

(defn catch-interpreter
  "Wraps an interpreter function that provides try-catch error behavior that
  run-cmds expects.

  Using this decorator is equivalent to passing `{:catch? true}` to [[run-cmds-debug]].
  "
  [interpreter]
  (vary-meta
   (fn catch-interpreter-runner [& args]
     (try
       (apply interpreter args)
       (catch Exception e
         (ex-info "Uncaught exception"
                  {::fail-fast  true
                   :exception   e
                   :interpreter interpreter
                   :args        args}
                  e))))
   assoc ::cleanup (::cleanup (meta interpreter))))

(defn catch-print-interpreter
  "Like [[catch-interpreter]], but prints to stdout

  Using this decorator is equivalent to passing `{:catch? true}` to [[run-cmds]].
  "
  [interpreter]
  (fn catch-interpreter-runner [& args]
    (try
      (apply interpreter args)
      (catch Exception e
        (.printStackTrace e)
        (ex-info "Uncaught exception"
                 {::fail-fast true
                  :exception   e
                  :interpreter interpreter
                  :args        args}
                 e)))))

(defn- error? [e]
  (boolean (::fail-fast (ex-data e))))

(defrecord HistoryEntry [valid? mstate cmd return-value var-table debug])

(defn str-history-entry
  "Returns a print-friendly output of a history entry"
  [^HistoryEntry v]
  (format "%s %s -> %s"
          (if (:valid? v)
            "(✓)"
            "(x)")
          (pr-str (:cmd v))
          (when-not (:valid? v)
            (condp = (:debug v)
              ::no-debug  (str (pr-str (:return-value v)))
              ::exception (str "threw" (pr-str (:return-value v)))
              (str (pr-str (:return-value v)) " (verify-debug " (:debug v) ")")))))

(defn interpreter-with-cleanup
  "Adds metadata to an interpreter function to allow cleanup code to run after a
  test run finishes.

  This allows an interpreter to clean up any resources created during the test
  run by accessing the var-table of the run - such as closing open files.
  "
  [interpreter-fn cleanup-fn]
  (vary-meta interpreter-fn assoc ::cleanup cleanup-fn))

;; Represents an execution result
;; Note: newer versions may add more fields
(defrecord ExecutionResult [pass? cmds history]
  results/Result
  (pass? [m] (:pass? m))
  (result-data [m] m))

(defn run-cmds
  "Executes the symbolic representation of a sequence of commands using an
  interpreter.

  ### Returns

  An ExecutionResult record. Always returns a map with a key `:pass?` to
  indicate if the test program succeeded or failed. Alternatively, you can use
  `clojure.test.check.results/pass?` to resolve to true. This means simply
  returning this value to `for-all` will work as expected (by checking `:pass?`).

  ExecutionResult contains the execution history that can be useful for debugging.
  See [[when-failed!]] macro for example usage.

  ### Parameters

  | key             | required? | expected type                                 | description     |
  | --------------- | --------- | --------------------------------------------- | --------------- |
  | `statem`        | required  | StateMachine                                  | The state machine needed to verify behavior against.
  | `cmds`          | required  | seq of symbolic commands                      | The sequence of commands to execute against the subject under test. This should be generated from [[cmd-seq]].
  | `intepreter`    | required  | `(fn [cmd ctx] ...)`                          | The interface to interacting with the subject under test. See 'Interpreter'section below.
  | `initial-state` | optional  | anything valid for StateMachine's model state | The initial state machine state. Should be the same as the one given to [[cmd-seq]].
  | `catch?`        | optional  | boolean                                       | Should this runner attempt to catch exceptions? Catching exceptions allows the runner to minimize the failure, but may lose the original stacktrace. (Default `true`)

  ### Interpreter

  ```
    :: (fn interpreter [cmd run-cmds-ctx])
      where
        cmd          :: [cmd-type & generated-cmd-args]
        run-cmds-ctx :: {:keys [var-table]}
        var-table    :: {VariableSymbol value}
  ```

  Interpreter receives every command to execute and is expected to run against
  the subject under test. The return value of interpreter is the `return-value`
  used in the state machine's `verify` method.

  This function bridges the model state machine with a concrete implementation.
  Having this interpreter function also keeps the model state machines free
  from directly comparing to a specific imlementation.

  `var-table` is a map of symbolic variables to concrete values. A symbolic
  value is in the form `[:var N]` in the command generation value:

  ```clojure
  [[:set [:var 1] [:upload \"foo\"]]
  ;;     ^^^^^^^^ this
   [:set [:var 2] [:result [:var 1]]]]
  ;;     ^^^^^^^^ and this
  ```

  This allows the state machine and interpreter to hold/refer to stateful
  references to from previous statements.

  For the state machine side, look at [[advance]] to see ret-sym,
  which is the symbolic reference to the return value of the command after
  execution.

  ### Example

  ```clojure
  ;; elided: TestQueue implementation
  (defn queue-runner [cmd {:keys [varsym var-table]}]
    (case (first cmd)
      :new     (TestQueue. [] (second cmd))
      :enqueue (.enqueue ^IQueue (var-table (second cmd)) (nth cmd 2))
      :deque   (.dequeue ^IQueue (var-table (second cmd)))))

  (defstatem queue-statem
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
            (verify [_ _ r] (= r (first (:items mstate))))))

  (for-all [cmds (cmd-seq queue-statem)]
            (run-cmds queue-statem cmds queue-interpreter))
  ```
  "
  ([^StateMachine statem cmds interpreter] (run-cmds statem cmds interpreter nil))
  ([^StateMachine statem cmds interpreter {:keys [initial-state catch?]
                                           :or   {catch? true}}]
   (trace 'run-cmds
     (assert (and (seqable? cmds)
                  (or (zero? (count cmds))
                      (and (vector? (first cmds))
                           (keyword (ffirst cmds)))))
             "Invalid commands. Did you mean to remove one level of nesting from test results?")
     (let [interpreter (if catch?
                         (catch-print-interpreter interpreter)
                         interpreter)
           result      (loop [rem-cmds  cmds
                              mstate    initial-state
                              var-table {}
                              history   (transient [(->HistoryEntry true initial-state [:initial-state]
                                                                    nil {} ::no-debug)])]
                         (if (pos? (count rem-cmds))
                           (let [[_ v [kind :as cmd]] (rem-cmds 0)
                                 c                    (lookup-command statem kind)
                                 next-mstate          (advance c mstate v cmd)
                                 return-value         (interpreter cmd {:var-table var-table})
                                 verify-result        (and (not (error? return-value))
                                                           (verify c next-mstate mstate cmd return-value))]
                             (if (results/pass? verify-result)
                               (recur (subvec rem-cmds 1)
                                      next-mstate
                                      (if (nil? return-value)
                                        var-table
                                        (assoc var-table v return-value))
                                      (conj! history (->HistoryEntry verify-result next-mstate cmd return-value var-table
                                                                     (cond (error? return-value)            ::exception
                                                                           (satisfies? DebuggableCommand c) (verify-debug c next-mstate mstate cmd return-value)
                                                                           :else                            ::no-debug))))
                               (->ExecutionResult false cmds
                                                  (persistent! (conj! history
                                                                      (->HistoryEntry verify-result next-mstate cmd
                                                                                      return-value var-table
                                                                                      (cond (error? return-value)            ::exception
                                                                                            (satisfies? DebuggableCommand c) (verify-debug c next-mstate mstate cmd return-value)
                                                                                            :else                            ::no-debug)))))))
                           (->ExecutionResult true cmds (persistent! history))))
           cleanup     (:check.statem/cleanup (meta interpreter))]
       (when cleanup
         (cleanup result))
       result))))

(defmacro ^{:style/indent [1]} when-failed!
  "Executes a given body when a execution fails. Useful for printing debugging information.

  You may prefer [[print-failed-runs!]] if you want something quick to print out on failure.

  Shorthand, for:

      (let [result ~execution-result]
        (when-not (clojure.test.check.results/pass? result)
          (let [~bindings [(:history result) (:cmds result)]]
            ~@body)))

  This useful for debugging failures.

  ### Example

    ```clojure
    (for-all [cmd (cmd-seq queue-statem)]
      (when-failed! (run-cmds queue-statem cmds interpreter)
        [history cmds]
        (println \"FAILED\")
        (doseq [entry history]
          (println \" \" (str-history-entry entry)))))
    ```
  "
  [execution-result bindings & body]
  (assert (vector? bindings) "bindings must be a vector of one or two variables")
  (assert (<= 1 (count bindings) 2) "bindings only supports one or two variables (history, cmds)")
  `(let [r# ~execution-result]
     (when-not (results/pass? r#)
       (let [~bindings [(:history r#) (:cmds r#)]]
         ~@body))
     r#))

(defn print-failed-runs!
  "A helper function thar prints execution results if a run has failed. Useful
  to scan and see all failing tests. For convinence, only prints small, failing
  executions by default.

  Returns the execution result given.

  If you're looking to do something else on failure, see the [[when-failed!]]
  macro.

  ### Parameters

    - `max-size` **(optional, integer)**
        If set, only prints history sizes less than or equal to this max size. Defaults to 10.
        Setting this to nil will print any sized failure.
    - `mstate?` **(optional, boolean)**
        If true, prints the model state for each executed command. Defaults to false.
    - `execution-result` **(required, ExecutionResult)**
        The execution result produced by [[run-cmds]].

  ### Examples

    ```clojure
    (print-failed-runs! (run-cmd statem cmds interpreter))
    (print-failed-runs! {:mstate? true} (run-cmd statem cmds interpreter))
    ```

  "
  ([execution-result] (print-failed-runs! nil execution-result))
  ([{:keys [mstate? max-size]
     :or   {mstate? false
            max-size 10}}
    execution-result]
   (when-failed! execution-result
     [history cmds]
     (when (or (nil? max-size) (<= (count history) max-size))
       (println "-----FAIL------")
       (println)
       (doseq [entry history]
         (println " " (str-history-entry entry))
         (when mstate?
           (println "      └" (pr-str (:mstate entry)))))
       (println)
       (println "======END=======")))))

(defn run-cmds-debug
  "Identical to [[run-cmds]], but prints out data related to each command executed.

  Because of the debugging instrumentation, this executes commands more slowly
  than [[run-cmds]]. This function is typically more useful if you're diagnosing
  why one particular sequence of commands is hanging.

  ### Parameters

  - `statem` **(required, StateMachine)**
      The state machine needed to verify behavior against.
  - `cmds` **(required, seq of symbolic commands)**
      The sequence of commands to execute against the subject under test.
  - `interpreter` **(required, fn[2-args])**
      The interface to interacting with the subject under test. See 'Interpreter'
      section below.
  - `inital-state` **(optional, anything valid for StateMachine's model state)**
      The initial state machine state. Should be the same as the one given to
      `cmd-seq`.
  - `mstate?` **(optional, bool)**
      If true, print out the model state after each command. Defaults to true.
  - `return-value?` **(optional, bool)**
      If true, print out the return value for `verify` after each command.
      Defaults to true.
  - `catch?` **(optional, bool)**
      Should this runner attempt to catch exceptions? Catching exceptions allows
      the runner to minimize the failure, but may lose the original stacktrace.
      Defaults to true.
  - `debug-method` **(optional, keyword)**
      How should debug information be emitted? Default is `:print` which prints
      to stdout. Supported methods:

        - `:print` Prints data to stdout. Default value.
        - `:print-short` Prints data to stdout in compact form. Ignores `:mstate?` option.
        - `:inspect` Emits data to clojure.inspector/inspect.
        - `:inspect-tree` Emits data to clojure.inspector/inspect-tree
        - `:inspect-table` Emits data to clojure.inspector/inspect-tabl

      Note that inspect debug methods create windows per `run-cmds-debug`
      invocation, so using those methods inside a property may cause many windows to
      be created.
  "
  ([^StateMachine statem cmds interpreter]
   (run-cmds-debug statem cmds interpreter nil))
  ([^StateMachine statem cmds interpreter
    {:keys [initial-state
            mstate?
            return-value?
            catch?
            debug-method]
     :or   {return-value? true
            catch?        true
            mstate?       true
            debug-method  :print}}]
   (let [interpreter (if catch?
                       (catch-interpreter interpreter)
                       interpreter)
         tracer      (cond
                       (#{:inspect :inspect-tree :inspect-table} debug-method)
                       (internal/->CmdRunInspector mstate? return-value? debug-method (atom nil))

                       (= :print debug-method) (internal/->CmdRunPrinter mstate? return-value?)

                       (= :print-short debug-method) (internal/->CmdRunAbridgedPrinter return-value?)

                       :else
                       (throw (IllegalArgumentException. (str "Unsupported debug-method: " (pr-str debug-method)))))
         _      (internal/run-start tracer cmds nil)
         result (loop [rem-cmds  cmds
                       mstate    initial-state
                       var-table {}
                       history   (transient [(->HistoryEntry true initial-state [:initial-state]
                                                             nil {} ::no-debug)])]
                  (if (pos? (count rem-cmds))
                    (let [[_ v [kind :as cmd] :as stmt]
                          (first rem-cmds)

                          c             (lookup-command statem kind)
                          next-mstate   (advance c mstate v cmd)
                          _             (internal/run-step tracer stmt mstate next-mstate var-table)
                          return-value  (interpreter cmd {:var-table var-table})
                          verify-result (and (not (error? return-value))
                                             (verify c next-mstate mstate cmd return-value))]
                      (internal/run-return tracer stmt mstate next-mstate var-table return-value verify-result)
                      (if (results/pass? verify-result)
                        (recur (rest rem-cmds)
                               next-mstate
                               (if (nil? return-value)
                                 var-table
                                 (assoc var-table v return-value))
                               (conj! history (->HistoryEntry verify-result next-mstate cmd return-value var-table
                                                              (cond (error? return-value)            ::exception
                                                                    (satisfies? DebuggableCommand c) (verify-debug c next-mstate mstate cmd return-value)
                                                                    :else                            ::no-debug))))
                        (->ExecutionResult false cmds
                                           (persistent! (conj! history
                                                               (->HistoryEntry verify-result next-mstate cmd
                                                                               return-value var-table
                                                                               (cond (error? return-value)            ::exception
                                                                                     (satisfies? DebuggableCommand c) (verify-debug c next-mstate mstate cmd return-value)
                                                                                     :else                            ::no-debug)))))))
                    (->ExecutionResult true cmds (persistent! history))))
         cleanup (:check.statem/cleanup (meta interpreter))]
     (internal/run-end tracer result)
     (when cleanup
       (cleanup result))
     result)))

(defn always-fn
  "Function form of [[always]] macro. See the macro for more details.

  ### Parameters

  | key      | required? | expected type    | description     |
  | -------- | --------- | ---------------- | --------------- |
  | `f`      | required  | `(fn [] ...)`    | The function to execute that returns a Result value (eg - [[run-cmds]])
  | `n`      | optional  | positive integer | The number of times to repeatedly run a property to see if it failed.
  "
  ([f] (always-fn 10 f))
  ([n f]
   (let [result         (f)]
     (if (results/pass? result)
       (if (zero? n)
         result
         (recur (dec n) f))
       result))))

(defmacro always
  "Repeats a body that returns any [[clojure.test.check.results/Result]] conforming
  value up to n times. Returns the first failing result encountered.

  This is useful to verify that asynchronous / concurrent behavior doesn't cause
  any flakey behavior.

  ### Parameters

  | key      | required? | expected type    | description     |
  | -------- | --------- | ---------------- | --------------- |
  | `body`   | required  | expression       | The form to execute that returns a Result value (eg - [[run-cmds]])
  | `n`      | optional  | positive integer | The number of times to repeatedly run a property to see if it failed.

  **Notes:**

    Papers from the Erlang QuickCheck implementation mention `n=10` is a
    'practically good enough' value. Lower values of `n` risk not finding flaky
    behavior, but higher values of `n` will require more executions (and take more
    time to complete).


  **Example:**

  ```clojure
  (always (run-cmds statem cmds interpreter))
  ```

  "
  ([body] `(always-fn (fn [] ~body)))
  ([n body] `(always-fn ~n (fn [] ~body))))

(defn sometimes-fn
  "Function form of [[sometimes]] macro. See the macro for more details."
  ([f] (sometimes-fn 10 f))
  ([n f]
   (let [result (f)]
     (if (results/pass? result)
       result
       (if (zero? n)
         result
         (recur (dec n) f))))))

(defmacro sometimes
  "Repeats a body that returns any [[clojure.test.check.results/Result]] conforming
  value up to n times. Returns failure if only every try also fails.

  This is useful to verify that asynchronous / concurrent behavior that you want
  to shrink to a reliably reproducable failure.

  ### Parameters

  | key      | required? | expected type    | description     |
  | -------- | --------- | ---------------- | --------------- |
  | `body`   | required  | expression       | The form to execute that returns a Result value (eg - [[run-cmds]])
  | `n`      | optional  | positive integer | The number of times to repeatedly run a property to see if it always fails.

  ### Example

  ```clojure
  (sometimes (run-cmds statem cmds interpreter))
  ```

  "
  ([body] `(sometimes-fn (fn [] ~body)))
  ([n body] `(sometimes-fn ~n (fn [] ~body))))

(defn- denamespace-syms [form]
  (walk/postwalk
   (fn [f]
     (if (symbol? f)
       (symbol (name f))
       f))
   form))

(defn check!
  "Performs validations against state machine. Useful for sanity-checking state
  machine definitions before attempting to use them.

  This will never run `verify` methods of state machine commands. But this does
  generate command sequences.
  "
  [^StateMachine statem]
  ;; basic checks
  (assert (.name statem) "State Machine is missing a name")
  ;; checks by generating programs
  (let [num-cmds      (count (.commands statem))
        programs      (gen/sample (cmd-seq statem {:size num-cmds}) (* num-cmds 10))
        cmd-metadatas (:cmd-metadatas statem)]
    ;; generator destructuring check
    (if (empty? cmd-metadatas)
      (println "WARN:" (.name statem) "has no command metadata forms. (only available with defcommand or defstatem)")
      (doseq [stmts                    programs
              [_ _ [cmd-name :as cmd]] stmts
              :let                     [cmd-meta (get cmd-metadatas cmd-name)
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
          (check-gen-args 'verify 1))))

    ;; check command coverage
    (let [exercised-cmds (->> programs
                              (map (fn [stmts] (set (map (comp first last) stmts))))
                              (apply concat [])
                              set)
          all-cmds       (set (list-commands statem))
          diff           (set/difference all-cmds exercised-cmds)]
      (assert (empty? diff)
              (str "Expected to generate all commands, but didn't. Commands that didn't get generated: "
                   (pr-str diff))))))

;; TODO: things to test
;;
;; - interpreter-with-cleanup
;; - shrinking command data va commands
;; - when-failed!
;; - valid-cmd-seq?
