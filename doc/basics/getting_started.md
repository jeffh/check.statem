# Making Your First State Machine Tests

We're going to write a basic state machine to get a feel for `check.statem`.
Then, we're going to write a state machine that models a basic key-value store
and then compare it against another implementation.


## Including Check.Statem

Add this to your `deps.edn` file:

```clojure
{:deps {net.jeffhui/check.statem {:mvn/version "1.0.0-SNAPSHOT"}}}
```

For leiningen, add:

```clojure
[net.jeffhui/check.statem "1.0.0-SNAPSHOT"]
```

And now we're ready.

## Modeling a Key Value Store

The most difficult part of state machine testing is modeling the system. That's
several articles in itself! For now, let's describe our tiny example: 


### Key Value Store Operations

| operation | data       | description |
| --------- | ---------- | ----------- |
| put       | key, value | Inserts a value at location key. Overwrites if an existing key is there.
| get       | key        | Retrieves a value for a given key. Getting a non-existant key is not allowed.

Yeah, it's just a hash map. But going through the basics motions will
demonstrate check.statem code needed instead of having to work on the
implementations.

At a high level, we're going to do this:

1. Define a state machine
2. Generate test programs using that state machine
3. Use the test programs to verify an implementation

So lets get started!


## State Machine #1

We can start defining our state machine:

```clojure
(ns example.project
  (:require [clojure.test.check.generators :as gen]
            [net.jeffhui.check.statem :as statem]))

(statem/defstatem key-value-statem ;; name of the state machine
  [mstate] ;; the internal model state -- starts as nil
  ;; define transitions (check.statem utilizes 'transitions' and 'commands' interchangably)
  (:put ;; name of the transition
    (args [] [gen/keyword gen/any-printable-equatable]) ;; associated data generators for this transition
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
```

Phew, that's done. Let's go through it step by step of the `defstatem` macro:

1. The first argument `key-value-statem`, defines the name of the state machine that we can use to refer to this definition later
2. The next argument is like a field definition. State machines have `[mstate cmd]` as bindable to variables.
  - `mstate` is the current model state. This is the state machine's internal state between transitions
  - `cmd` is refers to the current command instance. In practice, it's reserved for some special cases that we won't be using, so we can omit it.
3. The remaining arguments define the state machine transitions. It's a kin to
   `(:transition-name & methods)` with the methods be specific ones we can opt
   to implement (or leave a default implementation).

### Transitions

Each transition defines the following information:

- The name of the transition. It can be any edn value that is used to refer to
  the transition. Note that it is used for execution, so a keyword is
  convention.
- preconditions that must be met before being allowed to follow a transition.
  A transition that never has its precondition satisfied will never be
  utilized in program generation.
- Is there any associated data needed to follow this transition? These are
  defined as a sequence of clojure.test.check generators.
- next-state method that returns a new version of model state (`mstate`) for
  following this transition.
- postconditions that can be used to verify this state machine against another implementation.

And in code, we can translate to the following, expanded form:

```clojure
;; remember, mstate is defined in the defstatem macro
(:put                                                ;; -> name of the transition / command
 (assume [] true)                                    ;; precondition, before argument data generation
 (args [] [gen/keyword gen/any-printable-equatable]) ;; associated data generators for this transition
 (only-when [generated-args] true)                   ;; precondition, after argument data generation
 (advance [return-value-sym generated-args]          ;; produces the next mstate
   (assoc mstate
          (nth generated-args 1)
          (nth generated-args 2)))
 (verify [prev-mstate generated-args return-value]   ;; postcondition expression
   true))
```

That's a lot of data needed for each transition! In our original definition, we
omitted defining methods that don't change from the default value:

- `assume` returns `true` by default, so we can omit it
- `args` returns `[]` by default, so we need our implementation
- `only-when` returns `true` by default, so we can omit it
- `advance` returns `mstate` by default, so we need to override it with out implementation
- `verify` returns `true` by default, so we can omit it

This gives the form we had earlier:

```clojure
;; this equivalent to the expanded version
(:put
 (args [] [gen/keyword gen/any-printable-equatable]) ;; associated data generators for this transition
 (advance [_ [_ k v]]                                ;; produces the next mstate
   (assoc mstate k value)))
```

Ok, now lets at look at each of the transitions we defined, and their methods.

### PUT

#### args

This method is expected to return a vector of generators. We want a key and a
value so we're providing two generators:

```clojure
[gen/keyword                  ;; -> our key
 gen/any-printable-equatable] ;; -> our value
```

We could use `gen/any-printable-equatable` for both, but it's a bit faster to
restrict the kinds of data types generated, so this seems acceptable for now.

check.statem will prepend `(gen/return <command-name>)` to this argument list so
that it can identify which command was generated. This means the final data
generator looks like this:

```clojure
(gen/tuple (gen/return :put)
           gen/keyword
           gen/any-printable-equatable)
```


This is important to realize because the other methods will only see this format.


#### advance

Advance is expected to immutably return the next model state given the current
one and the generated argument values from `args`.

Our code is using destructuring to archieve this:

```clojure
(advance [_ [_n k v]] ...)
```

Here, the `k` maps to the return of `gen/keyword` and `v` maps to
`gen/any-printable-equatable`. Remember about the prefixed generator test.check
puts in? That's the first argument of the inner vector (named `_n` in the code
above).

So what's the first argument to `advance`? It's a symbolic reference to the
return value that another implementation returns. Typically named `ret-sym` for
return symbol.

`ret-sym` is for more specialized use cases. If our implementation returned a
mutable value that we want to refer to in subsequent transitions, we would want
to store `ret-sym` in the app state for future use.


### GET

#### assume

The `get` transition defines an `assume` precondition method. This means we can
constrain when `assume` gets generated.

```clojure
(:get (assume [] (pos? (count mstate)))
      ...)
```

Here, we're saying before we call get, we need a key in our app state. This is
useful for our `args` method...

#### args

```clojure
(:get
    (assume [] (pos? (count mstate)))
    (args [] [(gen/elements (keys mstate))])
    ...)
```

Here we choose one of the keys in our model state to return in our generator.
That means this transition will always pick a key should be able to receive a
value from.

Like in put's case, check.statem will prepend the command name to the generated
arguments.


#### verify

```
(:get
    (assume [] (pos? (count mstate)))
    (args [] [(gen/elements (keys mstate))])
    (verify [_ [_n k] return-value]
      (= (mstate k) return-value)))
```

Finally, we have a postcondition that compares the state machine against a real
implementation we comparing against.

The first argument is the previous model state prior to advance. This is
sometimes useful if advance removes data that you need for the postcondition.

The first argument in the inner vector (named `_n_` is the command name `:get`),
so we're ignoring it like we did in the `put` implementation.

Finally, `return-value` is the return value produced by the subject/api under
test. This doesn't come from the state machine, but can be used to compare
values the model state to ensure the subject is behaving as expected.

### Santity Checking

Phew, that was a compact state machine definition! Lets sanity check it.

```clojure
(statem/check! key-value-statem) ;; => nil
```

This throws an exception if we have something obviously incorrect about our
state machine definition - such as destructuring more generated arguments than
we specified in `args`.

Also, this throws an exception if it can't seem to practically generate all
commands. Usually that means something about the preconditions is preventing it
from doing so.

Since `check!` doesn't use an implementation to compare against. No verification
of `verify` methods are made.

## Generating Test Programs

With our state machine defined and validated, we can now start generating test
programs that conform to this state machine.

```clojure
(gen/generate (statem/cmd-seq key-value-statem))
;; =>
[[:set [:var 1] [:put :W4 {}]]
 [:set [:var 2] [:get :W4]]]

```

The returned data is an symbolic program. Or more simply said, it's a data
representation of a program we can execute. The program is a sequential,
and imperative that translates to:

```clojure
(do
  (def v1 (put :W4 {}))
  (def v2 (get :W4)))
```

This looks good and we're ready to run it against a production implementation.

## Comparing to an Implementation

Lets try it out! We're going to test in against an implementation that uses
vectors as the underlying storage:

```clojure
;; state = (atom [])
(defn kv-put [state key value]
  (swap! state conj v [k v]))
(defn kv-get [state key]
  (second (first (filter (comp (partial = key) first) @state))))
```

Then, to compare the model against an implementation in tests, we need
interpreter function that can translate the transitions from the state machine
to use against an actual implementation:

```clojure
;; based on the example data above, the inner function will be called twice with cmd being:
;;   cmd = [:put :W4 {}], ctx = {}
;;   cmd = [:get :W4], ctx = {:var-table {[:var 1] nil}}
(defn kv-interpreter []
  (let [state (atom [])]
    (fn interpreter [cmd ctx]
      (case (first cmd)
        :put (kv-put state (nth cmd 1) (nth cmd 2))
        :get (kv-get state (second cmd))))))
```

`kv-interpreter` receives a transition, one at a time. If we used the generated
example from above, it would be roughly similar to:


```clojure
(let [kv (kv-interpreter)]
  (kv [:put :W4 {}])
  (kv [:get :W4]))
```

With the return value of `kv` being given to the state machine's `verify`
methods for each transition.

Enough babbling, let's put it all together into a property:

```clojure
(require '[clojure.test.check.clojure-test :refer [defspec]])
(require '[clojure.test.check.properties :refer [for-all]])

(defspec kv-spec 100
  (for-all
   [cmds (statem/cmd-seq key-value-statem)]
   (statem/run-cmds key-value-statem cmds (kv-interpreter))))
```

If you run it a few times, you'll get a test failure similar to this value:

```clojure
[[:set [:var 16] [:put :A 0]]
 [:set [:var 17] [:put :A 1]]
 [:set [:var 18] [:get :A]]]
```

This is a minimal test case that produces the failure of our implementation (in
`kv-get`).

#### Troubleshooting

Sometimes it's easy enough to see the problem by the list of commands executed
that test.check emits, but it would be nice to get more details!

We can wrap our `run-cmds` call with `print-failed-runs!` which will print the
command and its return value:


```clojure
(defspec kv-spec 100
  (for-all
   [cmds (statem/cmd-seq key-value-statem)]
   (statem/print-failed-runs!
    (statem/run-cmds key-value-statem cmds (kv-interpreter)))))
```

In stdout of the repl, we'll get a lot of printing with the final being:

```
... many lines ...
-----FAIL------

  (✓) [:put :A 0] -> [[:A 0]]
  (✓) [:put :A -1] -> [[:A 0] [:A -1]]
  (x) [:get :A] -> 0

======END=======
```

Here, we see when we called `get` we got our original value instead of the
latter put value.

Alternatively, if it took to long to find the error the first time, you can
"create" an example test by just copying the specification, and replacing the
for-all with a let form. Then pasting the failing example from test.check.

```clojure
(let [cmds [[:set [:var 16] [:put :A 0]]
            [:set [:var 17] [:put :A 1]]
            [:set [:var 18] [:get :A]]]]
  (statem/print-failed-runs!
    (statem/run-cmds key-value-statem cmds (kv-interpreter))))
```

This can be used as a example-based test if you just use `:pass?` like:

```clojure
(deftest my-example
  (let [cmds [[:set [:var 16] [:put :A 0]]
              [:set [:var 17] [:put :A 1]]
              [:set [:var 18] [:get :A]]]]
    (is (:pass?
          (statem/print-failed-runs!
            (statem/run-cmds key-value-statem cmds (kv-interpreter)))))))
```

Finally, we can update the implementation to fix the test:


```clojure
(defn kv-get [state key]
  (second (last (filter (comp (partial = key) first) @state))))
  ;;       ^^^^ replace 'first' to 'last'
```

And if we run the specification again, everything should pass!
