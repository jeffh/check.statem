(ns net.jeffhui.check.do-not-use.parallel
  "100% experimental. Expect nothing will be here tomorrow."
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]
            [net.jeffhui.check.statem :as statem])
  (:import java.util.concurrent.Semaphore))

;; We acknowledge the evil of accessing a private var, but we need it
(def ^:private make-gen @#'gen/make-gen)

(defn- prop-always-passes-helper
  [pred gen required-num-passes rng size]
  (loop [tries-left required-num-passes
         rng rng]
    (let [[r1 r2] (random/split rng)
          value (gen/call-gen gen r1 size)]
      (if (>= 1 tries-left)
        value
        (if (pred (rose/root value))
          (recur (dec tries-left) r2)
          value)))))

(defn ^{:style/indent 1}
  prop-always-passes
  "Ensures a every property generated passes repeated executions of `required-num-passes`.

  A property fails if it every fails any execution. This isn't a perfect, but a
  practical way to find race conditions which can help with shrinking.
  "
  [required-num-passes gen]
  (assert (gen/generator? gen) "Second arg to prop-always-passes must be a generator")
  (make-gen
   (fn [rand-seed size]
     (prop-always-passes-helper :result gen required-num-passes rand-seed size))))

(defn ^{:style/indent 1}
  prop-sometimes-passes
  "Ensures a property generated passes at least once for repeated executions.

  Will attempt `required-num-passes` times before a no successful result will
  fail the property. This isn't a perfect, but a practical way to ignore flaky
  behaviors which can help with shrinking.
  "
  [num-attempts gen]
  (assert (gen/generator? gen) "Second arg to prop-sometimes-passes must be a generator")
  (make-gen
   (fn [rand-seed size]
     (prop-always-passes-helper (complement :result) gen num-attempts rand-seed size))))

;;;;;;;;;;;;;;;;;;;;;;;;;; TODO: everything below is experimental and incomplete

(defn- cmd-state-seq [select-generator state statem size excluded-commands varindex]
  (let [possible-commands (into {}
                                (comp
                                 (filter #(statem/assume (second %) state))
                                 (map #(vector (first %) (statem/args (second %) state))))
                                (apply dissoc (:commands statem) excluded-commands))]
    (if (pos? (count possible-commands))
      (gen/bind (select-generator statem state possible-commands)
                (fn [[kind & data :as cmd]]
                  (if (nil? cmd)
                    (gen/return [])
                    (if (statem/only-when (#'statem/statem-command statem kind) state cmd)
                      (if (pos? size)
                        (gen/fmap
                         (partial into [(#'statem/assignment-statement varindex cmd)])
                         (cmd-state-seq select-generator
                                        (statem/advance (#'statem/statem-command statem kind) state (#'statem/varsym varindex) cmd)
                                        statem
                                        (dec size)
                                        #{}
                                        (inc varindex)))
                        (gen/return [(#'statem/assignment-statement varindex cmd)]))
                      (cmd-state-seq select-generator state statem size (conj excluded-commands kind) (inc varindex))))))
      (gen/return []))))

(defn cmd-seq
  ([statem] (cmd-seq statem nil))
  ([statem {:keys [select-generator size initial-state]
            :or   {select-generator statem/select-by-any}
            :as   options}]
   (if size
     (gen/shrink-2
      (gen/bind
       (cmd-state-seq select-generator initial-state statem size #{} 1)
       (fn [cmds]
         (gen/gen-pure (#'statem/shrink-commands initial-state statem cmds)))))
     (gen/bind (gen/sized #(gen/choose 0 %))
               #(cmd-seq statem (assoc options :size %))))))

(defprotocol Scheduler
  ;; EQC uses the following interface
  ;; - sending {send, Pid, Msg}
  ;; - yielding {}
  ;; - spawning {Pid}
  ;; - receiving {Receiver} where Receiver is pred indicating ability to handle msg
  ;;
  ;; but that works well within the runtime environment of Erlang
  (^Thread spawn [sch ^Runnable runnable] "Requests the scheduler to spawn a thread.")
  (^void yield [sch] "Requests the scheduler to yield to another thread.")
  (start [sch] "Tears down all managed threads and the scheduler"))

(defn run-thread
  "Shorthand for (doto (Thread. r) (.start))"
  ([^Runnable r]
   (doto (Thread. r)
     (.start)))
  ([^String name ^Runnable r]
   (doto (Thread. r name)
     (.start))))

(defn system-scheduler
  "A scheduler that behaves like java concurrent primitives.

  `spawner` is a function that knows how to create and run threads. The default
  is a new java Thread each time spawn is called.
  "
  ([] (system-scheduler nil))
  ([spawner]
   (let [spawner (or spawner run-thread)]
     (reify Scheduler
       (spawn [this runnable]
         (spawner
          (fn []
            (yield this)
            (.run runnable))))
       (yield [this])
       (start [this])))))

(defn- ^Semaphore scheduler-get-semaphore [state-val t]
  (or (->> state-val :managed-threads (filter (comp #{t} first)) first second)
      (->> state-val :unmanaged-threads (filter (comp #{t} first)) first second)))

(defn print-taps [{:scheduler/keys [type current-thread yield-to
                                    yielded-thread possible-threads
                                    new-thread]}]
  (when type
    (locking *out*
      (println
       (format "%d [%s] %s"
               (System/nanoTime)
               (.getName current-thread)
               (case type
                 :scheduler/created                   "spawned scheduler"
                 :scheduler/scheduler-created         "scheduler created"
                 :scheduler/scheduler-thread-resume   (str "scheduler resumed: "
                                                           (mapv #(.getName %) possible-threads))
                 :scheduler/scheduler-thread-started  "scheduler started & yielded"
                 :scheduler/scheduler-schedule-thread (str "scheduler picks "
                                                           (.getName yield-to)
                                                           " out of "
                                                           (mapv #(.getName %) possible-threads))
                 :scheduler/scheduler-thread-exit     "scheduler exited"
                 :scheduler/thread-start              "thread started"
                 :scheduler/thread-exit               "thread exited"
                 :scheduler/thread-spawn              (str "spawn child thread "
                                                           (.getName new-thread))
                 :scheduler/thread-yield              "thread yielded"
                 :scheduler/thread-resume             "thread resumed"))))))

(comment
  (add-tap print-taps)
  (remove-tap print-taps)
  (swap! @#'clojure.core/tapset (constantly #{}))
  )

(defn- ^Semaphore semaphore []
  (Semaphore. 0))

(defn- scheduler-loop [tap ^Semaphore sem state-atom thread-selector]
  (tap #:scheduler{:type           :scheduler/scheduler-thread-started
                   :current-thread (Thread/currentThread)})
  (try
    (loop []
      (.acquire sem)
      (let [s                @state-atom
            thread           (:current-thread s)
            possible-threads (vec
                              (map first
                                   (into (:managed-threads s)
                                         (:unmanaged-threads s))))
            next-thread      (when (pos? (count possible-threads))
                               (thread-selector possible-threads))]
        (tap #:scheduler{:type             :scheduler/scheduler-thread-resume
                         :current-thread   (Thread/currentThread)
                         :yielded-thread   thread
                         :possible-threads possible-threads})
        (when (and thread next-thread)
          (tap #:scheduler{:type             :scheduler/scheduler-schedule-thread
                           :current-thread   (Thread/currentThread)
                           :yielded-thread   thread
                           :possible-threads possible-threads
                           :yield-to         next-thread})
          (swap! state-atom assoc :current-thread next-thread)
          (.release (scheduler-get-semaphore s next-thread))
          (recur))))
    (finally
      (tap #:scheduler{:type           :scheduler/scheduler-thread-exit
                       :current-thread (Thread/currentThread)}))))

(defn coop-lock-scheduler
  ([] (coop-lock-scheduler nil))
  ([{:keys [thread-selector tap thread-constructor]
     :or   {thread-selector rand-nth
            thread-constructor (fn [^Runnable r] (Thread. r))
            tap             tap>}}]
   (let [sch-sem    (semaphore)
         state      (atom {:managed-threads   []
                           :unmanaged-threads [[(Thread/currentThread) (semaphore)]]
                           :current-thread    (Thread/currentThread)})
         sch-thread (Thread. (fn []
                               (scheduler-loop tap sch-sem state thread-selector))
                             "coop-lock-scheduler-thread")]
     (swap! state assoc
            :scheduler-thread sch-thread
            :current-thread (Thread/currentThread))
     (.start sch-thread)
     (tap #:scheduler{:type           :scheduler/created
                      :current-thread (Thread/currentThread)})
     (reify Scheduler
       (spawn [this runnable]
         (let [sem (semaphore)
               t   (thread-constructor
                    (fn []
                      (tap #:scheduler{:type           :scheduler/thread-start
                                       :current-thread (Thread/currentThread)})
                      (let [t (Thread/currentThread)]
                        (tap #:scheduler{:type           :scheduler/thread-yield
                                         :current-thread (Thread/currentThread)
                                         :yield-to       :thread/scheduler})
                        (.acquire sem)
                        (tap #:scheduler{:type           :scheduler/thread-resume
                                         :current-thread (Thread/currentThread)}))
                      (try
                        (.run runnable)
                        (finally
                          (tap #:scheduler{:type           :scheduler/thread-exit
                                           :current-thread (Thread/currentThread)
                                           :yield-to       :thread/scheduler})
                          (swap! state (fn [s t]
                                         (-> s
                                             (update :managed-threads (partial remove #{[t sem]}))
                                             (update :unmanaged-threads (partial remove #{[t sem]}))
                                             (assoc :current-thread sch-thread)))
                                 (Thread/currentThread))
                          (.release sch-sem)))))]
           (tap #:scheduler{:type           :scheduler/thread-spawn
                            :current-thread (Thread/currentThread)
                            :new-thread     t})
           (swap! state update :managed-threads conj [t sem])
           (.start t)
           (yield this)
           t))
       (yield [this]
         (let [s              @state
               t              (Thread/currentThread)
               ^Semaphore sem (scheduler-get-semaphore s t)]
           (assert sem "Cannot yield on thread not owned by scheduler")
           (swap! state assoc :current-thread sch-thread)
           (.release sch-sem)
           (tap #:scheduler{:type           :scheduler/thread-yield
                            :current-thread (Thread/currentThread)
                            :yield-to       :thread/scheduler})
           (.acquire sem)
           (tap #:scheduler{:type           :scheduler/thread-resume
                            :current-thread (Thread/currentThread)})))))))

(defn scheduler-thread-factory
  [scheduler]
  (reify java.util.concurrent.ThreadFactory
    (^Thread newThread [this ^Runnable r]
     (spawn scheduler r))))

#_
(deftype CooperativeExecutor [scheduler]
  java.util.concurrent.ExecutorService
  (^void shutdown [this] )
  )
