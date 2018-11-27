(ns test.check.statem.parallel-test
  (:require [clojure.test :refer :all]
            [test.check.statem.parallel :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :refer [for-all]]))

(deftest prop-always-passes-fails-first-result-if-failiure-occurs-once-out-of-attempts
  (testing "fails on the first property generated"
    (is (false?
         (:result
          (first
           (gen/sample
            (prop-always-passes 10
              (let [state (atom 0)]
                (for-all [n gen/int]
                         (let [x (swap! state inc)]
                           (and
                            (pos? x)
                            (zero? (mod x 10)))))))))))))

  (testing "shrinks to the smallest value if prop-passes-at-least can cause a failure every 100 times"
    (is (= [1]
           (:smallest
            (:shrunk
             (tc/quick-check 1000
                             (prop-always-passes 100
                               (for-all [n gen/pos-int]
                                        (or
                                         (zero? n)
                                         (pos? (rand-int 10))))))))))))

(deftest prop-sometimes-passes-success-if-any-property-passes-out-of-attempts
  (is (:result
       (first
        (gen/sample
         (prop-sometimes-passes 10
           (let [state (atom 0)]
             (for-all [n gen/int]
                      (let [x (swap! state inc)]
                        (pos? x))))))))))

(deftest system-scheduler-runs
  (let [state (atom nil)
        t     (run-thread
               (fn []
                 (let [sch (system-scheduler)]
                   (spawn sch (fn [] (reset! state :child)))
                   (reset! state :parent))))]
    (.join t 10000)
    (.interrupt t)
    (is (#{:child :parent} @state))))

(deftest coop-scheduler-runs
  (let [state (atom nil)
        t     (run-thread
               (fn []
                 (let [sch (coop-lock-scheduler)]
                   (spawn sch (fn [] (reset! state :child)))
                   (reset! state :parent))))]
    (.join t 5000)
    (.interrupt t)
    (is (#{:child :parent} @state))))

(deftest coop-scheduler-can-be-deterministic
  (let [state (atom nil)
        t     (run-thread
               (fn []
                 (let [sch (coop-lock-scheduler {:thread-selector first})]
                   (spawn sch (fn [] (reset! state :child)))
                   (reset! state :parent))))]
    (.join t 10000)
    (.interrupt t)
    (is (= :parent @state))))
