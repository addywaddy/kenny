(ns kenny.test-runner
  (:require
   [cljs.test :refer-macros [run-tests]]
   [kenny.core-test]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful?
       (run-tests
        'kenny.core-test))
    0
    1))
