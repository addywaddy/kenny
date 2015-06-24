(ns kenny.core-test
  (:require-macros [cljs.test :refer (is deftest testing)])
  (:require [cljs.test :refer-macros [run-tests]]))

(deftest example-passing-test
  (is (= 1 1)))
