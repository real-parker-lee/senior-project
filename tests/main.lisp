(defpackage senior-project/tests/main
  (:use :cl
        :senior-project
        :rove))
(in-package :senior-project/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :senior-project)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
