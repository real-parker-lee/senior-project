(defpackage senior-project/validation
  (:use :cl :alexandria :senior-project/utils)
  (:export #:check-sum-compat
           
  )
)
(in-package :senior-project/validation)

(defun check-sum-compat (row-vec col-vec)
  "Checks whether the given row and column sums are compatible by seeing if they sum to the same thing. This does not guarantee that the given vectors have a solution."
  (= (apply '+ row-vec) (apply '+ col-vec))
)
